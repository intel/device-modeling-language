# Serialization and Checkpointing

<details>
<summary>Relevant source files</summary>

The following files were used as context for generating this wiki page:

- [include/simics/dmllib.h](include/simics/dmllib.h)
- [py/dml/c_backend.py](py/dml/c_backend.py)
- [py/dml/codegen.py](py/dml/codegen.py)
- [py/dml/ctree.py](py/dml/ctree.py)
- [py/dml/ctree_test.py](py/dml/ctree_test.py)
- [py/dml/expr.py](py/dml/expr.py)
- [py/dml/objects.py](py/dml/objects.py)
- [py/dml/serialize.py](py/dml/serialize.py)
- [test/1.4/serialize/T_saved_declaration.dml](test/1.4/serialize/T_saved_declaration.dml)
- [test/1.4/serialize/T_saved_declaration.py](test/1.4/serialize/T_saved_declaration.py)

</details>



## Purpose and Scope

This document describes the DML compiler's serialization and checkpointing system, which enables device state to be saved and restored in Simics. The system automatically generates C code to convert between DML types and Simics `attr_value_t` values, allowing device state to persist across checkpoints.

For information about the attribute system that exposes serialized state, see [Attributes and Connections](#4.6). For details on the code generation backend, see [C Code Generation Backend](#5.5).

## System Overview

The serialization system provides bidirectional conversion between DML values and Simics attribute values (`attr_value_t`). When a DML variable is marked as `saved`, the compiler generates:

1. A serialization function to convert the DML value to `attr_value_t`
2. A deserialization function to convert `attr_value_t` back to the DML value
3. Attribute getter/setter methods that use these functions
4. Runtime support code for complex types

```mermaid
graph TB
    subgraph "DML Source"
        SAVED["saved int x = 5;<br/>saved MyStruct s;"]
    end
    
    subgraph "Compiler Analysis"
        MARK["mark_for_serialization()"]
        LOOKUP_SER["lookup_serialize()"]
        LOOKUP_DESER["lookup_deserialize()"]
        TYPE_SIG["type_signature()"]
    end
    
    subgraph "Generated Code"
        SER_FUNC["_serialize_MyStruct()"]
        DESER_FUNC["_deserialize_MyStruct()"]
        GETTER["get_x() / get_s()"]
        SETTER["set_x() / set_s()"]
    end
    
    subgraph "Runtime Support"
        SERIALIZE_ARRAY["_serialize_array()"]
        DESERIALIZE_ARRAY["_deserialize_array()"]
        SERIALIZE_IDENTITY["_serialize_identity()"]
        DESERIALIZE_TRAIT["_deserialize_trait_reference()"]
        DESERIALIZE_HOOK["_deserialize_hook_reference()"]
    end
    
    subgraph "Simics API"
        ATTR_VALUE["attr_value_t"]
        MAKE_INT["SIM_make_attr_int64()"]
        MAKE_LIST["SIM_alloc_attr_list()"]
        ATTR_INTEGER["SIM_attr_integer()"]
    end
    
    SAVED --> MARK
    MARK --> LOOKUP_SER
    MARK --> LOOKUP_DESER
    LOOKUP_SER --> TYPE_SIG
    LOOKUP_DESER --> TYPE_SIG
    TYPE_SIG --> SER_FUNC
    TYPE_SIG --> DESER_FUNC
    
    SER_FUNC --> GETTER
    DESER_FUNC --> SETTER
    
    SER_FUNC --> SERIALIZE_ARRAY
    SER_FUNC --> SERIALIZE_IDENTITY
    DESER_FUNC --> DESERIALIZE_ARRAY
    DESER_FUNC --> DESERIALIZE_TRAIT
    DESER_FUNC --> DESERIALIZE_HOOK
    
    GETTER --> MAKE_LIST
    GETTER --> MAKE_INT
    SETTER --> ATTR_INTEGER
    
    SERIALIZE_ARRAY --> ATTR_VALUE
    DESERIALIZE_ARRAY --> ATTR_VALUE
    MAKE_INT --> ATTR_VALUE
```

**Sources**: [py/dml/serialize.py:1-800](), [py/dml/c_backend.py:388-633](), [include/simics/dmllib.h:1-650]()

## Type Mapping and Conversion

The `map_dmltype_to_attrtype()` function maps DML types to Simics attribute type descriptor strings:

| DML Type | Attribute Type String | Notes |
|----------|---------------------|-------|
| `int`, `uint`, endian integers | `"i"` | All integer types map to 64-bit integers |
| `bool` | `"b"` | Boolean value |
| `float`, `double` | `"f"` | Floating-point value |
| `struct` | `"[...]"` | List of member types |
| `array[N]` | `"[T{N}]"` | Fixed-length list of type T |
| `uint8 array[N]` | `"[i{N}]\|d"` | Can be data or integer list |
| `trait` references | `"[s[i*]]"` | Identity: logname string + index list |
| `hook()` references | `"[s[i*]]"` | Identity: logname string + index list |

```mermaid
graph LR
    subgraph "Primitive Types"
        INT["int/uint<br/>int24/uint48_be<br/>etc."]
        BOOL["bool"]
        FLOAT["float/double"]
    end
    
    subgraph "Composite Types"
        STRUCT["struct { int i; float f; }"]
        ARRAY["int[5]"]
        BYTE_ARRAY["uint8[5]"]
    end
    
    subgraph "Reference Types"
        TRAIT["trait MyTrait"]
        HOOK["hook(int, bool)"]
    end
    
    subgraph "Attribute Types"
        ATTR_INT["i (integer)"]
        ATTR_BOOL["b (boolean)"]
        ATTR_FLOAT["f (floating)"]
        ATTR_LIST["[...] (list)"]
        ATTR_DATA["d (data)"]
        ATTR_IDENTITY["[s[i*]]<br/>(identity)"]
    end
    
    INT --> ATTR_INT
    BOOL --> ATTR_BOOL
    FLOAT --> ATTR_FLOAT
    STRUCT --> ATTR_LIST
    ARRAY --> ATTR_LIST
    BYTE_ARRAY --> ATTR_DATA
    BYTE_ARRAY -.alternative.-> ATTR_LIST
    TRAIT --> ATTR_IDENTITY
    HOOK --> ATTR_IDENTITY
```

**Sources**: [py/dml/serialize.py:362-395](), [test/1.4/serialize/T_saved_declaration.dml:1-375]()

## Serialization Function Generation

The compiler generates unique serialization/deserialization functions for each distinct type. Functions are cached using a type signature to avoid duplicates.

### Type Signature System

Each type gets a unique signature string used for caching and naming:

```mermaid
graph TB
    subgraph "Type Analysis"
        TYPE["DMLType instance"]
        SAFE_REAL["safe_realtype()"]
        TYPE_SIG["type_signature(type, is_serialize)"]
    end
    
    subgraph "Signature Components"
        BASE["Base type name"]
        MEMBERS["Member types<br/>(for structs)"]
        DIMS["Array dimensions"]
        CONST["Constness info"]
    end
    
    subgraph "Function Cache"
        SER_LIST["serialize_function_list<br/>[(signature, func_name), ...]"]
        DESER_LIST["deserialize_function_list<br/>[(signature, func_name), ...]"]
    end
    
    subgraph "Generated Functions"
        FUNC["_serialize_type_X()<br/>_deserialize_type_X()"]
    end
    
    TYPE --> SAFE_REAL
    SAFE_REAL --> TYPE_SIG
    TYPE_SIG --> BASE
    TYPE_SIG --> MEMBERS
    TYPE_SIG --> DIMS
    TYPE_SIG --> CONST
    
    BASE --> SER_LIST
    MEMBERS --> SER_LIST
    DIMS --> SER_LIST
    CONST --> SER_LIST
    
    SER_LIST --> FUNC
    DESER_LIST --> FUNC
```

**Sources**: [py/dml/serialize.py:48-75](), [py/dml/serialize.py:396-462]()

### Primitive Type Serialization

For primitive types, serialization uses Simics API calls directly:

| Type | Serialization | Deserialization |
|------|---------------|-----------------|
| Signed integers | `SIM_make_attr_int64()` | `SIM_attr_integer()` |
| Unsigned integers | `SIM_make_attr_uint64()` | `SIM_attr_integer()` with cast |
| Boolean | `SIM_make_attr_boolean()` | `SIM_attr_boolean()` |
| Float/double | `SIM_make_attr_floating()` | `SIM_attr_floating()` |
| Endian integers | Convert to int first, then serialize | Deserialize int, then convert |

**Sources**: [py/dml/serialize.py:134-166](), [py/dml/serialize.py:275-287]()

### Struct Serialization

For struct types, the compiler generates functions that recursively serialize each member:

```mermaid
graph TB
    subgraph "Source Struct"
        STRUCT_DEF["struct MyStruct {<br/>  int i;<br/>  float f;<br/>  int arr[3];<br/>}"]
    end
    
    subgraph "Generated Serializer"
        SER_START["_serialize_MyStruct(<br/>  const void *src,<br/>  void *unused)"]
        SER_ALLOC["attr_value_t result =<br/>SIM_alloc_attr_list(3)"]
        SER_MEMBER_I["SIM_attr_list_set_item(<br/>  &result, 0,<br/>  _serialize_int(&s->i))"]
        SER_MEMBER_F["SIM_attr_list_set_item(<br/>  &result, 1,<br/>  _serialize_float(&s->f))"]
        SER_MEMBER_A["SIM_attr_list_set_item(<br/>  &result, 2,<br/>  _serialize_array(&s->arr))"]
        SER_RETURN["return result"]
    end
    
    subgraph "Generated Deserializer"
        DESER_START["_deserialize_MyStruct(<br/>  attr_value_t val,<br/>  void *dest,<br/>  void *unused)"]
        DESER_CHECK["Check val is list<br/>with 3 items"]
        DESER_MEMBER_I["_deserialize_int(<br/>  item[0], &s->i)"]
        DESER_MEMBER_F["_deserialize_float(<br/>  item[1], &s->f)"]
        DESER_MEMBER_A["_deserialize_array(<br/>  item[2], &s->arr)"]
        DESER_RETURN["return Sim_Set_Ok"]
    end
    
    STRUCT_DEF --> SER_START
    STRUCT_DEF --> DESER_START
    
    SER_START --> SER_ALLOC
    SER_ALLOC --> SER_MEMBER_I
    SER_MEMBER_I --> SER_MEMBER_F
    SER_MEMBER_F --> SER_MEMBER_A
    SER_MEMBER_A --> SER_RETURN
    
    DESER_START --> DESER_CHECK
    DESER_CHECK --> DESER_MEMBER_I
    DESER_MEMBER_I --> DESER_MEMBER_F
    DESER_MEMBER_F --> DESER_MEMBER_A
    DESER_MEMBER_A --> DESER_RETURN
```

**Sources**: [py/dml/serialize.py:190-196](), [py/dml/serialize.py:306-311](), [py/dml/serialize.py:463-556]()

## Array Serialization

Arrays have special handling, particularly for byte arrays which can use the more efficient `data` attribute type.

### Array Serialization Strategy

```mermaid
graph TB
    subgraph "Array Type Classification"
        ARRAY_TYPE["Array Type Analysis"]
        IS_BYTE["Final dimension<br/>is uint8?"]
        IS_SIGNED_BYTE["Final dimension<br/>is int8?"]
        OTHER["Other types"]
    end
    
    subgraph "Serialization Strategy"
        DATA["Serialize as data<br/>(binary blob)"]
        LIST["Serialize as list<br/>(recursive)"]
        SIGNED_LIST["Serialize as signed<br/>integer list"]
    end
    
    subgraph "Deserialization Strategy"
        ACCEPT_DATA["Accept data<br/>or list"]
        ACCEPT_LIST["Accept list only"]
        ACCEPT_BOTH["Accept data<br/>or list"]
    end
    
    subgraph "Runtime Functions"
        SER_ARRAY["_serialize_array()"]
        DESER_ARRAY["_deserialize_array()"]
    end
    
    ARRAY_TYPE --> IS_BYTE
    ARRAY_TYPE --> IS_SIGNED_BYTE
    ARRAY_TYPE --> OTHER
    
    IS_BYTE --> DATA
    IS_SIGNED_BYTE --> SIGNED_LIST
    OTHER --> LIST
    
    DATA --> ACCEPT_DATA
    SIGNED_LIST --> ACCEPT_BOTH
    LIST --> ACCEPT_LIST
    
    DATA --> SER_ARRAY
    LIST --> SER_ARRAY
    SIGNED_LIST --> SER_ARRAY
    
    ACCEPT_DATA --> DESER_ARRAY
    ACCEPT_LIST --> DESER_ARRAY
    ACCEPT_BOTH --> DESER_ARRAY
```

**Sources**: [py/dml/serialize.py:166-188](), [py/dml/serialize.py:288-305](), [include/simics/dmllib.h:818-1030]()

### Multi-Dimensional Arrays

For multi-dimensional arrays, only the final dimension gets special treatment:

```
// DML declaration
saved uint8 matrix[4][3];

// Serialization produces:
// [ (data: 3 bytes), (data: 3 bytes), (data: 3 bytes), (data: 3 bytes) ]
//   ^row 0           ^row 1           ^row 2           ^row 3

// Can be deserialized from either:
// 1. Data format: [ (data), (data), (data), (data) ]
// 2. List format: [ [i, i, i], [i, i, i], [i, i, i], [i, i, i] ]
```

**Sources**: [test/1.4/serialize/T_saved_declaration.dml:50-75](), [test/1.4/serialize/T_saved_declaration.py:62-74]()

## Identity and Reference Serialization

DML supports serializing references to objects, traits, and hooks. These are serialized as "identities" consisting of a logname string and index array.

### Identity Structure

The `_identity_t` structure represents an object's identity:

```c
typedef struct {
    uint32 id;              // Unique object ID
    uint32 encoded_index;   // Flattened array index
} _identity_t;
```

The `_id_info_t` structure provides metadata for resolving identities:

```c
typedef struct {
    const char *logname;    // Format string with %u for indices
    const uint32 *dimsizes; // Array dimensions
    uint32 dimensions;      // Number of dimensions
    uint32 id;              // Object ID
} _id_info_t;
```

**Sources**: [include/simics/dmllib.h:209-219](), [py/dml/serialize.py:197-214]()

### Object Identity Serialization

```mermaid
graph TB
    subgraph "DML Code"
        OBJ_CAST["saved object ref =<br/>cast(port.bank[2], object)"]
    end
    
    subgraph "Runtime Identity"
        IDENTITY["_identity_t {<br/>  id: 15,<br/>  encoded_index: 2<br/>}"]
        ID_INFO["_id_info_t {<br/>  logname: 'port.bank[%u]',<br/>  dimsizes: [4],<br/>  dimensions: 1,<br/>  id: 15<br/>}"]
    end
    
    subgraph "Serialized Form"
        ATTR_FORM["['port.bank[%u]', [2]]"]
        LOGNAME_STR["'port.bank[%u]'<br/>(string)"]
        INDEX_LIST["[2]<br/>(integer list)"]
    end
    
    subgraph "Serialization Process"
        SER_IDENT["_serialize_identity(<br/>  _id_infos,<br/>  identity)"]
        LOOKUP_INFO["Look up id in<br/>_id_infos array"]
        FORMAT_NAME["Format logname<br/>with indices"]
        BUILD_ATTR["Build attr_value_t<br/>list"]
    end
    
    subgraph "Deserialization Process"
        DESER_IDENT["_deserialize_trait_reference()"]
        PARSE_NAME["Parse logname string"]
        LOOKUP_VTABLE["Look up in<br/>_id_info_ht"]
        VALIDATE_IND["Validate indices<br/>against dimsizes"]
        COMPUTE_ENC["Compute<br/>encoded_index"]
    end
    
    OBJ_CAST --> IDENTITY
    IDENTITY --> ID_INFO
    
    IDENTITY --> SER_IDENT
    ID_INFO --> SER_IDENT
    SER_IDENT --> LOOKUP_INFO
    LOOKUP_INFO --> FORMAT_NAME
    FORMAT_NAME --> BUILD_ATTR
    BUILD_ATTR --> ATTR_FORM
    
    ATTR_FORM --> LOGNAME_STR
    ATTR_FORM --> INDEX_LIST
    
    ATTR_FORM --> DESER_IDENT
    DESER_IDENT --> PARSE_NAME
    PARSE_NAME --> LOOKUP_VTABLE
    LOOKUP_VTABLE --> VALIDATE_IND
    VALIDATE_IND --> COMPUTE_ENC
    COMPUTE_ENC --> IDENTITY
```

**Sources**: [include/simics/dmllib.h:1031-1137](), [py/dml/serialize.py:312-336]()

### Trait Reference Serialization

Trait references serialize the object identity plus validate that the object implements the trait:

```mermaid
graph TB
    subgraph "Trait System"
        TRAIT_DEF["trait MyTrait {<br/>  method m();<br/>}"]
        IMPL["group g is MyTrait"]
        REF["saved MyTrait ref =<br/>cast(g, MyTrait)"]
    end
    
    subgraph "Runtime Representation"
        TRAITREF["_traitref_t {<br/>  trait: vtable_ptr,<br/>  id: {obj_id, index}<br/>}"]
        VTABLE["Vtable for g's<br/>MyTrait implementation"]
        VTABLE_HT["_MyTrait_vtable_ht<br/>(id -> vtable mapping)"]
    end
    
    subgraph "Serialization"
        SER_TRAIT["Extract id from traitref"]
        SER_AS_ID["Serialize as identity<br/>['g', []]"]
    end
    
    subgraph "Deserialization"
        DESER_TRAIT["_deserialize_trait_reference(<br/>  _id_info_ht,<br/>  _MyTrait_vtable_ht,<br/>  'MyTrait',<br/>  val, dest)"]
        RESOLVE_ID["Resolve identity to<br/>object and index"]
        LOOKUP_VT["Look up vtable in<br/>_MyTrait_vtable_ht"]
        CHECK_IMPL["Verify object<br/>implements trait"]
        BUILD_REF["Build _traitref_t<br/>with vtable + id"]
    end
    
    TRAIT_DEF --> IMPL
    IMPL --> VTABLE
    IMPL --> REF
    REF --> TRAITREF
    TRAITREF --> VTABLE
    VTABLE --> VTABLE_HT
    
    TRAITREF --> SER_TRAIT
    SER_TRAIT --> SER_AS_ID
    
    SER_AS_ID --> DESER_TRAIT
    DESER_TRAIT --> RESOLVE_ID
    RESOLVE_ID --> LOOKUP_VT
    LOOKUP_VT --> CHECK_IMPL
    CHECK_IMPL --> BUILD_REF
    BUILD_REF --> TRAITREF
```

**Sources**: [py/dml/serialize.py:312-336](), [include/simics/dmllib.h:1138-1228](), [test/1.4/serialize/T_saved_declaration.dml:177-187]()

### Hook Reference Serialization

Hook references are serialized similarly to identities but with type validation:

```mermaid
graph TB
    subgraph "Hook System"
        HOOK_DECL["hook(int, bool) h[3];"]
        HOOK_REF["saved hook(int, bool) ref =<br/>g.h[2]"]
    end
    
    subgraph "Runtime Representation"
        HOOKREF["_hookref_t {<br/>  id: hook_obj_id,<br/>  encoded_index: 2<br/>}"]
        HOOK_AUX["_hook_aux_info_t {<br/>  msg_types_uniq: 17<br/>}"]
    end
    
    subgraph "Type Info"
        TYPE_SEQ["TypeSequenceInfo {<br/>  types: [TInt, TBool],<br/>  uniq: 17,<br/>  struct: _typeseq_17_t<br/>}"]
    end
    
    subgraph "Serialization"
        SER_HOOK["Serialize as identity<br/>['g.h[%u]', [2]]"]
    end
    
    subgraph "Deserialization"
        DESER_HOOK["_deserialize_hook_reference(<br/>  _hook_id_info_ht,<br/>  _hook_aux_infos,<br/>  expected_type_uniq,<br/>  val, dest)"]
        RESOLVE_H["Resolve hook identity"]
        CHECK_TYPE["Verify hook type<br/>matches expected"]
        BUILD_HREF["Build _hookref_t"]
    end
    
    HOOK_DECL --> HOOK_REF
    HOOK_REF --> HOOKREF
    HOOKREF --> HOOK_AUX
    HOOK_AUX --> TYPE_SEQ
    
    HOOKREF --> SER_HOOK
    SER_HOOK --> DESER_HOOK
    DESER_HOOK --> RESOLVE_H
    RESOLVE_H --> CHECK_TYPE
    CHECK_TYPE --> BUILD_HREF
    BUILD_HREF --> HOOKREF
```

**Sources**: [py/dml/serialize.py:337-357](), [include/simics/dmllib.h:1229-1290](), [test/1.4/serialize/T_saved_declaration.dml:189-214]()

## Attribute Integration

The serialization system integrates with the DML attribute system to provide checkpoint-able attributes.

### Attribute Getter/Setter Generation

For each `saved` variable or object, the compiler generates attribute getter and setter methods:

```mermaid
graph TB
    subgraph "DML Declaration"
        SAVED_VAR["saved int x = 5;"]
        SAVED_ARRAY["port p[4] {<br/>  saved int v;<br/>}"]
    end
    
    subgraph "Generated Getters"
        GET_X["attr_value_t get_x(<br/>  conf_object_t *obj,<br/>  lang_void *aux)"]
        GET_X_CODE["return _serialize_int(<br/>  &_dev->x);"]
        
        GET_P_V["attr_value_t get_port_p_v(<br/>  conf_object_t *obj,<br/>  lang_void *aux)"]
        GET_P_V_LOOP["for each index i:<br/>  serialize _dev->p[i].v<br/>  into list"]
    end
    
    subgraph "Generated Setters"
        SET_X["set_error_t set_x(<br/>  conf_object_t *obj,<br/>  attr_value_t *val,<br/>  lang_void *aux)"]
        SET_X_CODE["return _deserialize_int(<br/>  *val, &_dev->x);"]
        
        SET_P_V["set_error_t set_port_p_v(<br/>  conf_object_t *obj,<br/>  attr_value_t *val,<br/>  lang_void *aux)"]
        SET_P_V_LOOP["for each index i:<br/>  deserialize val[i]<br/>  into _dev->p[i].v"]
    end
    
    subgraph "Attribute Registration"
        REG["_DML_register_attribute(<br/>  class, 'x',<br/>  get_x, NULL,<br/>  set_x, NULL,<br/>  flags, 'i', doc)"]
        REG_ARRAY["_register_port_attr(<br/>  port_class, 'p_v',<br/>  get_port_p_v,<br/>  set_port_p_v,<br/>  flags, '[i{4}]', doc)"]
    end
    
    SAVED_VAR --> GET_X
    SAVED_VAR --> SET_X
    GET_X --> GET_X_CODE
    SET_X --> SET_X_CODE
    GET_X_CODE --> REG
    SET_X_CODE --> REG
    
    SAVED_ARRAY --> GET_P_V
    SAVED_ARRAY --> SET_P_V
    GET_P_V --> GET_P_V_LOOP
    SET_P_V --> SET_P_V_LOOP
    GET_P_V_LOOP --> REG_ARRAY
    SET_P_V_LOOP --> REG_ARRAY
```

**Sources**: [py/dml/c_backend.py:388-450](), [py/dml/c_backend.py:452-505](), [py/dml/c_backend.py:629-633]()

### Device Structure Layout

Saved variables are stored in the device structure, with careful layout to handle arrays and nested objects:

```
Device Structure:
├── conf_object_t obj
├── Static variables
├── _immediate_after_state pointer
└── Composite objects (banks, ports, etc.)
    ├── _obj pointer (if named)
    ├── Session variables (runtime only)
    ├── Saved variables (checkpointed)
    └── Hook structures
```

**Sources**: [py/dml/c_backend.py:116-224](), [py/dml/c_backend.py:355-359]()

## Runtime Support Functions

The `dmllib.h` header provides runtime support functions used by generated code:

### Array Serialization Runtime

| Function | Purpose |
|----------|---------|
| `_serialize_array()` | Recursively serialize multi-dimensional arrays |
| `_deserialize_array()` | Recursively deserialize multi-dimensional arrays |
| Element serializer parameter | Function pointer for element-wise serialization |
| Element deserializer parameter | Function pointer for element-wise deserialization |

### Identity Resolution Runtime

| Function | Purpose |
|----------|---------|
| `_serialize_identity()` | Convert `_identity_t` to `["name[%u]", [indices]]` |
| `_deserialize_trait_reference()` | Parse identity and look up trait vtable |
| `_deserialize_hook_reference()` | Parse identity and validate hook type |
| `_id_info_ht` | Hash table mapping lognames to `_id_info_t` |
| `_MyTrait_vtable_ht` | Hash table mapping object IDs to trait vtables |
| `_hook_aux_infos` | Array of hook auxiliary info (type signatures) |

**Sources**: [include/simics/dmllib.h:818-1030](), [include/simics/dmllib.h:1031-1290]()

### Error Handling

Deserialization functions return `set_error_t` to indicate success or failure:

| Error Code | Meaning |
|------------|---------|
| `Sim_Set_Ok` | Successfully deserialized |
| `Sim_Set_Illegal_Type` | Attribute value has wrong type |
| `Sim_Set_Illegal_Value` | Value is out of range or invalid |

Serialization is expected to always succeed if the type is correctly marked as serializable.

**Sources**: [py/dml/serialize.py:220-360](), [py/dml/c_backend.py:388-450]()

## Checkpointing Flow

The complete flow from DML source to checkpoint and back:

```mermaid
sequenceDiagram
    participant DML as DML Source
    participant Compiler as DMLC
    participant Device as Device C Code
    participant Simics as Simics Core
    participant Checkpoint as Checkpoint File
    
    Note over DML: saved int x = 5;
    DML->>Compiler: Compile device
    Compiler->>Compiler: mark_for_serialization(int)
    Compiler->>Compiler: lookup_serialize(TInt)
    Compiler->>Compiler: lookup_deserialize(TInt)
    Compiler->>Device: Generate get_x(), set_x()
    Compiler->>Device: Register attribute 'x'
    
    Note over Simics: Checkpoint save
    Simics->>Device: Call get_x()
    Device->>Device: _serialize_int(&_dev->x)
    Device->>Simics: Return attr_value_t (integer)
    Simics->>Checkpoint: Write value to file
    
    Note over Simics: Device reset/restart
    
    Note over Simics: Checkpoint restore
    Checkpoint->>Simics: Read value from file
    Simics->>Device: Call set_x(val)
    Device->>Device: _deserialize_int(val, &_dev->x)
    Device->>Device: Validate type
    Device->>Device: Store value
    Device->>Simics: Return Sim_Set_Ok
```

**Sources**: [py/dml/serialize.py:1-800](), [py/dml/c_backend.py:388-677](), [test/1.4/serialize/T_saved_declaration.py:1-153]()