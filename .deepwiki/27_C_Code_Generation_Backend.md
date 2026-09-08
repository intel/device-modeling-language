# C Code Generation Backend

<details>
<summary>Relevant source files</summary>

The following files were used as context for generating this wiki page:

- [include/simics/dmllib.h](include/simics/dmllib.h)
- [py/dml/c_backend.py](py/dml/c_backend.py)
- [py/dml/codegen.py](py/dml/codegen.py)
- [py/dml/ctree.py](py/dml/ctree.py)

</details>



## Purpose and Scope

The C Code Generation Backend is the final stage of the DML compilation pipeline that transforms the analyzed device model into executable C code compatible with the Simics simulator. This backend takes the semantic structures built during analysis (see [Semantic Analysis](#5.3)) and generates multiple C files including device structure definitions, method implementations, attribute accessors, interface wrappers, and event callbacks.

This page covers the code generation orchestration, device structure construction, method compilation, and output file management. For details about the intermediate representation used during generation, see [Intermediate Representation (ctree)](#5.4). For runtime library functions that the generated code links against, see [Runtime Support (dmllib.h)](#5.6).

## Architecture Overview

The C code generation backend consists of three main layers that transform DML semantic structures into C code:

```mermaid
graph TB
    subgraph "Input: Semantic Structures"
        DEVICE["Device Object Tree<br/>(objects.py)"]
        METHODS["Method Definitions<br/>(objects.Method)"]
        TRAITS["Trait Metadata<br/>(traits.py)"]
        TYPES["Type Definitions<br/>(types.py)"]
    end
    
    subgraph "Layer 1: Code Generation (codegen.py)"
        EXPR_DISP["expression_dispatcher<br/>DML AST → ctree IR"]
        METHOD_INST["method_instance()<br/>Function Instance Creation"]
        CODEGEN_CALL["codegen_call()<br/>Method Call Generation"]
        FAILURE["Failure Handlers<br/>NoFailure, LogFailure, etc."]
        AFTER_INFO["AfterInfo Classes<br/>Event/Hook Metadata"]
    end
    
    subgraph "Layer 2: C IR (ctree.py)"
        STATEMENTS["Statement Classes<br/>Compound, If, While, For"]
        EXPRESSIONS["Expression Classes<br/>BinOp, Apply, Cast"]
        FACTORY["Factory Functions<br/>mkIf, mkWhile, mkCompound"]
        TOC["toc() Methods<br/>C Code Emission"]
    end
    
    subgraph "Layer 3: Backend Orchestration (c_backend.py)"
        GENERATE["generate()<br/>Main Entry Point"]
        STRUCT_GEN["print_device_substruct()<br/>Device Structure"]
        ATTR_GEN["generate_attributes()<br/>Attribute Accessors"]
        IFACE_GEN["generate_implements()<br/>Interface Wrappers"]
        EVENT_GEN["generate_simple_events()<br/>Event Callbacks"]
        HOOK_GEN["generate_after_on_hooks_artifacts()<br/>Hook Callbacks"]
    end
    
    subgraph "Output: C Files"
        C_IMPL["device.c<br/>Implementation"]
        C_HEADER["device.h<br/>Declarations"]
        C_STRUCT["device-struct.h<br/>Device Structure"]
        DEBUG["device.g<br/>Debug Info"]
    end
    
    DEVICE --> METHOD_INST
    METHODS --> METHOD_INST
    TRAITS --> AFTER_INFO
    TYPES --> STRUCT_GEN
    
    METHOD_INST --> CODEGEN_CALL
    EXPR_DISP --> EXPRESSIONS
    CODEGEN_CALL --> STATEMENTS
    FAILURE --> STATEMENTS
    AFTER_INFO --> EVENT_GEN
    
    STATEMENTS --> TOC
    EXPRESSIONS --> TOC
    FACTORY --> STATEMENTS
    FACTORY --> EXPRESSIONS
    
    TOC --> GENERATE
    STRUCT_GEN --> GENERATE
    ATTR_GEN --> GENERATE
    IFACE_GEN --> GENERATE
    EVENT_GEN --> GENERATE
    HOOK_GEN --> GENERATE
    
    GENERATE --> C_IMPL
    GENERATE --> C_HEADER
    GENERATE --> C_STRUCT
    GENERATE --> DEBUG
    
    style GENERATE fill:#f9f9f9
    style TOC fill:#f9f9f9
    style EXPR_DISP fill:#f9f9f9
```

Sources: [py/dml/c_backend.py:1-30](), [py/dml/codegen.py:1-71](), [py/dml/ctree.py:1-189]()

## Code Generation Pipeline

The code generation pipeline transforms DML semantic structures through multiple stages:

```mermaid
graph LR
    subgraph "Stage 1: Expression Translation"
        AST["DML AST Nodes<br/>ast.AST"]
        EXPR_DISPATCH["expression_dispatcher<br/>codegen.py"]
        CTREE_EXPR["ctree Expressions<br/>mkIfExpr, mkBinOp"]
    end
    
    subgraph "Stage 2: Statement Generation"
        STMTS["DML Statements<br/>if, while, call"]
        FACTORY["Factory Functions<br/>mkIf, mkWhile, mkCompound"]
        CTREE_STMT["ctree Statements<br/>If, While, Compound"]
    end
    
    subgraph "Stage 3: Method Compilation"
        METHOD_DEF["Method Definition<br/>objects.Method"]
        METHOD_FUNC["Method Function<br/>codegen_method_func()"]
        FUNC_INST["Function Instance<br/>MethodFunction"]
    end
    
    subgraph "Stage 4: C Code Emission"
        TOC_METHOD["Statement.toc()<br/>Expression.read()"]
        OUTPUT["output module<br/>out() function"]
        C_CODE["Generated C Code<br/>Strings"]
    end
    
    AST --> EXPR_DISPATCH
    EXPR_DISPATCH --> CTREE_EXPR
    STMTS --> FACTORY
    FACTORY --> CTREE_STMT
    
    METHOD_DEF --> METHOD_FUNC
    CTREE_EXPR --> METHOD_FUNC
    CTREE_STMT --> METHOD_FUNC
    METHOD_FUNC --> FUNC_INST
    
    FUNC_INST --> TOC_METHOD
    TOC_METHOD --> OUTPUT
    OUTPUT --> C_CODE
    
    style EXPR_DISPATCH fill:#f9f9f9
    style FACTORY fill:#f9f9f9
    style TOC_METHOD fill:#f9f9f9
```

Sources: [py/dml/codegen.py:977-1327](), [py/dml/ctree.py:341-420]()

### Expression Translation

The expression dispatcher converts DML AST nodes to ctree expressions:

| DML AST Node | Handler Function | ctree Output |
|--------------|-----------------|--------------|
| `conditional` | `expr_conditional()` | `IfExpr` |
| `binop` | `expr_binop()` | `BinOp` subclasses |
| `unop` | `expr_unop()` | Unary operator nodes |
| `apply` | `expr_apply()` | `Apply` |
| `variable` | `expr_variable()` | `NodeRef`, `LocalVariable` |
| `objectref` | `expr_objectref()` | `NodeRef` |
| `member` | `expr_member()` | `SubRef` |
| `cast` | `expr_cast()` | `Cast` |

Sources: [py/dml/codegen.py:979-1327]()

### Statement Factory Functions

The ctree module provides factory functions that create optimized statement IR:

- `mkCompound(site, statements)` - Creates compound statements with automatic flattening [py/dml/ctree.py:421-434]()
- `mkIf(site, cond, truebranch, falsebranch)` - Creates conditional statements with constant folding [py/dml/ctree.py:856-865]()
- `mkWhile(site, expr, stmt)` - Creates while loops [py/dml/ctree.py:890-891]()
- `mkFor(site, pres, expr, posts, stmt)` - Creates for loops [py/dml/ctree.py:956-957]()
- `mkAssignStatement(site, target, init)` - Creates assignments with type checking [py/dml/ctree.py:1126-1141]()

Sources: [py/dml/ctree.py:421-957]()

## Device Structure Generation

The device structure is a C struct containing all device state, generated by recursively traversing the object hierarchy:

```mermaid
graph TB
    subgraph "Device Structure Components"
        ROOT["Device Root<br/>conf_object_t obj"]
        STATIC["Static Variables<br/>dml.globals.static_vars"]
        IMMEDIATE["Immediate After State<br/>_immediate_after_state"]
        COMPONENTS["Object Components<br/>banks, ports, subdevices"]
    end
    
    subgraph "Object Storage Types"
        SESSION["session objects<br/>arraywrap(storage_type)"]
        SAVED["saved objects<br/>arraywrap(storage_type)"]
        HOOK["hook objects<br/>_dml_hook_t"]
        COMPOSITE["Composite Objects<br/>TStruct with members"]
    end
    
    subgraph "Generation Process"
        PRINT_SUB["print_device_substruct()<br/>Recursive traversal"]
        COMPOSITE_TYPE["composite_ctype()<br/>Build TStruct"]
        ARRAYWRAP["arraywrap()<br/>Add array dimensions"]
        PRINT_DEF["TStruct.print_struct_definition()<br/>Emit C struct"]
    end
    
    ROOT --> PRINT_SUB
    STATIC --> PRINT_SUB
    IMMEDIATE --> PRINT_SUB
    COMPONENTS --> PRINT_SUB
    
    PRINT_SUB --> SESSION
    PRINT_SUB --> SAVED
    PRINT_SUB --> HOOK
    PRINT_SUB --> COMPOSITE
    
    SESSION --> COMPOSITE_TYPE
    SAVED --> COMPOSITE_TYPE
    HOOK --> COMPOSITE_TYPE
    COMPOSITE --> COMPOSITE_TYPE
    
    COMPOSITE_TYPE --> ARRAYWRAP
    ARRAYWRAP --> PRINT_DEF
    
    style PRINT_SUB fill:#f9f9f9
    style COMPOSITE_TYPE fill:#f9f9f9
```

Sources: [py/dml/c_backend.py:116-223]()

### Structure Generation Rules

The `print_device_substruct()` function determines storage for each node type:

| Object Type | Storage Type | Notes |
|------------|--------------|-------|
| `device` | `TStruct` with all components | Root structure, includes `conf_object_t obj` |
| `session` | `arraywrap(node_storage_type())` | Session variables |
| `saved` | `arraywrap(node_storage_type())` | Checkpointed variables |
| `hook` | `arraywrap(_dml_hook_t)` | Hook storage |
| `bank` | `TStruct` with `_obj` pointer + components | Port object pointer for named banks |
| `port` | `TStruct` with `_obj` pointer + components | Port object pointer |
| `subdevice` | `TStruct` with `_obj` pointer + components | Port object pointer |
| `register`, `field` | `TStruct` or `arraywrap` | Depends on `allocate` parameter in DML 1.2 |
| `group`, `event`, `connect` | `TStruct` with components | Composite containers |

Sources: [py/dml/c_backend.py:140-223]()

## Method Compilation

Methods are compiled into C functions through a multi-stage process:

```mermaid
graph TB
    subgraph "Method Instance Creation"
        METHOD_NODE["objects.Method<br/>Method AST"]
        METHOD_INST["method_instance()<br/>Create MethodFunction"]
        FUNC_OBJ["MethodFunction<br/>inp, outp, throws, body"]
    end
    
    subgraph "Code Generation"
        CODEGEN_FUNC["codegen_method_func()<br/>Generate function body"]
        FAILURE_CTX["Failure Context<br/>CatchFailure, LogFailure"]
        EXIT_CTX["Exit Context<br/>ReturnExit, GotoExit"]
        BODY_GEN["Generate statements<br/>codegen.compound()"]
    end
    
    subgraph "Method Wrapping"
        WRAP["wrap_method()<br/>Simics API wrapper"]
        CONF_OBJ["conf_object_t *_obj<br/>First parameter"]
        INDICES["Index resolution<br/>From port object or literals"]
        STATE_CHANGE["output_dml_state_change()<br/>State notification"]
    end
    
    subgraph "Output"
        FUNC_DEF["C Function Definition<br/>return_type name(args)"]
        FUNC_BODY["Function Body<br/>Local vars + statements"]
        PROTO["Function Prototype<br/>Added to prototypes list"]
    end
    
    METHOD_NODE --> METHOD_INST
    METHOD_INST --> FUNC_OBJ
    
    FUNC_OBJ --> CODEGEN_FUNC
    CODEGEN_FUNC --> FAILURE_CTX
    CODEGEN_FUNC --> EXIT_CTX
    CODEGEN_FUNC --> BODY_GEN
    
    FUNC_OBJ --> WRAP
    WRAP --> CONF_OBJ
    WRAP --> INDICES
    WRAP --> STATE_CHANGE
    
    BODY_GEN --> FUNC_DEF
    FAILURE_CTX --> FUNC_BODY
    EXIT_CTX --> FUNC_BODY
    FUNC_DEF --> FUNC_BODY
    FUNC_DEF --> PROTO
    
    style METHOD_INST fill:#f9f9f9
    style CODEGEN_FUNC fill:#f9f9f9
    style WRAP fill:#f9f9f9
```

Sources: [py/dml/codegen.py:2200-2500](), [py/dml/c_backend.py:713-764]()

### Method Function Structure

A compiled method function has this general structure:

```
static return_type METHOD_NAME(device_t *_dev, [indices], [inputs], [outputs])
{
    // Local variable declarations
    // Method body statements
    // Return statement or exit label
}
```

Sources: [py/dml/codegen.py:2350-2450]()

### Failure Handling

Different failure handlers control exception behavior:

| Handler | Purpose | Generated Code |
|---------|---------|----------------|
| `NoFailure` | Disallow exceptions | Reports ICE if `throw` encountered |
| `LogFailure` | Log and continue | Generates `SIM_LOG_ERROR` call |
| `CatchFailure` | Re-throw | Generates `goto throw_label` |
| `ReturnFailure` | Return boolean | Returns `true` on exception |
| `IgnoreFailure` | Silent failure | Generates empty statement |

Sources: [py/dml/codegen.py:150-214]()

### Exit Handling

Exit handlers control how methods return:

| Handler | Purpose | Generated Code |
|---------|---------|----------------|
| `ReturnExit` | Direct return | `return [value];` |
| `GotoExit_dml12` | Goto exit label (DML 1.2) | `goto exit_label;` |
| `GotoExit_dml14` | Goto exit label (DML 1.4) | Assigns outputs then `goto exit_label;` |

Sources: [py/dml/codegen.py:217-268]()

## Attribute Generation

Attributes are generated as getter/setter function pairs that interface with Simics:

```mermaid
graph TB
    subgraph "Attribute Generation Process"
        ATTR_NODE["Attribute Node<br/>connect, attribute, register"]
        GEN_ATTR["generate_attributes()<br/>Recursive traversal"]
        CHECK["check_attribute()<br/>Validation"]
        REGISTER["register_attribute()<br/>Name registration"]
    end
    
    subgraph "Getter Generation"
        GETTER["generate_attr_getter()<br/>Get accessor"]
        GET_CODE["codegen_inline_byname()<br/>get_attribute method"]
        LIST_ALLOC["SIM_alloc_attr_list<br/>For array attributes"]
        SERIALIZE["Serialize to attr_value_t<br/>Recursive for arrays"]
    end
    
    subgraph "Setter Generation"
        SETTER["generate_attr_setter()<br/>Set accessor"]
        SET_CODE["codegen_inline_byname()<br/>set_attribute method"]
        DESERIALIZE["Deserialize from attr_value_t<br/>Recursive for arrays"]
        ERROR["set_error_t return<br/>Error handling"]
    end
    
    subgraph "Registration"
        ATTR_FLAGS["get_attr_flags()<br/>Configuration, persistence"]
        ATTR_TYPE["attr_type parameter<br/>Type string"]
        DOC["get_long_doc()<br/>Documentation"]
        SIM_REG["_DML_register_attribute()<br/>Simics registration"]
    end
    
    ATTR_NODE --> GEN_ATTR
    GEN_ATTR --> CHECK
    CHECK --> REGISTER
    
    GEN_ATTR --> GETTER
    GETTER --> GET_CODE
    GET_CODE --> LIST_ALLOC
    LIST_ALLOC --> SERIALIZE
    
    GEN_ATTR --> SETTER
    SETTER --> SET_CODE
    SET_CODE --> DESERIALIZE
    DESERIALIZE --> ERROR
    
    REGISTER --> ATTR_FLAGS
    REGISTER --> ATTR_TYPE
    REGISTER --> DOC
    ATTR_FLAGS --> SIM_REG
    ATTR_TYPE --> SIM_REG
    DOC --> SIM_REG
    
    style GEN_ATTR fill:#f9f9f9
    style GETTER fill:#f9f9f9
    style SETTER fill:#f9f9f9
```

Sources: [py/dml/c_backend.py:388-632]()

### Attribute Accessor Structure

Generated attribute accessors follow this pattern:

**Getter:**
```c
attr_value_t get_ATTR(conf_object_t *_obj, lang_void *_aux)
{
    device_t *_dev = (device_t *)_obj;
    attr_value_t _val0;
    // For arrays: allocate list and iterate
    // Call get_attribute method
    // Return attr_value_t
}
```

**Setter:**
```c
set_error_t set_ATTR(conf_object_t *_obj, attr_value_t *_val, lang_void *_aux)
{
    device_t *_dev = (device_t *)_obj;
    set_error_t _status = Sim_Set_Illegal_Value;
    // For arrays: iterate and extract list items
    // Call set_attribute method
    // Return status
}
```

Sources: [py/dml/c_backend.py:452-505]()

## Interface Implementation

Interface methods are wrapped to match the expected function signature:

```mermaid
graph TB
    subgraph "Interface Processing"
        IMPL_NODE["implement Node<br/>Interface declaration"]
        FIND_IFACE["Find Interface Type<br/>TNamed from c_type param"]
        IFACE_STRUCT["Interface Struct<br/>TStruct with method pointers"]
        METHODS["Method Components<br/>implement.methods"]
    end
    
    subgraph "Method Validation"
        EXPECT_SIG["Expected Signature<br/>From interface struct"]
        CHECK_TYPES["Check Input Types<br/>Match interface"]
        CHECK_OUTS["Check Output Types<br/>Match interface"]
        VALIDATE["require_fully_typed()<br/>Type validation"]
    end
    
    subgraph "Wrapper Generation"
        WRAPPER["generate_implement_method()<br/>Create wrapper"]
        WRAP_CALL["wrap_method()<br/>Simics API wrapper"]
        NAME["Wrapper Name<br/>_DML_IFACE_* or _DML_PIFACE_*"]
    end
    
    subgraph "Interface Registration"
        IFACE_BLOCK["interface_block()<br/>Struct initializer"]
        PORT_REG["Port Interface<br/>Per-port registration"]
        DEV_REG["Device Interface<br/>Global registration"]
        SIM_REG["SIM_register_interface()<br/>Simics call"]
    end
    
    IMPL_NODE --> FIND_IFACE
    FIND_IFACE --> IFACE_STRUCT
    IMPL_NODE --> METHODS
    
    IFACE_STRUCT --> EXPECT_SIG
    METHODS --> CHECK_TYPES
    EXPECT_SIG --> CHECK_TYPES
    CHECK_TYPES --> CHECK_OUTS
    CHECK_OUTS --> VALIDATE
    
    VALIDATE --> WRAPPER
    WRAPPER --> WRAP_CALL
    WRAP_CALL --> NAME
    
    NAME --> IFACE_BLOCK
    IFACE_BLOCK --> PORT_REG
    IFACE_BLOCK --> DEV_REG
    PORT_REG --> SIM_REG
    DEV_REG --> SIM_REG
    
    style WRAPPER fill:#f9f9f9
    style WRAP_CALL fill:#f9f9f9
```

Sources: [py/dml/c_backend.py:766-925]()

### Interface Block Structure

The generated interface implementation:

```c
static const interface_name_t varname = {
    .method1 = &_DML_IFACE_method1,
    .method2 = &_DML_IFACE_method2,
    // ...
};
SIM_register_interface(class, "interface_name", &varname);
```

For port interfaces, the wrapper name uses `_DML_PIFACE_` prefix and receives indices from the port object.

Sources: [py/dml/c_backend.py:831-925]()

## Event and Hook Artifacts

The backend generates callbacks for `after` statements and hooks:

```mermaid
graph TB
    subgraph "Event Info Management"
        AFTER_KEY["After Statement Key<br/>Method or TypeSequenceInfo"]
        GET_INFO["get_after_delay()<br/>Lookup or create info"]
        INFO_OBJ["AfterDelayInfo<br/>Dimensions, args_type, callback"]
        EVENT_CLASS["Simics Event Class<br/>Registration"]
    end
    
    subgraph "Callback Generation"
        GEN_EVENT["generate_simple_events()<br/>Iterate after_delay_infos"]
        CALLBACK_FUNC["Event Callback Function<br/>void callback(conf_object_t *, lang_void *)"]
        UNPACK["Unpack Event Data<br/>_simple_event_data_t"]
        CALL_METHOD["Call Target Method<br/>codegen_call()"]
    end
    
    subgraph "Serialization Functions"
        GET_VALUE["get_value() Function<br/>Serialize event data"]
        SET_VALUE["set_value() Function<br/>Deserialize event data"]
        SERIALIZE_FUNC["_serialize_simple_event_data<br/>dmllib.h"]
        DESERIALIZE_FUNC["_deserialize_simple_event_data<br/>dmllib.h"]
    end
    
    subgraph "Hook Processing"
        HOOK_INFO["AfterOnHookInfo<br/>Hook callback metadata"]
        GEN_HOOK["generate_after_on_hooks_artifacts()<br/>Generate callbacks"]
        HOOK_CALLBACK["Hook Callback Function<br/>callback(obj, indices, args, msg)"]
        ARGS_SER["Args Serializer/Deserializer<br/>For checkpointing"]
    end
    
    AFTER_KEY --> GET_INFO
    GET_INFO --> INFO_OBJ
    INFO_OBJ --> EVENT_CLASS
    
    INFO_OBJ --> GEN_EVENT
    GEN_EVENT --> CALLBACK_FUNC
    CALLBACK_FUNC --> UNPACK
    UNPACK --> CALL_METHOD
    
    INFO_OBJ --> GET_VALUE
    INFO_OBJ --> SET_VALUE
    GET_VALUE --> SERIALIZE_FUNC
    SET_VALUE --> DESERIALIZE_FUNC
    
    HOOK_INFO --> GEN_HOOK
    GEN_HOOK --> HOOK_CALLBACK
    HOOK_CALLBACK --> CALL_METHOD
    HOOK_CALLBACK --> ARGS_SER
    
    style GEN_EVENT fill:#f9f9f9
    style GEN_HOOK fill:#f9f9f9
```

Sources: [py/dml/c_backend.py:1009-1195](), [py/dml/codegen.py:461-855]()

### Event Callback Structure

Generated event callbacks:

```c
void _simple_event_N_callback(conf_object_t *_obj, lang_void *_data)
{
    device_t *_dev = (device_t *)_obj;
    _simple_event_data_t *data = (_simple_event_data_t *)_data;
    const uint32 *_indices = data ? data->indices : ...;
    const args_type *_args = data ? data->args : ...;
    
    // Call target method
    
    if (data) {
        _free_simple_event_data(*data);
        MM_FREE(data);
    }
}
```

Sources: [py/dml/c_backend.py:1031-1110]()

### Hook Callback Structure

Generated hook callbacks:

```c
void _after_on_hook_N_callback(conf_object_t *_obj, 
                               const uint32 *indices,
                               const void *_args, 
                               const void *_msg)
{
    device_t *_dev = (device_t *)_obj;
    const typeseq_struct_t *msg = _msg;
    const args_type *args = _args;
    
    // Call target method with args from msg/args
}
```

Sources: [py/dml/c_backend.py:1112-1195]()

## Output File Generation

The backend generates multiple C files with specific purposes:

```mermaid
graph TB
    subgraph "Main Generation Entry"
        GENERATE["generate()<br/>c_backend.py entry point"]
        SPLITTING["Code Splitting<br/>c_split_threshold"]
    end
    
    subgraph "Structure File (device-struct.h)"
        STRUCTFILE["generate_structfile()<br/>Struct typedef and init function"]
        TYPEDEF["typedef struct device device_t"]
        INIT_PROTO["init_function_name() prototype"]
        EXPORT_DECL["Exported method declarations"]
    end
    
    subgraph "Header File (device.h)"
        HFILE["generate_hfile()<br/>Main header"]
        GUARD["Include guards"]
        MACROS["DML_PREFIX macro"]
        TYPES["Type declarations<br/>Traits, structs, vtables"]
        DEV_STRUCT["Device structure definition<br/>print_device_substruct()"]
        LOG_GROUPS["Log group constants"]
        RESET_PROTOS["Reset function prototypes"]
    end
    
    subgraph "Implementation File (device.c)"
        CFILE["Implementation code"]
        INCLUDES["Standard includes + device.h"]
        PROTOS["generate_protofile()<br/>Function prototypes"]
        PORT_CLASSES["generate_port_classes()<br/>Port registrations"]
        ATTRIBUTES["generate_attributes()<br/>Attribute accessors"]
        IMPLEMENTS["generate_implements()<br/>Interface implementations"]
        EVENTS["generate_simple_events()<br/>Event callbacks"]
        HOOKS["generate_after_on_hooks_artifacts()<br/>Hook callbacks"]
        METHODS["Method implementations<br/>Referenced methods"]
        INIT["Initialization function"]
    end
    
    subgraph "Debug File (device.g)"
        GFILE["g_backend.py<br/>Debug metadata (optional)"]
    end
    
    GENERATE --> SPLITTING
    SPLITTING --> STRUCTFILE
    SPLITTING --> HFILE
    SPLITTING --> CFILE
    SPLITTING --> GFILE
    
    STRUCTFILE --> TYPEDEF
    STRUCTFILE --> INIT_PROTO
    STRUCTFILE --> EXPORT_DECL
    
    HFILE --> GUARD
    HFILE --> MACROS
    HFILE --> TYPES
    HFILE --> DEV_STRUCT
    HFILE --> LOG_GROUPS
    HFILE --> RESET_PROTOS
    
    CFILE --> INCLUDES
    CFILE --> PROTOS
    CFILE --> PORT_CLASSES
    CFILE --> ATTRIBUTES
    CFILE --> IMPLEMENTS
    CFILE --> EVENTS
    CFILE --> HOOKS
    CFILE --> METHODS
    CFILE --> INIT
    
    style GENERATE fill:#f9f9f9
```

Sources: [py/dml/c_backend.py:240-373]()

### Generated File Summary

| File | Purpose | Key Contents |
|------|---------|--------------|
| `device-struct.h` | Public device interface | `typedef struct device device_t;`<br/>Init function prototype<br/>Exported method declarations |
| `device.h` | Type definitions | Trait types, struct definitions<br/>Device structure with all state<br/>Vtable struct declarations |
| `device.c` | Implementation | All method implementations<br/>Attribute getters/setters<br/>Interface wrappers<br/>Event/hook callbacks<br/>Initialization function |
| `device.g` | Debug info | Source locations, method info<br/>(optional, for debugging) |

Sources: [py/dml/c_backend.py:240-373]()

### Code Splitting

When `c_split_threshold` is set, the backend splits generated code across multiple files to improve compilation parallelism:

- `splitting_point()` is called between major code sections [py/dml/output.py]()
- Prototypes use `static` linkage within single files, `extern` across split files [py/dml/c_backend.py:376-379]()
- The split preserves functional correctness by ensuring all dependencies are included

Sources: [py/dml/c_backend.py:32-33](), [py/dml/c_backend.py:376-379]()

## C Code Emission

The final stage converts ctree IR to actual C code through the `toc()` method hierarchy:

```mermaid
graph TB
    subgraph "Statement Emission"
        STMT_TOC["Statement.toc()<br/>Top-level emission"]
        STMT_TOC_STMT["Statement.toc_stmt()<br/>Single C statement"]
        STMT_TOC_INLINE["Statement.toc_inline()<br/>Inline into existing block"]
        COMPOUND["Compound.toc()<br/>Emit substatements"]
    end
    
    subgraph "Expression Reading"
        EXPR_READ["Expression.read()<br/>Generate C expression"]
        PRIORITY["Priority-based parenthesization"]
        CONSTANT["Constant folding"]
    end
    
    subgraph "Output Control"
        OUT_FUNC["out() function<br/>Write to current output"]
        LINEMARK["linemark()<br/>#line directive generation"]
        INDENT["Indentation tracking<br/>preindent, postindent"]
        OUTPUT_MGR["Output manager<br/>File switching"]
    end
    
    subgraph "Specialized Emission"
        CONTROL_FLOW["Control flow analysis<br/>Statement.control_flow()"]
        LINEMARKS_CTX["allow_linemarks()<br/>Context manager"]
        TOC_UNDER_IF["toc_under_if()<br/>Avoid if-else ambiguity"]
    end
    
    STMT_TOC --> STMT_TOC_STMT
    STMT_TOC --> STMT_TOC_INLINE
    STMT_TOC_STMT --> COMPOUND
    COMPOUND --> STMT_TOC
    
    EXPR_READ --> PRIORITY
    PRIORITY --> CONSTANT
    
    STMT_TOC_STMT --> OUT_FUNC
    EXPR_READ --> OUT_FUNC
    OUT_FUNC --> LINEMARK
    OUT_FUNC --> INDENT
    OUT_FUNC --> OUTPUT_MGR
    
    CONTROL_FLOW --> STMT_TOC
    LINEMARKS_CTX --> LINEMARK
    TOC_UNDER_IF --> STMT_TOC_STMT
    
    style STMT_TOC fill:#f9f9f9
    style EXPR_READ fill:#f9f9f9
    style OUT_FUNC fill:#f9f9f9
```

Sources: [py/dml/ctree.py:350-420](), [py/dml/output.py]()

### Statement Emission Methods

Key statement emission methods:

- `toc_stmt()` - Emits a single labeled C statement [py/dml/ctree.py:352]()
- `toc()` - Emits any number of statements/declarations for existing block [py/dml/ctree.py:355]()
- `toc_inline()` - Emits statements guaranteed in dedicated block [py/dml/ctree.py:358]()

The `Compound` statement flattens nested compounds without declarations to avoid excessive bracing [py/dml/ctree.py:402-410]().

Sources: [py/dml/ctree.py:341-420]()

### Expression Reading

Expression `read()` methods generate C code with proper precedence:

```python
def read(self):
    lh = self.lh.read()
    rh = self.rh.read()
    if self.lh.priority <= self.priority:
        lh = '(' + lh + ')'
    if self.rh.priority <= self.priority:
        rh = '(' + rh + ')'
    return lh + ' ' + self.op + ' ' + rh
```

This ensures correct parenthesization based on operator priority values.

Sources: [py/dml/ctree.py:1313-1320]()

### Control Flow Analysis

The `control_flow()` method performs limited dataflow analysis:

- Returns a `ControlFlow` object indicating possible exit paths [py/dml/ctree.py:302-339]()
- Used to prove methods never reach their end without returning [py/dml/ctree.py:361-378]()
- Handles fallthrough, exceptions (`throw`), and breaks [py/dml/ctree.py:318]()
- Conservative analysis that may fail to prove some valid cases [py/dml/ctree.py:361-378]()

Sources: [py/dml/ctree.py:302-379]()