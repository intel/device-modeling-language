# Advanced Topics

<details>
<summary>Relevant source files</summary>

The following files were used as context for generating this wiki page:

- [RELEASENOTES-1.2.md](RELEASENOTES-1.2.md)
- [RELEASENOTES-1.4.md](RELEASENOTES-1.4.md)
- [RELEASENOTES.md](RELEASENOTES.md)
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



This page covers advanced DML features that involve sophisticated runtime behavior and code generation. These topics require understanding of DML's compilation pipeline and runtime support infrastructure.

The advanced features documented here include:
- **Serialization and Checkpointing** ([6.1](#6.1)): Converting DML state to/from `attr_value_t` for checkpointing
- **Event and Hook System** ([6.2](#6.2)): Delayed execution mechanisms including `after` statements and hooks
- **Trait System Implementation** ([6.3](#6.3)): Runtime polymorphism through vtables and identity tracking
- **Breaking Changes and Compatibility** ([6.4](#6.4)): Version management across DML 1.2/1.4 and Simics API versions

For basic language concepts, see [DML Language Reference](#3). For standard library usage, see [Standard Library](#4). For compiler internals not related to these runtime features, see [Compiler Architecture](#5).

## Overview of Advanced Runtime Features

The DML compiler generates C code that interfaces with the `dmllib.h` runtime library to support advanced features. These features share common infrastructure for object identity tracking, type serialization, and delayed execution.

### Runtime Support Architecture

```mermaid
graph TB
    subgraph "DML Source Constructs"
        SAVED["saved variables"]
        AFTER["after statements"]
        HOOKS["hook declarations"]
        TRAITS["trait references"]
    end
    
    subgraph "Compiler Analysis (codegen.py)"
        SERIALIZE["serialize.py<br/>lookup_serialize()<br/>lookup_deserialize()"]
        AFTER_INFO["AfterInfo hierarchy<br/>AfterDelayInfo<br/>AfterOnHookInfo<br/>ImmediateAfterInfo"]
        TRAIT_INFO["TraitMethod<br/>ObjTraits<br/>vtable generation"]
    end
    
    subgraph "Generated C Artifacts"
        SERIALIZERS["serialize/deserialize<br/>functions"]
        EVENTS["event callbacks<br/>_simple_event_*_callback"]
        VTABLES["trait vtables<br/>struct _traitname"]
        IDENTITY["identity arrays<br/>_id_infos[]<br/>_hook_id_infos[]"]
    end
    
    subgraph "Runtime Library (dmllib.h)"
        TYPES["_identity_t<br/>_traitref_t<br/>_hookref_t"]
        EVENT_RT["_simple_event_data_t<br/>_DML_post_immediate_after"]
        HOOK_RT["_dml_hook_t<br/>_DML_attach_callback_to_hook<br/>_DML_send_hook"]
        TRAIT_RT["VTABLE_PARAM<br/>CALL_TRAIT_METHOD"]
    end
    
    SAVED --> SERIALIZE
    AFTER --> AFTER_INFO
    HOOKS --> AFTER_INFO
    TRAITS --> TRAIT_INFO
    
    SERIALIZE --> SERIALIZERS
    AFTER_INFO --> EVENTS
    TRAIT_INFO --> VTABLES
    TRAIT_INFO --> IDENTITY
    AFTER_INFO --> IDENTITY
    
    SERIALIZERS --> TYPES
    EVENTS --> EVENT_RT
    EVENTS --> HOOK_RT
    VTABLES --> TRAIT_RT
    IDENTITY --> TYPES
```

Sources: [py/dml/serialize.py:1-856](), [py/dml/codegen.py:421-667](), [py/dml/c_backend.py:1-3143](), [include/simics/dmllib.h:1-1370]()

### Identity System Foundation

All advanced features rely on the identity system to track objects at runtime. Each object that can be referenced (via traits, hooks, or serialization) is assigned a unique ID stored in `_id_info_t` structures.

```mermaid
graph LR
    subgraph "Object Hierarchy"
        DEVICE["Device<br/>uniq=1"]
        BANK["bank b[i]<br/>uniq=2-5"]
        REG["register r<br/>uniq=6-9"]
    end
    
    subgraph "Identity Tables"
        ID_INFOS["_id_infos[]<br/>[0]: invalid<br/>[1]: device<br/>[2-5]: bank b<br/>[6-9]: register r"]
        HOOK_INFOS["_hook_id_infos[]<br/>parallel structure<br/>for hook objects"]
    end
    
    subgraph "Identity References"
        IDENTITY_T["_identity_t<br/>{id: uint32<br/>encoded_index: uint32}"]
        TRAITREF["_traitref_t<br/>{trait: void*<br/>id: _identity_t}"]
        HOOKREF["_hookref_t<br/>typedef _identity_t"]
    end
    
    DEVICE --> ID_INFOS
    BANK --> ID_INFOS
    REG --> ID_INFOS
    
    ID_INFOS --> IDENTITY_T
    IDENTITY_T --> TRAITREF
    IDENTITY_T --> HOOKREF
    HOOK_INFOS --> HOOKREF
```

Sources: [include/simics/dmllib.h:209-226](), [py/dml/objects.py:194-239](), [py/dml/c_backend.py:116-223]()

The `uniq` property on `CompositeObject` instances ([py/dml/objects.py:238]()) is set during code generation and corresponds to the index into `_id_infos` plus one (zero is reserved for invalid references).

## Serialization System

The serialization system converts DML values to and from `attr_value_t` for checkpointing. The compiler generates specialized serialization functions for each unique type encountered in `saved` variables or `after` statement arguments.

### Type-Specific Serialization

```mermaid
graph TB
    subgraph "Type Categories"
        PRIMITIVES["Primitives<br/>int, bool, float"]
        ARRAYS["Arrays<br/>T[N]"]
        STRUCTS["Structs<br/>struct {...}"]
        ENDIAN["Endian Integers<br/>uint32_be_t"]
        LAYOUTS["Layouts<br/>layout {...}"]
        TRAITS["Template Types<br/>trait references"]
        HOOKS["Hook References<br/>hook(...)"]
    end
    
    subgraph "Serialization Functions (serialize.py)"
        LOOKUP_SER["lookup_serialize(type)<br/>returns function name"]
        LOOKUP_DESER["lookup_deserialize(type)<br/>returns function name"]
        TYPE_SIG["type_signature(type)<br/>unique string key"]
        GEN_SER["generate_serialize(type)<br/>creates serializer"]
        GEN_DESER["generate_deserialize(type)<br/>creates deserializer"]
    end
    
    subgraph "Generated Functions"
        PRIM_SER["SIM_make_attr_int64<br/>SIM_make_attr_boolean"]
        ARRAY_SER["_serialize_array<br/>recursive element serialization"]
        STRUCT_SER["_serialize_typename<br/>member-by-member conversion"]
        IDENTITY_SER["_serialize_identity<br/>converts _identity_t to attr"]
    end
    
    PRIMITIVES --> LOOKUP_SER
    ARRAYS --> LOOKUP_SER
    STRUCTS --> LOOKUP_SER
    TRAITS --> LOOKUP_SER
    HOOKS --> LOOKUP_SER
    
    LOOKUP_SER --> TYPE_SIG
    TYPE_SIG --> GEN_SER
    GEN_SER --> PRIM_SER
    GEN_SER --> ARRAY_SER
    GEN_SER --> STRUCT_SER
    GEN_SER --> IDENTITY_SER
```

Sources: [py/dml/serialize.py:36-75](), [py/dml/serialize.py:133-218](), [py/dml/serialize.py:429-581]()

### Saved Variable Attributes

For each `saved` variable, DMLC generates getter and setter attributes that invoke the appropriate serialization functions:

```mermaid
graph LR
    subgraph "DML Declaration"
        SAVED_DECL["saved int x = 5;"]
    end
    
    subgraph "Generated Device Struct"
        STRUCT_MEMBER["struct device {<br/>  ...<br/>  int x;<br/>  ...<br/>}"]
    end
    
    subgraph "Generated Attributes"
        GETTER["get_x(conf_object_t *obj)<br/>→ SIM_make_attr_int64(_dev->x)"]
        SETTER["set_x(conf_object_t *obj, attr_value_t *val)<br/>→ _dev->x = SIM_attr_integer(*val)"]
    end
    
    subgraph "Simics Registration"
        REGISTER["_DML_register_attribute(<br/>  class, 'x',<br/>  get_x, set_x, ...)"]
    end
    
    SAVED_DECL --> STRUCT_MEMBER
    STRUCT_MEMBER --> GETTER
    STRUCT_MEMBER --> SETTER
    GETTER --> REGISTER
    SETTER --> REGISTER
```

Sources: [py/dml/c_backend.py:387-450](), [py/dml/c_backend.py:452-505](), [test/1.4/serialize/T_saved_declaration.dml:16-25]()

### Special Cases

**Array Serialization**: The final dimension of `uint8` arrays is serialized as `data` attribute values rather than integer lists for efficiency ([py/dml/serialize.py:166-187]()).

**Layout Serialization**: Layout types are serialized by converting to/from byte arrays, handling endianness and member alignment ([py/dml/serialize.py:507-546]()).

**Identity Serialization**: Trait references and hook references serialize their `_identity_t` component using `_serialize_identity()` which stores the object's logname format string and indices ([py/dml/serialize.py:196-213](), [include/simics/dmllib.h:846-916]()).

## Event and Hook System

DML provides three mechanisms for delayed execution: time-based `after`, hook-based `after`, and immediate `after`. Each has different scheduling semantics and generated code patterns.

### After Statement Variants

```mermaid
graph TB
    subgraph "After Statement Types"
        DELAY["after (delay) s:<br/>method()"]
        ON_HOOK["after hook:<br/>method()"]
        IMMEDIATE["after:<br/>method()"]
    end
    
    subgraph "Scheduling Mechanisms"
        SIMICS_EVENT["SIM_event_post_time/<br/>SIM_event_post_cycle<br/>(Simics event queue)"]
        HOOK_ATTACH["_DML_attach_callback_to_hook<br/>(hook callback list)"]
        IMM_QUEUE["_DML_post_immediate_after<br/>(immediate queue)"]
    end
    
    subgraph "Callback Invocation"
        EVENT_CB["_simple_event_N_callback<br/>(event fires)"]
        HOOK_CB["_after_on_hook_N_callback<br/>(hook.send_now())"]
        IMM_CB["_immediate_after_N_callback<br/>(simulation step boundary)"]
    end
    
    DELAY --> SIMICS_EVENT
    ON_HOOK --> HOOK_ATTACH
    IMMEDIATE --> IMM_QUEUE
    
    SIMICS_EVENT --> EVENT_CB
    HOOK_ATTACH --> HOOK_CB
    IMM_QUEUE --> IMM_CB
```

Sources: [py/dml/ctree.py:697-818](), [py/dml/codegen.py:461-667](), [include/simics/dmllib.h:1047-1177]()

### After Delay Implementation

Time-based `after` statements compile to Simics event postings with serialized arguments:

```mermaid
graph TB
    subgraph "Compilation (codegen.py)"
        DELAY_STMT["after (t) s: m(args)"]
        GET_INFO["get_after_delay(method)<br/>→ AfterDelayInfo"]
        INFO_KEY["key = method identity"]
    end
    
    subgraph "Generated Callback"
        CALLBACK["static void<br/>_simple_event_N_callback(<br/>  conf_object_t *obj,<br/>  lang_void *data)"]
        UNPACK["_simple_event_data_t *d<br/>= (_simple_event_data_t*)data"]
        DESERIALIZE["deserialize arguments<br/>from d->args"]
        CALL["call method with<br/>deserialized args"]
    end
    
    subgraph "Event Posting (After.toc)"
        ALLOC["allocate _simple_event_data_t"]
        SERIALIZE["serialize method args<br/>into data->args"]
        POST["SIM_event_post_time(<br/>  clock, evclass,<br/>  obj, delay, data)"]
    end
    
    DELAY_STMT --> GET_INFO
    GET_INFO --> INFO_KEY
    INFO_KEY --> CALLBACK
    DELAY_STMT --> ALLOC
    ALLOC --> SERIALIZE
    SERIALIZE --> POST
    POST --> CALLBACK
```

Sources: [py/dml/codegen.py:595-626](), [py/dml/ctree.py:697-756](), [include/simics/dmllib.h:1047-1084]()

The `AfterDelayInfo` class ([py/dml/codegen.py:495-529]()) stores metadata about the callback including argument types and dimensions. Each unique method+arguments combination gets its own event class and callback function.

### Hook-Based After Implementation

Hook-based `after` statements attach callbacks to hook objects that execute when `send_now()` is called:

```mermaid
graph TB
    subgraph "Hook Declaration"
        HOOK_DECL["hook(int, bool) h;"]
        HOOK_STRUCT["_dml_hook_t _dev->h<br/>{callbacks: list}"]
    end
    
    subgraph "After on Hook"
        AFTER_HOOK["after h: m(x, msg)"]
        ATTACH["_DML_attach_callback_to_hook(<br/>  &_dev->h,<br/>  &_after_on_hook_infos[N],<br/>  indices, args, domains)"]
    end
    
    subgraph "Hook Send"
        SEND["h.send_now(42, true)"]
        DISPATCH["_DML_send_hook(<br/>  obj, queue_stack,<br/>  hook, msg_data)"]
        ITERATE["iterate callback list"]
        INVOKE["_after_on_hook_N_callback(<br/>  indices, args, msg)"]
    end
    
    HOOK_DECL --> HOOK_STRUCT
    AFTER_HOOK --> ATTACH
    ATTACH --> HOOK_STRUCT
    SEND --> DISPATCH
    DISPATCH --> ITERATE
    ITERATE --> INVOKE
    INVOKE --> AFTER_HOOK
```

Sources: [py/dml/ctree.py:764-789](), [py/dml/codegen.py:531-582](), [include/simics/dmllib.h:1086-1133]()

The `AfterOnHookInfo` hierarchy ([py/dml/codegen.py:531-582]()) handles two cases:
1. `AfterOnHookIntoMethodInfo`: Target is a method call
2. Hook messages can be passed to the callback via `param_to_msg_comp` mapping

Hook callbacks support serialization for checkpointing, requiring complex bookkeeping in `_after_on_hook_state_t` structures ([include/simics/dmllib.h:1107-1133]()).

### Immediate After Implementation

Immediate `after` defers execution until the current device entry completes:

```mermaid
graph LR
    subgraph "Immediate After"
        IMM_STMT["after: method()"]
        POST["_DML_post_immediate_after(<br/>  &_dev->obj,<br/>  _dev->_immediate_after_state,<br/>  callback, indices, args, ...)"]
    end
    
    subgraph "Immediate Queue"
        STATE["_dml_immediate_after_state_t<br/>_dev->_immediate_after_state"]
        QUEUE["pending callback list"]
    end
    
    subgraph "Execution"
        BOUNDARY["device method exits"]
        NOTIFY["_immediate_after_notify_state_change"]
        DRAIN["drain queue:<br/>invoke all callbacks"]
    end
    
    IMM_STMT --> POST
    POST --> STATE
    STATE --> QUEUE
    BOUNDARY --> NOTIFY
    NOTIFY --> DRAIN
    DRAIN --> IMM_STMT
```

Sources: [py/dml/ctree.py:792-817](), [py/dml/codegen.py:583-593](), [include/simics/dmllib.h:1135-1177]()

Immediate after uses a simpler queue structure since callbacks execute within the same simulation cycle and don't require checkpointing.

## Trait System Runtime

Traits provide polymorphism in DML through vtable-based dispatch. Each trait reference is a `_traitref_t` containing a vtable pointer and object identity.

### Vtable Structure

```mermaid
graph TB
    subgraph "Trait Definition"
        TRAIT_DEF["trait t {<br/>  param p : int;<br/>  method m(int x) -> (int);<br/>}"]
    end
    
    subgraph "Vtable Type (struct _t)"
        VTABLE["struct _t {<br/>  typeof(p) *p;<br/>  bool (*m)(...);<br/>  _each_in_param_t seq_param;<br/>}"]
    end
    
    subgraph "Vtable Instance"
        INSTANCE["static struct _t _vtable_g_t = {<br/>  .p = &param_value,<br/>  .m = &_method_g_m,<br/>  .seq_param = {...}<br/>}"]
    end
    
    subgraph "Trait Reference"
        TRAITREF["_traitref_t ref = {<br/>  .trait = &_vtable_g_t,<br/>  .id = {uniq, encoded_index}<br/>}"]
    end
    
    TRAIT_DEF --> VTABLE
    VTABLE --> INSTANCE
    INSTANCE --> TRAITREF
```

Sources: [py/dml/traits.py:1-951](), [py/dml/c_backend.py:2191-2288](), [include/simics/dmllib.h:223-324]()

### Parameter Access

Parameters in vtables are stored with a tagged pointer scheme to handle both constant and index-varying values:

```mermaid
graph LR
    subgraph "Parameter Storage"
        CONST["Constant param:<br/>pointer[0] = 0<br/>points to single value"]
        VARYING["Varying param:<br/>pointer[0] = 1<br/>points to array"]
    end
    
    subgraph "VTABLE_PARAM Macro"
        EXTRACT["__member = vtable->param"]
        CHECK["if (__member & 1)"]
        INDEX["use encoded_index<br/>else use 0"]
        DEREF["dereference adjusted pointer"]
    end
    
    CONST --> EXTRACT
    VARYING --> EXTRACT
    EXTRACT --> CHECK
    CHECK --> INDEX
    INDEX --> DEREF
```

Sources: [include/simics/dmllib.h:312-324](), [py/dml/c_backend.py:2219-2288]()

The least significant bit serves as a tag because parameter values are always aligned to at least 2 bytes.

### Method Dispatch

Calling a trait method requires dereferencing the vtable function pointer:

```mermaid
graph TB
    subgraph "DML Call"
        CALL["trait_ref.method(args)"]
    end
    
    subgraph "CALL_TRAIT_METHOD Macro"
        EXTRACT_VTABLE["vtable = (_t*)traitref.trait"]
        EXTRACT_FN["fn = vtable->method"]
        INVOKE["fn(_dev, traitref, args)"]
    end
    
    subgraph "Method Implementation"
        IMPL["bool _method_g_m(<br/>  device *_dev,<br/>  _traitref_t _t,<br/>  int x, int *ret)"]
    end
    
    CALL --> EXTRACT_VTABLE
    EXTRACT_VTABLE --> EXTRACT_FN
    EXTRACT_FN --> INVOKE
    INVOKE --> IMPL
```

Sources: [include/simics/dmllib.h:298-310](), [py/dml/codegen.py:1887-2056]()

Independent trait methods (those marked `independent`) don't receive `_dev` parameter and can operate without device context ([py/dml/codegen.py:330-385]()).

### Trait Casting and Ancestry

Traits support inheritance, requiring upcasting and downcasting between related trait types:

```mermaid
graph LR
    subgraph "Trait Hierarchy"
        BASE["trait base"]
        CHILD["trait child extends base"]
    end
    
    subgraph "Vtable Nesting"
        CHILD_VTABLE["struct _child {<br/>  struct _base base;<br/>  ...<br/>}"]
    end
    
    subgraph "Casting Macros"
        UPCAST["UPCAST(child_ref, child, base)<br/>adjust pointer by offsetof"]
        DOWNCAST["DOWNCAST(base_ref, child, base)<br/>subtract offset"]
    end
    
    BASE --> CHILD
    CHILD --> CHILD_VTABLE
    CHILD_VTABLE --> UPCAST
    CHILD_VTABLE --> DOWNCAST
```

Sources: [include/simics/dmllib.h:274-296](), [py/dml/traits.py:573-677]()

The vtable for a derived trait embeds the base trait's vtable as its first member, enabling pointer arithmetic for casting.

## Breaking Changes and Compatibility

DML manages compatibility across language versions (1.2 vs 1.4) and Simics API versions (4.8, 5, 6, 7) through feature flags and compatibility layers.

### Compatibility Architecture

```mermaid
graph TB
    subgraph "Version Detection"
        DML_VERSION["dml_version = (1,2) or (1,4)"]
        API_VERSION["compat_api_version<br/>from --simics-api flag"]
    end
    
    subgraph "Feature Flags (breaking_changes.py)"
        FLAGS["CompatFeature objects<br/>enabled based on versions"]
        EXAMPLES["modern_attributes<br/>lenient_typechecking<br/>suppress_WLOGMIXUP<br/>port_proxy_ifaces"]
    end
    
    subgraph "Compatibility Layer"
        DML12_COMPAT["dml12-compatibility.dml<br/>provides 1.4 features in 1.2"]
        COMPAT_PY["compat.py<br/>dml12_int()<br/>state_change_dml12"]
    end
    
    subgraph "Code Generation Variations"
        ATTR_API["attribute registration:<br/>legacy vs modern API"]
        TYPEDEF_CHECK["type checking:<br/>lenient vs strict"]
        LOGGING["log level mixup:<br/>warn vs suppress"]
    end
    
    DML_VERSION --> FLAGS
    API_VERSION --> FLAGS
    FLAGS --> DML12_COMPAT
    FLAGS --> COMPAT_PY
    FLAGS --> ATTR_API
    FLAGS --> TYPEDEF_CHECK
    FLAGS --> LOGGING
```

Sources: [py/dml/breaking_changes.py:1-465](), [py/dml/compat.py:1-77](), [RELEASENOTES-1.4.md:1-588]()

### Breaking Change Mechanism

The `CompatFeature` class represents a specific compatibility feature that can be enabled or disabled:

```mermaid
graph LR
    subgraph "CompatFeature Definition"
        DEFINE["class CompatFeature<br/>{ident, enabled, msg}"]
        EXAMPLE["modern_attributes =<br/>CompatFeature(<br/>  'modern_attributes',<br/>  api >= 7,<br/>  description)"]
    end
    
    subgraph "Usage in Compiler"
        CHECK["if modern_attributes.enabled:<br/>  use_modern_api()<br/>else:<br/>  use_legacy_api()"]
    end
    
    subgraph "User Override"
        FLAG["--no-compat=modern_attributes<br/>force disable feature"]
    end
    
    DEFINE --> EXAMPLE
    EXAMPLE --> CHECK
    FLAG --> CHECK
```

Sources: [py/dml/breaking_changes.py:46-142](), [py/dml/c_backend.py:264-265]()

Key compatibility features:

| Feature | Default Enabled | Purpose |
|---------|-----------------|---------|
| `modern_attributes` | API >= 8 | Use modern attribute registration API without dictionary support |
| `lenient_typechecking` | API <= 7 | Allow loose pointer type conversions |
| `suppress_WLOGMIXUP` | API <= 6 | Don't warn about log level/group confusion |
| `port_proxy_ifaces` | API < ∞ | Generate interface trampolines for port arrays |
| `legacy_attributes` | API <= 7 | Use legacy attribute registration API |

Sources: [py/dml/breaking_changes.py:263-465](), [RELEASENOTES.md:168-170]()

### DML 1.2/1.4 Interoperability

The `dml12-compatibility.dml` library enables DML 1.4 code to be imported from DML 1.2 devices:

```mermaid
graph TB
    subgraph "DML 1.4 Module"
        MOD14["device.dml<br/>dml 1.4;<br/>saved int x;"]
    end
    
    subgraph "DML 1.2 Device"
        DEV12["device.dml<br/>dml 1.2;<br/>import 'dml12-compatibility.dml';<br/>import 'device.dml';"]
    end
    
    subgraph "Compatibility Adaptations"
        PARAMS["shared parameters:<br/>name, desc, etc."]
        METHODS["method wrappers:<br/>dml12_compat_read_register"]
        EVENTS["event templates:<br/>simple_time_event"]
        SAVED["saved variable support:<br/>field.val parameter"]
    end
    
    MOD14 --> DEV12
    DEV12 --> PARAMS
    DEV12 --> METHODS
    DEV12 --> EVENTS
    DEV12 --> SAVED
```

Sources: [lib/1.4/dml12-compatibility.dml:1-1084](), [RELEASENOTES.md:62-66](), [RELEASENOTES-1.2.md:86-118]()

The compatibility layer works by:
1. Defining 1.4-style templates that work in 1.2 context
2. Providing shared parameters that both versions can access
3. Wrapping method signatures to handle different calling conventions
4. Translating event semantics between versions

This allows gradual migration where parts of a device are converted to 1.4 while the main device remains in 1.2.

Sources: [lib/1.4/dml12-compatibility.dml:1-1084](), [py/dml/compat.py:1-77](), [RELEASENOTES-1.2.md:1-121](), [RELEASENOTES-1.4.md:1-588]()