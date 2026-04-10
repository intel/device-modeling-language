# Event and Hook System

<details>
<summary>Relevant source files</summary>

The following files were used as context for generating this wiki page:

- [include/simics/dmllib.h](include/simics/dmllib.h)
- [lib/1.2/dml-builtins.dml](lib/1.2/dml-builtins.dml)
- [lib/1.4/dml-builtins.dml](lib/1.4/dml-builtins.dml)
- [py/dml/c_backend.py](py/dml/c_backend.py)
- [py/dml/codegen.py](py/dml/codegen.py)
- [py/dml/ctree.py](py/dml/ctree.py)

</details>



## Purpose and Scope

This page documents DML's event and hook system, which provides mechanisms for delayed execution and callback registration. The system consists of:

- **Events**: Three forms of the `after` statement for scheduling delayed execution
- **Hooks**: Objects that allow multiple callbacks to be registered and invoked together

For information about the lifecycle methods (`init`, `post_init`, `destroy`), see [Standard Library](#4). For trait-based polymorphism, see [Trait System Implementation](#6.3).

---

## After Statement Overview

DML provides three distinct forms of the `after` statement for scheduling delayed execution:

| Form | Execution Time | Checkpointed | Use Case |
|------|---------------|--------------|----------|
| `after` delay | After time/cycle delay | Yes | Time-based device behavior |
| `after` on hook | When hook is sent | Yes | Event-driven callbacks |
| `immediate after` | Before next Simics instruction | No | Deferred side effects |

All three forms support **after domains**, which are object identity values used for selective cancellation via `cancel_after()`.

Sources: [lib/1.4/dml-builtins.dml:2906-3198](), [py/dml/ctree.py:697-817]()

---

## After with Time/Cycle Delay

### DML Syntax

```dml
after (delay_expr) seconds: callback_method(args);
after (delay_expr) cycles: callback_method(args);
after (delay_expr) s on (domain_expr): callback_method(args);
```

### Compilation Pipeline

```mermaid
graph TD
    DML["DML after statement<br/>site.dml"]
    Parse["ast.EAfter node"]
    Codegen["codegen_after()<br/>py/dml/codegen.py"]
    
    InfoLookup["get_after_delay()<br/>Returns AfterDelayInfo"]
    InfoCache["dml.globals.after_delay_infos<br/>Dict[key, AfterDelayInfo]"]
    
    MethodInfo["AfterDelayIntoMethodInfo<br/>key = method node"]
    SendNowInfo["AfterDelayIntoSendNowInfo<br/>key = TypeSequenceInfo"]
    
    Statement["ctree.After statement"]
    EventPost["SIM_event_post_time/cycle<br/>Generated C code"]
    
    Callback["_simple_event_N_callback()<br/>Generated callback function"]
    EventClass["Simics event class<br/>registered at init"]
    
    DML --> Parse
    Parse --> Codegen
    Codegen --> InfoLookup
    InfoLookup --> InfoCache
    InfoCache --> MethodInfo
    InfoCache --> SendNowInfo
    
    InfoLookup --> Statement
    Statement --> EventPost
    
    MethodInfo --> Callback
    SendNowInfo --> Callback
    Callback --> EventClass
```

### AfterDelayInfo Structure

The compiler creates an `AfterDelayInfo` instance for each unique combination of target callback and argument types:

```mermaid
graph LR
    subgraph "AfterDelayInfo Subclasses"
        Method["AfterDelayIntoMethodInfo<br/>key: method node<br/>args_type: struct of inp params"]
        SendNow["AfterDelayIntoSendNowInfo<br/>key: TypeSequenceInfo<br/>args_type: hookref + args"]
    end
    
    Base["AfterDelayInfo (abstract)<br/>key: unique identifier<br/>dimsizes: method dimensions<br/>cident_prefix: _simple_event_N_<br/>cident_callback: callback func name<br/>cident_get_value: serializer<br/>cident_set_value: deserializer"]
    
    Method --> Base
    SendNow --> Base
    
    Base --> Generated["Generated Artifacts:<br/>- Callback function<br/>- Serializer/deserializer<br/>- Event class registration"]
```

Sources: [py/dml/codegen.py:461-665](), [py/dml/ctree.py:697-756]()

### Runtime Structures

```mermaid
graph TB
    subgraph "Generated C Code (ctree.After)"
        PostCall["SIM_event_post_time/cycle"]
        EventData["_simple_event_data_t struct"]
    end
    
    subgraph "dmllib.h Runtime (_simple_event_data_t)"
        Indices["uint32 *indices<br/>Method array indices"]
        Args["void *args<br/>Callback arguments"]
        Domains["_identity_t *domains<br/>Cancellation domains"]
        NoDomains["uint64 no_domains"]
    end
    
    subgraph "Event Queue"
        SimicsEvent["Simics event instance<br/>Posted to device clock"]
        ClassPointer["Event class pointer"]
        DataPointer["User data = _simple_event_data_t*"]
    end
    
    subgraph "Callback Execution"
        Handler["Event class callback handler"]
        Deserialize["_simple_event_N_get_value()"]
        CallMethod["Method call with deserialized args"]
        Cleanup["MM_FREE(data)"]
    end
    
    PostCall --> EventData
    EventData --> Indices
    EventData --> Args
    EventData --> Domains
    EventData --> NoDomains
    
    EventData --> DataPointer
    DataPointer --> SimicsEvent
    SimicsEvent --> Handler
    Handler --> Deserialize
    Deserialize --> CallMethod
    CallMethod --> Cleanup
```

Sources: [include/simics/dmllib.h:859-967](), [py/dml/c_backend.py]()

### Event Serialization Format

Events are checkpointed using a pseudo-dictionary format in `attr_value_t`:

```
[["indices", [idx0, idx1, ...]],
 ["arguments", [arg0, arg1, ...]],
 ["domains", [domain0, domain1, ...]]]
```

Legacy format (pre-2022) is also supported for backward compatibility:
```
[[idx0, idx1, ...], [arg0, arg1, ...]]
```

Sources: [include/simics/dmllib.h:887-1015]()

---

## After on Hook

### DML Syntax

```dml
after on (hookref_expr): callback_method(args);
after on (hookref_expr) on (domain_expr): callback_method(args);
```

### Compilation Pipeline

```mermaid
graph TD
    DML["after on hook statement"]
    Parse["ast.EAfterOnHook node"]
    
    TypeSeq["get_type_sequence_info()<br/>Creates TypeSequenceInfo<br/>for hook message types"]
    
    InfoLookup["TypeSequenceInfo.get_after_on_hook()<br/>Returns AfterOnHookInfo"]
    
    MethodInfo["AfterOnHookIntoMethodInfo<br/>prim_key = method<br/>param_to_msg_comp mapping"]
    SendNowInfo["AfterOnHookIntoSendNowInfo<br/>prim_key = sendnow typeseq"]
    
    Statement["ctree.AfterOnHook"]
    Attach["_DML_attach_callback_to_hook()<br/>Runtime function call"]
    
    HookStruct["Hook's _dml_hook_t.callbacks<br/>Linked list updated"]
    
    DML --> Parse
    Parse --> TypeSeq
    TypeSeq --> InfoLookup
    InfoLookup --> MethodInfo
    InfoLookup --> SendNowInfo
    
    InfoLookup --> Statement
    Statement --> Attach
    Attach --> HookStruct
```

### AfterOnHookInfo Structure

```mermaid
graph TB
    subgraph "AfterOnHookInfo (abstract)"
        PrimKey["prim_key:<br/>method or TypeSequenceInfo"]
        ParamMap["param_to_msg_comp:<br/>Dict mapping callback params<br/>to hook message components"]
        TypeSeq["typeseq_info:<br/>Hook message type sequence"]
        
        Uniq["uniq: global index"]
        Prefix["cident_prefix:<br/>_after_on_hook_N_"]
        
        ArgsType["args_type:<br/>Struct of non-message params"]
        Serializer["cident_args_serializer"]
        Deserializer["cident_args_deserializer"]
    end
    
    subgraph "Method Target"
        MethodConcrete["AfterOnHookIntoMethodInfo<br/>Target: method node<br/>Maps some params to message"]
        MethodCallback["generate_callback_call():<br/>Calls method with mixed<br/>message + serialized args"]
    end
    
    subgraph "SendNow Target"
        SendNowConcrete["AfterOnHookIntoSendNowInfo<br/>Target: hook send_now<br/>sendnow_typeseq_info"]
        SendNowCallback["generate_callback_call():<br/>Constructs message and<br/>calls _DML_send_hook()"]
    end
    
    PrimKey --> MethodConcrete
    PrimKey --> SendNowConcrete
    ParamMap --> MethodConcrete
    ParamMap --> SendNowConcrete
    
    MethodConcrete --> MethodCallback
    SendNowConcrete --> SendNowCallback
```

### Hook Callback Lifecycle

```mermaid
sequenceDiagram
    participant DML as DML Code
    participant Attach as _DML_attach_callback_to_hook
    participant Hook as _dml_hook_t
    participant List as Callback List
    participant Send as Hook.send_now()
    participant Callback as Registered Callback
    
    DML->>Attach: after on (hook): method(args)
    Attach->>List: Allocate callback entry
    Note over List: Contains:<br/>- info (AfterOnHookInfo*)<br/>- indices<br/>- serialized args<br/>- domains
    List->>Hook: Append to hook.callbacks
    
    Note over Hook: Later...
    
    Send->>Hook: hook.send_now(msg)
    Hook->>List: Iterate callbacks
    loop Each Callback
        List->>Callback: info->cident_callback
        Note over Callback: Deserialize args<br/>Mix with message<br/>Call target
        Callback->>List: Return
    end
```

Sources: [py/dml/codegen.py:531-855](), [py/dml/ctree.py:764-790]()

---

## Immediate After

### DML Syntax

```dml
immediate after: callback_method(args);
immediate after on (domain_expr): callback_method(args);
```

Immediate after executes before the next Simics instruction step, making it suitable for deferred side effects that must complete within the current transaction context. Unlike timed events, immediate after callbacks are **not checkpointed**.

### Compilation Pipeline

```mermaid
graph TD
    DML["immediate after statement"]
    
    InfoLookup["get_immediate_after()<br/>Returns ImmediateAfterInfo"]
    MethodInfo["ImmediateAfterIntoMethodInfo"]
    SendNowInfo["ImmediateAfterIntoSendNowInfo"]
    
    Statement["ctree.ImmediateAfter"]
    Post["_DML_post_immediate_after()<br/>dmllib.h runtime"]
    
    Queue["device._immediate_after_state<br/>Callback queue"]
    StepHandler["_dml_immediate_after_handler()<br/>Simics step event handler"]
    Execute["Execute all queued callbacks"]
    
    DML --> InfoLookup
    InfoLookup --> MethodInfo
    InfoLookup --> SendNowInfo
    InfoLookup --> Statement
    
    Statement --> Post
    Post --> Queue
    Queue --> StepHandler
    StepHandler --> Execute
```

### Runtime State

The device structure contains an `_immediate_after_state` pointer managed by the runtime library:

```c
// In generated device struct
_dml_immediate_after_state_t *_immediate_after_state;
```

The runtime maintains a queue of pending callbacks that execute before the next instruction. The queue is cleared after execution and is not persisted in checkpoints.

Sources: [py/dml/codegen.py:583-903](), [py/dml/ctree.py:792-817](), [include/simics/dmllib.h]()

---

## Hook Objects and Operations

### Hook Template Hierarchy

```mermaid
graph TD
    BaseHook["hook template<br/>lib/1.4/dml-builtins.dml:2688"]
    
    SendNow["send_now() method<br/>Invokes all callbacks immediately"]
    Register["Automatically instantiated<br/>for all hook objects"]
    
    Storage["C struct: _dml_hook_t<br/>In device structure"]
    Callbacks["callbacks: linked list<br/>of registered callbacks"]
    
    VTHook["THook type<br/>Type system representation"]
    HookRef["_hookref_t<br/>Runtime reference type"]
    
    BaseHook --> SendNow
    BaseHook --> Register
    Register --> Storage
    Storage --> Callbacks
    
    VTHook --> HookRef
```

### Hook Type System

```mermaid
graph LR
    subgraph "Python Type System"
        THook["THook(args_tuple, validated)<br/>py/dml/types.py"]
        TypeSeq["TypeSequence<br/>Tuple of DMLType"]
        Validated["validated: bool<br/>True after send_now validation"]
    end
    
    subgraph "C Type System"
        HookRefType["_hookref_t<br/>Runtime reference"]
        Identity["_identity_t id<br/>Object identity for hook"]
    end
    
    subgraph "Generated Code"
        SendNowRef["mkHookSendNowRef<br/>ctree expression node"]
        SendNowApply["mkHookSendNowApply<br/>Function call node"]
        
        HookArrayRef["HookArrayRef<br/>For indexed hook access"]
    end
    
    THook --> TypeSeq
    THook --> Validated
    THook --> HookRefType
    HookRefType --> Identity
    
    THook --> SendNowRef
    SendNowRef --> SendNowApply
    THook --> HookArrayRef
```

Sources: [py/dml/types.py](), [lib/1.4/dml-builtins.dml:2688-2904]()

### Hook Send Operation

```mermaid
sequenceDiagram
    participant DML as DML Code
    participant SendNow as hook.send_now(args)
    participant Runtime as _DML_send_hook
    participant Queue as Detached Hook Queue
    participant List as Hook Callbacks
    participant Callback as Individual Callback
    
    DML->>SendNow: hook.send_now(arg0, arg1)
    SendNow->>Runtime: _DML_send_hook(dev, queue, hook, args)
    
    alt Hook is detached
        Runtime->>Queue: Push to queue
        Note over Queue: Deferred execution
    else Hook is live
        Runtime->>List: Iterate callbacks
        loop Each Callback
            List->>Callback: Execute callback
            Note over Callback: May access message args<br/>via param_to_msg_comp
            alt Callback throws
                Callback->>Runtime: Exception caught
                Note over Runtime: Log and continue
            end
            Callback->>List: Return
        end
    end
    
    Runtime->>SendNow: Return
    SendNow->>DML: Return
```

### Hook Reference Resolution

```c
// C macro in generated code
_DML_resolve_hookref(_dev, _hook_aux_infos, hookref)

// Returns _dml_hook_t* by:
// 1. If hookref is constant (HookRef), return &_dev->hookpath
// 2. Otherwise, use _hook_aux_infos lookup table to resolve
//    _hookref_t.id to _dml_hook_t* at runtime
```

Sources: [py/dml/ctree.py:759-762](), [include/simics/dmllib.h]()

---

## Hook Serialization

### Serialization Format

Hook references are serialized as `_identity_t` values, which checkpoint the object and index:

```
[logname_string, [index0, index1, ...]]
```

Example:
```
["device.hooks.my_hook", [0]]  // hooks.my_hook[0]
["", []]                        // Zero-initialized hookref
```

### Callback Serialization

Callbacks attached via `after on hook` are serialized within the hook's checkpoint data:

```
{
  "callbacks": [
    {
      "info_uniq": N,           // AfterOnHookInfo index
      "indices": [i, j],        // Method indices
      "args": <serialized>,     // Non-message arguments
      "domains": [<id>, <id>]   // Cancellation domains
    },
    ...
  ]
}
```

### Checkpoint Compatibility

The system supports deserializing hooks from older checkpoints where:
- Hook objects didn't exist (creates empty hooks)
- Callback format was different (legacy format support)
- Object topology has changed (reports error with helpful message)

Sources: [include/simics/dmllib.h:691-829](), [py/dml/serialize.py]()

---

## Event Cancellation

### cancel_after() Method

Every DML object provides a `cancel_after()` method that cancels pending events:

```dml
// In object template
shared method cancel_after() {
    local object ref = this;
    _cancel_simple_events(dev.obj, cast(&ref, _traitref_t *)->id);
}
```

### Cancellation Mechanism

```mermaid
graph TD
    subgraph "DML Call Site"
        CancelCall["obj.cancel_after()"]
        Identity["Object identity extraction<br/>cast to _traitref_t->id"]
    end
    
    subgraph "Runtime Library"
        CancelFunc["_cancel_simple_events(dev, id)"]
        EventQueue["SIM_event_find_next_cycle<br/>with predicate"]
        Predicate["_simple_event_predicate<br/>Check if event.domains contains id"]
        EventCancel["SIM_event_cancel_time/cycle"]
    end
    
    subgraph "Event Data"
        EventStruct["_simple_event_data_t"]
        Domains["domains: _identity_t*<br/>Array of domain identities"]
    end
    
    CancelCall --> Identity
    Identity --> CancelFunc
    CancelFunc --> EventQueue
    EventQueue --> Predicate
    Predicate --> Domains
    
    Predicate -->|Match| EventCancel
    EventCancel --> EventStruct
```

Cancellation works by:
1. Finding all events with matching device
2. Filtering to events whose `domains` array contains the cancelling object's identity
3. Calling Simics `SIM_event_cancel` on matched events
4. The event destructor (`_destroy_simple_event_data`) frees allocated memory

Sources: [lib/1.4/dml-builtins.dml:574-577](), [include/simics/dmllib.h:866-885]()

---

## Generated Artifacts

### Per-Device Artifacts

For each device, the compiler generates:

| Artifact | Purpose | Location |
|----------|---------|----------|
| `_immediate_after_state` | Runtime state for immediate after queue | Device struct member |
| `_detached_hook_queue_stack` | Stack of detached hook queues | Device struct member |
| `_hook_aux_infos` | Hookref resolution lookup table | Global array |
| `_after_on_hook_infos` | AfterOnHookInfo metadata | Global array |
| `_id_infos` | Object identity metadata | Global array |

### Per-AfterInfo Artifacts

For each unique `AfterDelayInfo`:

```c
// Event callback function
static void _simple_event_N_callback(conf_object_t *obj, lang_void *data) {
    _simple_event_data_t *event_data = (_simple_event_data_t *)data;
    // Deserialize indices and args
    // Call target method or send hook
    _free_simple_event_data(*event_data);
}

// Serialization functions (if needed)
static attr_value_t _simple_event_N_get_value(conf_object_t *obj, 
                                                lang_void *data);
static set_error_t _simple_event_N_set_value(void *dont_care, 
                                              conf_object_t *obj,
                                              lang_void *data,
                                              attr_value_t *value);

// Event class registration in init
_DML_register_event_class(&_dev->obj, "simple_event_N",
                          _simple_event_N_callback,
                          _simple_event_N_get_value,
                          _simple_event_N_set_value,
                          &_simple_event_classes[N]);
```

For each unique `AfterOnHookInfo`:

```c
// Callback function
static void _after_on_hook_N_callback(conf_object_t *obj,
                                       const uint32 *indices,
                                       const void *args,
                                       const void *msg);

// Argument serializers (if has_serialized_args)
static attr_value_t _after_on_hook_N_args_serializer(const void *args);
static set_error_t _after_on_hook_N_args_deserializer(attr_value_t val,
                                                        void *out);

// Info struct in global array
{
    .cident_callback = _after_on_hook_N_callback,
    .cident_args_serializer = _after_on_hook_N_args_serializer,
    .cident_args_deserializer = _after_on_hook_N_args_deserializer,
    .dimensions = <N>,
    .typeseq_uniq = <M>,
    .string_key = "(<types>, (<param_map>))"
}
```

Sources: [py/dml/c_backend.py](), [py/dml/codegen.py:461-903]()

---

## Type Sequence System

### TypeSequenceInfo

Hook message types are tracked through `TypeSequenceInfo` instances, which cache information about unique type sequences:

```mermaid
graph TD
    subgraph "TypeSequenceInfo"
        Types["types: tuple[DMLType]<br/>Message component types"]
        Uniq["uniq: int<br/>Global unique ID"]
        Struct["struct: TStruct<br/>C struct for message"]
        StringKey["string_key: str<br/>'(type1, type2, ...)'"]
        AfterOnHooks["after_on_hooks: dict<br/>Cached AfterOnHookInfo"]
    end
    
    subgraph "Global Cache"
        GlobalDict["dml.globals.type_sequence_infos<br/>Dict[TypeSequence, TypeSequenceInfo]"]
    end
    
    subgraph "Generated C Struct"
        CStruct["struct _typeseq_N {<br/>  typeof(arg0) comp0;<br/>  typeof(arg1) comp1;<br/>  ...<br/>};"]
    end
    
    Types --> Struct
    Struct --> CStruct
    StringKey --> Types
    
    GlobalDict --> TypeSequenceInfo
    TypeSequenceInfo --> AfterOnHooks
```

### Type Sequence Creation

```python
# py/dml/codegen.py
def get_type_sequence_info(index, create_new=False):
    typeseq = TypeSequence(index)  # Tuple of types
    try:
        return dml.globals.type_sequence_infos[typeseq]
    except KeyError:
        if create_new:
            info = TypeSequenceInfo(typeseq.types, 
                                   len(dml.globals.type_sequence_infos))
            dml.globals.type_sequence_infos[typeseq] = info
            return info
        else:
            return None
```

Each `TypeSequenceInfo` serves as a cache key for:
- `AfterDelayIntoSendNowInfo` (for `after delay: hook.send_now(...)`)
- `AfterOnHookIntoSendNowInfo` (for `after on hook: other_hook.send_now(...)`)
- `AfterOnHookIntoMethodInfo` (for `after on hook: method(...)` with parameter mapping)

Sources: [py/dml/codegen.py:421-459]()

---

## Error Handling

### Exception Propagation

Events and hooks handle exceptions differently:

| Context | Behavior | Implementation |
|---------|----------|----------------|
| `after` delay callback | Log error, continue | `LogFailure` wrapper |
| `after on hook` callback | Log error, continue | Loop catches per-callback |
| `immediate after` callback | Log error, continue | Runtime catches exceptions |
| `send_now()` method | Propagates to caller | Normal DML exception handling |

### Logging Context

When event callbacks execute, they run with a `LogFailure` context that logs uncaught exceptions:

```c
// Generated in AfterDelayIntoMethodInfo.generate_callback_call
void _simple_event_N_callback(conf_object_t *obj, lang_void *data) {
    // ... setup ...
    
    // LogFailure context: logs "Uncaught DML exception" on throw
    method_call(indices, args);  // May throw
    
    // Cleanup even if exception occurred
}
```

Sources: [py/dml/codegen.py:178-188](), [py/dml/c_backend.py]()

---

## Performance Considerations

### Event Data Allocation

Event data is allocated with `MM_MALLOC`/`MM_ZALLOC` and freed in the event callback or destructor. For events with no indices/args/domains, no allocation occurs:

```c
// Optimized case: no allocation needed
if (!indices && !args && !domains) {
    SIM_event_post_time(clock, evclass, obj, delay, NULL);
}
```

### Hook Callback Overhead

Hook callbacks are stored in a linked list and executed sequentially. Performance characteristics:
- **Registration**: O(1) append to list
- **Send**: O(n) iteration over callbacks
- **Cancellation**: O(n) list traversal

### Memoization Interaction

Independent methods with memoization (see [Memoization](#)) cannot use `after` statements that reference `this`, as the `this` object may vary across memoized calls.

Sources: [py/dml/ctree.py:697-817](), [include/simics/dmllib.h]()