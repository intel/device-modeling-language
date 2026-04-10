# Object Model

<details>
<summary>Relevant source files</summary>

The following files were used as context for generating this wiki page:

- [doc/1.4/language.md](doc/1.4/language.md)
- [lib/1.2/dml-builtins.dml](lib/1.2/dml-builtins.dml)
- [lib/1.4/dml-builtins.dml](lib/1.4/dml-builtins.dml)
- [py/dml/crep.py](py/dml/crep.py)
- [py/dml/dmlparse.py](py/dml/dmlparse.py)
- [py/dml/messages.py](py/dml/messages.py)
- [py/dml/structure.py](py/dml/structure.py)
- [py/dml/template.py](py/dml/template.py)
- [py/dml/traits.py](py/dml/traits.py)
- [py/dml/types.py](py/dml/types.py)

</details>



## Purpose and Scope

This page describes the **DML object model**, which defines the hierarchical structure of objects in a DML device model and the containment relationships between them. The object model specifies which object types can exist, how they nest within each other, and the common infrastructure provided by the `object` base template.

For information about the type system used by DML expressions and declarations, see [Type System](#3.3). For details on templates and code reuse mechanisms, see [Templates](#3.5). For the trait-based polymorphism system, see [Traits](#3.6). For comprehensive documentation on each object type's parameters and methods, see [Core Templates](#4.1).

## Object Hierarchy

Every DML model describes a single **device object** which contains a hierarchical tree of member objects. Each object has a specific **object type** (e.g., `bank`, `register`, `field`) that determines its role and capabilities. All objects inherit from the base `object` template, which provides common parameters and functionality.

### Object Containment Rules

The object model enforces strict containment rules that determine which object types can appear as children of other object types:

```mermaid
graph TD
    device["device<br/>(top-level)"]
    bank["bank"]
    port["port"]
    subdevice["subdevice"]
    register["register"]
    field["field"]
    attribute1["attribute"]
    attribute2["attribute"]
    attribute3["attribute"]
    attribute4["attribute"]
    attribute5["attribute"]
    connect1["connect"]
    connect2["connect"]
    connect3["connect"]
    connect4["connect"]
    interface["interface"]
    event1["event"]
    event2["event"]
    implement1["implement"]
    implement2["implement"]
    implement3["implement"]
    implement4["implement"]
    group["group<br/>(neutral container)"]
    
    device --> bank
    device --> port
    device --> subdevice
    device --> attribute1
    device --> connect1
    device --> event1
    device --> implement1
    device --> group
    
    bank --> register
    bank --> attribute2
    bank --> connect2
    bank --> event2
    bank --> implement2
    
    register --> field
    
    port --> attribute3
    port --> connect3
    port --> implement3
    
    subdevice --> bank
    subdevice --> attribute4
    subdevice --> connect4
    subdevice --> implement4
    
    connect1 --> interface
    connect2 --> interface
    connect3 --> interface
    connect4 --> interface
    
    group -.-> bank
    group -.-> port
    group -.-> subdevice
    group -.-> register
    group -.-> field
    group -.-> attribute5
```

**Sources:** [doc/1.4/language.md:304-340](), [lib/1.4/dml-builtins.dml:269-747]()

Key containment rules:
- **device**: Top-level object; can contain `bank`, `port`, `subdevice`, `attribute`, `connect`, `implement`, `event`, and `group`
- **bank**: Can contain `register`, `attribute`, `connect`, `implement`, and `event`
- **register**: Can contain `field` objects
- **field**: Leaf node in the bank→register→field hierarchy
- **port** and **subdevice**: Similar containment rules to device
- **connect**: Can only contain `interface` objects
- **interface**: Must be directly under a `connect` object
- **implement**: Can only appear under `device`, `port`, `bank`, or `subdevice`
- **group**: Neutral container that can appear anywhere and inherit parent's containment rules (except cannot contain `interface` or `implement`)
- **event**: Can appear anywhere except under `field`, `interface`, `implement`, or another `event`

## Base Object Template

All DML objects inherit from the `object` template, which provides fundamental parameters and methods:

### Core Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `this` | reference | Always refers to the current object |
| `objtype` | string constant | Object type (e.g., `"register"`, `"bank"`) |
| `parent` | reference or undefined | Containing object; `undefined` for device |
| `dev` | reference | The top-level device object |
| `name` | const char * | Object name exposed to end-user |
| `qname` | string | Fully qualified name including indices |
| `indices` | list | Local indices for this object (empty for non-arrays) |
| `templates` | auto | For template-qualified method calls |

**Sources:** [lib/1.4/dml-builtins.dml:540-578](), [doc/1.4/language.md:480-538]()

### Object Identity and Indexing

Objects can be declared as **arrays** with one or more index parameters:

```
register regs[i < 4][j < 11];  // 2D register array
```

The `indices` parameter contains the list of local indices `[i, j]`. Individual index parameters (like `i` and `j`) are also available, or can be named with the discard identifier `_` if not needed.

**Sources:** [lib/1.4/dml-builtins.dml:510-528]()

### Object Methods

The `object` template provides:
- **cancel_after()**: Cancels all pending `after` events associated with this object (but not subobjects)

Common lifecycle methods are available through template inheritance:
- **init()**: Called when device is created, before attributes are initialized
- **post_init()**: Called after attributes are initialized
- **destroy()**: Called when device is being deleted

**Sources:** [lib/1.4/dml-builtins.dml:530-537](), [lib/1.4/dml-builtins.dml:373-477]()

## Object Types

### Device Object

The `device` object represents the top-level scope of a DML file and corresponds to a Simics configuration class.

```mermaid
graph LR
    dml_device["device (DML)"]
    simics_class["Configuration Class (Simics)"]
    conf_object["conf_object_t *"]
    
    dml_device --> simics_class
    simics_class --> conf_object
    
    style dml_device fill:#f9f9f9
    style simics_class fill:#f9f9f9
```

Key parameters:
- `classname`: Name of the Simics configuration class (defaults to device name)
- `obj`: Pointer to the `conf_object_t` C struct
- `register_size`: Default register width in bytes (inherited by banks)
- `byte_order`: Default byte order (`"little-endian"` or `"big-endian"`)
- `use_io_memory`: Default value for banks

**Sources:** [lib/1.4/dml-builtins.dml:626-722](), [doc/1.4/language.md:379-401]()

### Register Banks

A **bank** groups registers and exposes them through the `io_memory` Simics interface. Banks can be arrays, where each element is a separate Simics configuration object.

Bank configuration objects in Simics are named with a `.bank` prefix: for `bank regs[i < 2]` in device `dev`, the Simics objects are `dev.bank.regs[0]` and `dev.bank.regs[1]`.

**Sources:** [doc/1.4/language.md:402-420]()

### Registers and Fields

A **register** represents a hardware register with an integer value. Registers have a fixed size (1-8 bytes) and can be mapped to an address within the bank via the `offset` parameter, or left unmapped.

A **field** represents a bit range within a register. Fields enable modeling hardware registers where different bit ranges have different semantics:

```
register r0 size 2 @ 0x0000 {
    field status @ [2:0];     // bits 0-2
    field flags @ [8:3];      // bits 3-8
    field reserved @ [15:9];  // bits 9-15
}
```

**Sources:** [doc/1.4/language.md:421-636]()

### Attributes

An **attribute** object exposes device state to Simics for configuration, checkpointing, or inspection. Attributes define `get()` and `set()` methods and specify a Simics attribute type.

**Sources:** [lib/1.4/dml-builtins.dml:944-968](), [doc/1.4/language.md:638-668]()

### Connects and Interfaces

A **connect** object represents a connection to another Simics device or object. Each `connect` can contain **interface** objects that specify which Simics interfaces must be supported by the connected object.

**Sources:** [doc/1.4/language.md:669-709]()

### Implements

An **implement** object declares that the device implements a specific Simics interface, making it available to other objects in the simulation.

**Sources:** [doc/1.4/language.md:710-726]()

### Ports and Subdevices

**Port** objects group interfaces and connections, creating separate Simics configuration objects. **Subdevice** objects represent sub-components of a device with their own banks and hierarchy.

**Sources:** [doc/1.4/language.md:727-757]()

### Events

An **event** object represents a timed callback mechanism. Events can be scheduled to execute at a specific time or cycle count in the simulation.

**Sources:** [doc/1.4/language.md:758-786]()

### Groups

A **group** is a neutral container object used for organizational purposes. It can appear anywhere in the hierarchy and can contain any objects its parent can contain (with restrictions on `interface` and `implement`).

**Sources:** [lib/1.4/dml-builtins.dml:738-746](), [doc/1.4/language.md:336-339]()

## Implementation Architecture

### Python Object Representation

The DML compiler's Python implementation represents the object tree using classes defined in `py/dml/objects.py`. The structure-building process is coordinated by functions in `py/dml/structure.py`.

```mermaid
graph TB
    mkglobals["mkglobals()<br/>structure.py"]
    mkdev["mkdev()<br/>structure.py"]
    mkobj["mkobj()<br/>structure.py"]
    
    DMLObject["DMLObject<br/>(base class)"]
    Device["Device"]
    CompositeObject["CompositeObject"]
    Bank["Bank"]
    Register["Register"]
    Field["Field"]
    Attribute["Attribute"]
    Method["Method"]
    
    mkglobals --> mkdev
    mkdev --> mkobj
    mkobj --> DMLObject
    
    DMLObject --> Device
    DMLObject --> CompositeObject
    DMLObject --> Attribute
    DMLObject --> Method
    
    CompositeObject --> Bank
    CompositeObject --> Register
    
    Bank --> Register
    Register --> Field
    
    style mkglobals fill:#f9f9f9
    style mkdev fill:#f9f9f9
    style mkobj fill:#f9f9f9
```

**Sources:** [py/dml/structure.py:74-112](), [py/dml/structure.py:39-41]()

### Object Creation Process

The compiler builds the object tree in phases:

1. **Global Symbol Collection** (`mkglobals()`): Processes top-level declarations (constants, typedefs, templates, externs, loggroups)

2. **Device Tree Construction** (`mkdev()`): Creates the device object hierarchy by:
   - Instantiating object declarations
   - Expanding templates with `add_templates()`
   - Merging parameter definitions with `merge_parameters()`
   - Resolving method overrides

3. **Template Instantiation**: The template system processes inheritance and composition:

```mermaid
graph LR
    ObjectSpec["ObjectSpec<br/>(AST-level)"]
    add_templates["add_templates()"]
    Template["Template"]
    InstantiatedTemplateSpec["InstantiatedTemplateSpec"]
    used_templates["used_templates dict"]
    
    ObjectSpec --> add_templates
    Template --> add_templates
    add_templates --> InstantiatedTemplateSpec
    add_templates --> used_templates
```

**Sources:** [py/dml/structure.py:525-576](), [py/dml/template.py:64-146]()

### Parameter and Method Resolution

When multiple templates or object declarations define the same parameter or method, the compiler uses a **rank system** to determine precedence:

- Each `ObjectSpec` has a `Rank` with an `inferior` set
- A declaration in ObjectSpec A overrides a declaration in ObjectSpec B if A's rank's inferior set contains B's rank
- The `merge_parameters()` function computes the winning declaration
- The system reports `EAMBINH` errors if there's no unique superior declaration

**Sources:** [py/dml/structure.py:578-709](), [py/dml/template.py:24-62]()

### Object Identity and C Representation

Each object in the DML hierarchy has corresponding C structures and identifiers:

```mermaid
graph TD
    object_param["object parameters<br/>(name, qname, indices)"]
    identity_t["_identity_t struct<br/>(id, encoded_index)"]
    id_info_t["_id_info_t struct<br/>(logname, dimsizes, dimensions, id)"]
    cref_session["cref_session()<br/>crep.py"]
    cref_portobj["cref_portobj()<br/>crep.py"]
    
    object_param --> identity_t
    identity_t --> id_info_t
    identity_t --> cref_session
    identity_t --> cref_portobj
    
    style identity_t fill:#f9f9f9
    style id_info_t fill:#f9f9f9
```

The `crep.py` module provides functions to generate C references to objects:
- `cref_session(node, indices)`: Reference to session/saved variables
- `cref_portobj(node, indices)`: Reference to port/bank/subdevice objects
- `cref_hook(hook, indices)`: Reference to hook objects
- `conf_object(site, node, indices)`: Get the `conf_object_t*` for an object

**Sources:** [lib/1.4/dml-builtins.dml:70-82](), [py/dml/crep.py:120-152]()

## Internal Object Parameters

The `object` template and its subtypes define several internal parameters used by the compiler:

| Parameter | Purpose |
|-----------|---------|
| `_ident` | Object identifier used in DML code |
| `_is_simics_object` | Whether this object maps to a Simics configuration object |
| `_nongroup_parent` | Closest ancestor that is not a group |
| `_object_relative_dims` | Number of dimensions relative to nearest Simics object |
| `_static_qname` | Statically-known portion of qualified name |

These parameters are primarily used during code generation and are not intended for user access.

**Sources:** [lib/1.4/dml-builtins.dml:540-578]()

## Object Template Inheritance

All standard object types inherit from the `object` template and may inherit additional templates:

```mermaid
graph TB
    object["object<br/>base template"]
    name["name"]
    desc["desc"]
    documentation["documentation"]
    limitations["limitations"]
    
    device["device"]
    bank["bank"]
    register["register"]
    field["field"]
    attribute["attribute"]
    
    init["init"]
    post_init["post_init"]
    destroy["destroy"]
    
    object --> name
    object --> desc
    object --> documentation
    object --> limitations
    
    object --> device
    object --> bank
    object --> register
    object --> field
    object --> attribute
    
    device --> init
    device --> post_init
    device --> destroy
    
    style object fill:#f9f9f9
```

The template system allows objects to inherit behavior from multiple templates while maintaining a clear override hierarchy through the rank system.

**Sources:** [lib/1.4/dml-builtins.dml:271-370](), [lib/1.4/dml-builtins.dml:626-722]()

## Relationship to Type System and Traits

The object model is distinct from but related to other DML subsystems:

- **Type System** ([#3.3](#3.3)): Objects have types, but the object hierarchy itself is not part of the type system. Session variables and fields have types from the type system.

- **Traits** ([#3.6](#3.6)): Templates can define trait types that support polymorphism. Objects instantiate templates, potentially implementing traits.

- **Templates** ([#3.5](#3.5)): The primary mechanism for code reuse in the object model. Every object instantiates its object-type template plus any additional templates specified via `is` declarations.

The object model defines the **structural** aspects of a device (what objects exist and how they nest), while types, traits, and templates define **behavioral** aspects (what operations are available and how they're implemented).

**Sources:** [py/dml/structure.py:1-41](), [py/dml/template.py:1-22]()