# Object-Oriented Programming Features

<cite>
**Referenced Files in This Document**
- [language.md](file://doc/1.2/language.md)
- [language.md](file://doc/1.4/language.md)
- [dml-builtins.dml](file://lib/1.2/dml-builtins.dml)
- [dml-builtins.dml](file://lib/1.4/dml-builtins.dml)
- [utility.dml](file://lib/1.2/utility.dml)
- [utility.dml](file://lib/1.4/utility.dml)
- [objects.py](file://py/dml/objects.py)
- [structure.py](file://py/dml/structure.py)
- [traits.py](file://py/dml/traits.py)
- [types.py](file://py/dml/types.py)
- [ctree.py](file://py/dml/ctree.py)
- [simics-types.dml](file://lib/1.2/simics-types.dml)
- [simics-configuration.dml](file://lib/1.2/simics-configuration.dml)
</cite>

## Table of Contents
1. [Introduction](#introduction)
2. [Project Structure](#project-structure)
3. [Core Components](#core-components)
4. [Architecture Overview](#architecture-overview)
5. [Detailed Component Analysis](#detailed-component-analysis)
6. [Dependency Analysis](#dependency-analysis)
7. [Performance Considerations](#performance-considerations)
8. [Troubleshooting Guide](#troubleshooting-guide)
9. [Conclusion](#conclusion)

## Introduction
This document explains DML’s object-oriented programming features with a focus on how objects are defined, composed, and orchestrated. It covers object definitions, hierarchies, composition patterns, inheritance and method override semantics, the object type system, polymorphism, method signatures and calling conventions, exception handling, attributes and state management, validation and type checking, and the relationship between DML objects and Simics configuration objects. The content is grounded in the DML language documentation and the Python compiler implementation that models DML’s object model.

## Project Structure
DML organizes device models as hierarchical object structures. The language documentation describes object types and rules; the built-in libraries define templates and default behaviors; the compiler’s Python implementation models the AST and object graph.

```mermaid
graph TB
subgraph "Documentation"
L12["doc/1.2/language.md"]
L14["doc/1.4/language.md"]
end
subgraph "Built-in Libraries"
B12["lib/1.2/dml-builtins.dml"]
U12["lib/1.2/utility.dml"]
B14["lib/1.4/dml-builtins.dml"]
U14["lib/1.4/utility.dml"]
end
subgraph "Compiler Implementation"
OBJ["py/dml/objects.py"]
STR["py/dml/structure.py"]
TRA["py/dml/traits.py"]
TYP["py/dml/types.py"]
CT["py/dml/ctree.py"]
end
subgraph "Simics Types"
ST["lib/1.2/simics-types.dml"]
SC["lib/1.2/simics-configuration.dml"]
end
L12 --> B12
L14 --> B14
B12 --> OBJ
B14 --> OBJ
U12 --> B12
U14 --> B14
OBJ --> STR
STR --> TRA
TRA --> TYP
CT --> STR
ST --> B12
SC --> B12
```

**Diagram sources**
- [language.md](file://doc/1.2/language.md#L1-L200)
- [language.md](file://doc/1.4/language.md#L1-L200)
- [dml-builtins.dml](file://lib/1.2/dml-builtins.dml#L1-L200)
- [dml-builtins.dml](file://lib/1.4/dml-builtins.dml#L1-L200)
- [utility.dml](file://lib/1.2/utility.dml#L1-L200)
- [utility.dml](file://lib/1.4/utility.dml#L1-L200)
- [objects.py](file://py/dml/objects.py#L1-L200)
- [structure.py](file://py/dml/structure.py#L1-L200)
- [traits.py](file://py/dml/traits.py#L1-L200)
- [types.py](file://py/dml/types.py#L1-L200)
- [ctree.py](file://py/dml/ctree.py#L1-L200)
- [simics-types.dml](file://lib/1.2/simics-types.dml#L1-L16)
- [simics-configuration.dml](file://lib/1.2/simics-configuration.dml#L1-L15)

**Section sources**
- [language.md](file://doc/1.2/language.md#L1-L200)
- [language.md](file://doc/1.4/language.md#L1-L200)

## Core Components
- Object model: DML models a device as a tree of objects (device, bank, register, field, attribute, connect, interface, port, subdevice, implement, event, group, session, saved, hook). Each object has a type and may contain child objects.
- Templates: Built-in templates provide default behaviors and can be instantiated to compose functionality. Examples include device, bank, register, attribute, and utility templates for read/write behaviors.
- Methods: Methods are object members with input/output parameters, optional exception handling, and optional qualifiers (e.g., nothrow, independent, startup, memoized).
- Parameters: Parameters are compile-time expressions that can be overridden; they describe static properties and can be typed or untyped.
- Attributes: Attributes expose device state to Simics, with get/set methods and configuration flags (required, optional, pseudo, none), persistence, and internal visibility.
- Composition: Objects are composed by instantiating templates and adding methods/parameters/data fields. Arrays of objects are supported with index variables.

**Section sources**
- [language.md](file://doc/1.2/language.md#L714-L800)
- [language.md](file://doc/1.4/language.md#L255-L330)
- [dml-builtins.dml](file://lib/1.2/dml-builtins.dml#L160-L390)
- [dml-builtins.dml](file://lib/1.4/dml-builtins.dml#L477-L563)
- [objects.py](file://py/dml/objects.py#L31-L120)

## Architecture Overview
DML’s object model is mapped to Simics configuration objects. The device object corresponds to a Simics configuration class; banks, registers, fields, attributes, connects, ports, subdevices, and implements map to Simics constructs. The compiler transforms DML declarations into code that binds these objects to Simics APIs.

```mermaid
graph TB
DMLDev["DML Device (device)"]
DMLBank["DML Bank (bank)"]
DMLReg["DML Register (register)"]
DMLField["DML Field (field)"]
DMLAttr["DML Attribute (attribute)"]
DMLConn["DML Connect (connect)"]
DMLPort["DML Port (port)"]
DMLSub["DML Subdevice (subdevice)"]
DMLImpl["DML Implement (implement)"]
SimDev["Simics Config Class"]
SimBank["Simics Bank Instance"]
SimReg["Simics Register Attribute"]
SimField["Simics Field Attribute"]
SimAttr["Simics Attribute"]
SimConn["Simics Connection Attribute"]
SimPort["Simics Port"]
SimSub["Simics Subdevice"]
SimImpl["Simics Implemented Interface"]
DMLDev --> SimDev
DMLBank --> SimBank
DMLReg --> SimReg
DMLField --> SimField
DMLAttr --> SimAttr
DMLConn --> SimConn
DMLPort --> SimPort
DMLSub --> SimSub
DMLImpl --> SimImpl
```

**Diagram sources**
- [language.md](file://doc/1.4/language.md#L285-L293)
- [dml-builtins.dml](file://lib/1.2/dml-builtins.dml#L322-L389)
- [dml-builtins.dml](file://lib/1.4/dml-builtins.dml#L796-L807)

## Detailed Component Analysis

### Object Definitions and Hierarchies
- Object types and containment rules are defined in the language docs. For example, device, bank, register, field, attribute, connect, interface, port, subdevice, implement, event, group are recognized object types with specific allowed parents and children.
- The compiler models these as classes and enforces allowed component types per parent.

```mermaid
classDiagram
class DMLObject {
+ident
+site
+parent
+nongroup_parent
+object_parent
+dimensions
+get_component()
+get_components()
+get_recursive_components()
}
class CompositeObject {
+name
+_components
+_component_dict
+_arraylens
+traits
+templates
+template_method_impls
+add_component()
+accepts_child_type()
}
class Device
class Bank
class Register
class Field
class Attribute
class Connection
class Interface
class Port
class Subdevice
class Implement
class Event
class Group
class Session
class Saved
class Hook
DMLObject <|-- CompositeObject
CompositeObject <|-- Device
CompositeObject <|-- Bank
CompositeObject <|-- Register
CompositeObject <|-- Field
CompositeObject <|-- Attribute
CompositeObject <|-- Connection
CompositeObject <|-- Interface
CompositeObject <|-- Port
CompositeObject <|-- Subdevice
CompositeObject <|-- Implement
CompositeObject <|-- Event
CompositeObject <|-- Group
DMLObject <|-- Session
Session <|-- Saved
DMLObject <|-- Hook
```

**Diagram sources**
- [objects.py](file://py/dml/objects.py#L31-L120)
- [objects.py](file://py/dml/objects.py#L194-L380)
- [objects.py](file://py/dml/objects.py#L317-L563)

**Section sources**
- [language.md](file://doc/1.2/language.md#L314-L330)
- [language.md](file://doc/1.4/language.md#L314-L330)
- [objects.py](file://py/dml/objects.py#L31-L120)

### Composition Patterns and Templates
- Templates provide reusable blocks of methods, parameters, and defaults. Built-in templates include device, bank, register, attribute, and many utility templates for read/write behaviors.
- Utility templates encapsulate common register/field behaviors (e.g., read_only, write_only, constant, reserved, ignore, unmapped, sticky, soft_reset_val).

```mermaid
graph LR
T["Template: device"]
B["Template: bank"]
R["Template: register"]
A["Template: attribute"]
UR["Utility: read_only"]
UW["Utility: write_only"]
UC["Utility: constant"]
UZ["Utility: zeros"]
UO["Utility: ones"]
UI["Utility: ignore"]
US["Utility: soft_reset_val"]
T --> B
B --> R
R --> UR
R --> UW
R --> UC
R --> UZ
R --> UO
R --> UI
R --> US
```

**Diagram sources**
- [dml-builtins.dml](file://lib/1.2/dml-builtins.dml#L199-L270)
- [dml-builtins.dml](file://lib/1.4/dml-builtins.dml#L611-L670)
- [utility.dml](file://lib/1.2/utility.dml#L114-L149)
- [utility.dml](file://lib/1.4/utility.dml#L434-L445)
- [utility.dml](file://lib/1.2/utility.dml#L165-L191)
- [utility.dml](file://lib/1.4/utility.dml#L467-L473)
- [utility.dml](file://lib/1.2/utility.dml#L365-L378)
- [utility.dml](file://lib/1.4/utility.dml#L638-L653)
- [utility.dml](file://lib/1.2/utility.dml#L425-L428)
- [utility.dml](file://lib/1.4/utility.dml#L700-L702)
- [utility.dml](file://lib/1.2/utility.dml#L447-L449)
- [utility.dml](file://lib/1.4/utility.dml#L723-L725)
- [utility.dml](file://lib/1.2/utility.dml#L465-L476)
- [utility.dml](file://lib/1.4/utility.dml#L740-L740)
- [utility.dml](file://lib/1.4/utility.dml#L363-L369)

**Section sources**
- [dml-builtins.dml](file://lib/1.2/dml-builtins.dml#L160-L390)
- [dml-builtins.dml](file://lib/1.4/dml-builtins.dml#L477-L563)
- [utility.dml](file://lib/1.2/utility.dml#L1-L200)
- [utility.dml](file://lib/1.4/utility.dml#L1-L200)

### Inheritance and Method Override Semantics
- Overridable built-in methods are defined by templates. A method can be overridden once by declaring a non-default method with the same signature in the same object.
- Resolution of overrides considers template instantiation and import hierarchy. Conflicting overrides produce errors if ambiguity cannot be resolved.
- Template-qualified method implementation calls allow invoking a specific template’s implementation even when unrelated templates provide competing implementations.

```mermaid
sequenceDiagram
participant Obj as "Object"
participant T1 as "Template A"
participant T2 as "Template B"
participant Impl as "Final Implementation"
Obj->>T1 : "Instantiate template A"
Obj->>T2 : "Instantiate template B"
Obj->>Impl : "Define method override"
Note over Obj,T2 : "Override must be default and match signature"
Obj->>Obj : "Resolve override order (template/import)"
Obj->>Impl : "Call default() to invoke overridden behavior"
Obj->>T1 : "Call templates.T1.m() for specific template impl"
```

**Diagram sources**
- [language.md](file://doc/1.4/language.md#L1754-L1797)
- [language.md](file://doc/1.4/language.md#L3356-L3408)
- [structure.py](file://py/dml/structure.py#L1876-L1928)
- [ctree.py](file://py/dml/ctree.py#L4370-L4401)

**Section sources**
- [language.md](file://doc/1.4/language.md#L1754-L1797)
- [language.md](file://doc/1.4/language.md#L3356-L3408)
- [structure.py](file://py/dml/structure.py#L1876-L1928)
- [ctree.py](file://py/dml/ctree.py#L4370-L4401)

### Object Type System and Polymorphism
- DML supports typed parameters and methods with input/output parameters. Methods can be declared nothrow to allow expression-like invocation.
- Polymorphic behavior is achieved via macro-like methods (parameter types omitted) and template-based composition. Type checking compares signatures and qualifiers rigorously.

```mermaid
flowchart TD
Start(["Method Definition"]) --> CheckSig["Check Input/Output Counts"]
CheckSig --> TypeMatch{"Types Match?"}
TypeMatch --> |No| Error["Raise Type Mismatch Error"]
TypeMatch --> |Yes| Qualifiers{"Qualifiers Match?"}
Qualifiers --> |No| Error
Qualifiers --> |Yes| OK["Method Signature Valid"]
OK --> End(["Ready for Override Resolution"])
```

**Diagram sources**
- [traits.py](file://py/dml/traits.py#L387-L419)
- [types.py](file://py/dml/types.py#L977-L1010)

**Section sources**
- [language.md](file://doc/1.2/language.md#L551-L576)
- [traits.py](file://py/dml/traits.py#L387-L419)
- [types.py](file://py/dml/types.py#L977-L1010)

### Object Lifecycle, Instantiation, and Memory Management
- Device lifecycle includes init, post_init, and destroy. These are automatically invoked on objects implementing the respective templates. Destruction order is defined relative to parent-child relationships.
- Memory management: DML supports dynamic allocation/deallocation via new/delete operators. The destroy template provides a place to release resources not managed by the compiler.

```mermaid
stateDiagram-v2
[*] --> Created
Created --> Initialized : "init()"
Initialized --> PostInitialized : "post_init()"
PostInitialized --> Running
Running --> Destroying : "destroy()"
Destroying --> [*]
```

**Diagram sources**
- [dml-builtins.dml](file://lib/1.2/dml-builtins.dml#L247-L270)
- [dml-builtins.dml](file://lib/1.4/dml-builtins.dml#L664-L670)
- [language.md](file://doc/1.4/language.md#L420-L475)

**Section sources**
- [dml-builtins.dml](file://lib/1.2/dml-builtins.dml#L247-L270)
- [dml-builtins.dml](file://lib/1.4/dml-builtins.dml#L664-L670)
- [language.md](file://doc/1.4/language.md#L420-L475)

### Methods: Signatures, Parameters, Return Values, Exceptions
- Methods accept typed input parameters and return values via output parameters. They support exception handling with try/throw.
- Methods can be declared nothrow to allow expression-like calls. Qualifiers (independent, startup, memoized) influence execution semantics.

```mermaid
sequenceDiagram
participant Caller as "Caller"
participant Obj as "Object.Method"
Caller->>Obj : "call $method(args) -> (outs)"
Obj->>Obj : "Execute body"
alt Throws
Obj-->>Caller : "throw"
else Normal
Obj-->>Caller : "outs assigned"
end
```

**Diagram sources**
- [language.md](file://doc/1.2/language.md#L486-L576)
- [language.md](file://doc/1.4/language.md#L1780-L1797)

**Section sources**
- [language.md](file://doc/1.2/language.md#L486-L576)
- [language.md](file://doc/1.4/language.md#L1780-L1797)

### Attributes, Properties, and State Management
- Attributes expose state to Simics with get/set methods. Parameters control configuration (required/optional/pseudo/none), persistence, and internal visibility.
- Built-in attribute templates provide default get/set implementations and integrate with Simics attribute registration.

```mermaid
classDiagram
class Attribute {
+readable
+writable
+datamember
+get()
+set()
+set_attribute()
+get_attribute()
}
class ConfAttribute {
+configuration
+persistent
+internal
+_attr_type
+_attr_name
+_flags
}
Attribute --|> ConfAttribute
```

**Diagram sources**
- [dml-builtins.dml](file://lib/1.2/dml-builtins.dml#L286-L389)
- [dml-builtins.dml](file://lib/1.4/dml-builtins.dml#L713-L792)

**Section sources**
- [dml-builtins.dml](file://lib/1.2/dml-builtins.dml#L286-L389)
- [dml-builtins.dml](file://lib/1.4/dml-builtins.dml#L713-L792)

### Object Validation, Type Checking, and Runtime Behavior
- The compiler validates method signatures and qualifiers, ensuring input/output counts and types match. It also checks that overriding methods preserve qualifiers and types.
- Runtime behavior is enforced by templates and default methods. For example, bank read/write callbacks and register access methods are provided by built-in templates.

```mermaid
flowchart TD
Parse["Parse Object/Method"] --> Resolve["Resolve Overrides"]
Resolve --> Check["Type Check Signatures"]
Check --> OK{"OK?"}
OK --> |No| Err["Report Error"]
OK --> |Yes| Gen["Generate Bindings"]
Gen --> Runtime["Runtime Behavior via Templates"]
```

**Diagram sources**
- [traits.py](file://py/dml/traits.py#L387-L419)
- [structure.py](file://py/dml/structure.py#L1876-L1928)
- [dml-builtins.dml](file://lib/1.2/dml-builtins.dml#L448-L520)

**Section sources**
- [traits.py](file://py/dml/traits.py#L387-L419)
- [structure.py](file://py/dml/structure.py#L1876-L1928)
- [dml-builtins.dml](file://lib/1.2/dml-builtins.dml#L448-L520)

### Relationship Between DML Objects and Simics Configuration Objects
- The device object maps to a Simics configuration class. Banks, registers, fields, attributes, connects, ports, subdevices, and implements map to Simics constructs.
- Simics types and constants are declared in DML libraries to bridge types and interfaces.

```mermaid
graph TB
DMLDev["DML Device"]
SimDev["Simics Device Class"]
DMLDev --> SimDev
DMLDev --> |"Registers"| SimRegs["Simics Register Attributes"]
DMLDev --> |"Connects"| SimConns["Simics Connection Attributes"]
DMLDev --> |"Ports"| SimPorts["Simics Ports"]
DMLDev --> |"Implements"| SimImpls["Simics Interfaces"]
```

**Diagram sources**
- [language.md](file://doc/1.4/language.md#L285-L293)
- [simics-types.dml](file://lib/1.2/simics-types.dml#L13-L16)
- [simics-configuration.dml](file://lib/1.2/simics-configuration.dml#L8-L15)

**Section sources**
- [language.md](file://doc/1.4/language.md#L285-L293)
- [simics-types.dml](file://lib/1.2/simics-types.dml#L13-L16)
- [simics-configuration.dml](file://lib/1.2/simics-configuration.dml#L8-L15)

## Dependency Analysis
DML’s object model is implemented by a layered design: documentation defines semantics, built-in libraries provide templates, and the compiler enforces structure and type safety.

```mermaid
graph LR
Docs["Language Docs"] --> Libs["Built-in Libraries"]
Libs --> AST["Compiler AST (objects.py)"]
AST --> Struct["Structure & Traits (structure.py, traits.py)"]
Struct --> Types["Type Checker (types.py)"]
Types --> Code["Code Tree (ctree.py)"]
```

**Diagram sources**
- [language.md](file://doc/1.2/language.md#L1-L200)
- [dml-builtins.dml](file://lib/1.2/dml-builtins.dml#L1-L200)
- [objects.py](file://py/dml/objects.py#L1-L200)
- [structure.py](file://py/dml/structure.py#L1-L200)
- [traits.py](file://py/dml/traits.py#L1-L200)
- [types.py](file://py/dml/types.py#L1-L200)
- [ctree.py](file://py/dml/ctree.py#L1-L200)

**Section sources**
- [objects.py](file://py/dml/objects.py#L1-L200)
- [structure.py](file://py/dml/structure.py#L1-L200)
- [traits.py](file://py/dml/traits.py#L1-L200)
- [types.py](file://py/dml/types.py#L1-L200)
- [ctree.py](file://py/dml/ctree.py#L1-L200)

## Performance Considerations
- Template instantiation and method override resolution add compile-time overhead; minimize deep template hierarchies when performance is critical.
- Using nothrow methods enables expression-like calls and reduces overhead in hot paths.
- Avoid excessive dynamic allocations; prefer built-in attributes and registers for frequently accessed state.

## Troubleshooting Guide
Common issues and resolutions:
- Ambiguous method overrides: Ensure only one declaration overrides others; use template-qualified calls to disambiguate.
- Type mismatches: Verify input/output counts and types match; lenient typechecking can relax strictness in specific compat modes.
- Destruction timing: Avoid Simics interactions in destroy; rely on automatic cancellation of events before destroy is invoked.

**Section sources**
- [language.md](file://doc/1.4/language.md#L1754-L1797)
- [traits.py](file://py/dml/traits.py#L387-L419)
- [language.md](file://doc/1.4/language.md#L420-L475)

## Conclusion
DML’s object-oriented model provides a robust, template-driven framework for device modeling. The combination of typed methods, parameterized templates, and strict override/type checking yields predictable behavior and strong composability. The mapping to Simics configuration objects ensures seamless integration with the simulation environment, while lifecycle hooks and attributes enable precise state management and runtime behavior.