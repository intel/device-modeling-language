# Semantic Analysis

<details>
<summary>Relevant source files</summary>

The following files were used as context for generating this wiki page:

- [deprecations_to_md.py](deprecations_to_md.py)
- [py/dml/breaking_changes.py](py/dml/breaking_changes.py)
- [py/dml/crep.py](py/dml/crep.py)
- [py/dml/dmlc.py](py/dml/dmlc.py)
- [py/dml/globals.py](py/dml/globals.py)
- [py/dml/structure.py](py/dml/structure.py)
- [py/dml/template.py](py/dml/template.py)
- [py/dml/toplevel.py](py/dml/toplevel.py)
- [py/dml/traits.py](py/dml/traits.py)
- [py/dml/types.py](py/dml/types.py)

</details>



## Purpose and Scope

Semantic analysis is the middle-end phase of the DML compiler that transforms the Abstract Syntax Tree (AST) from parsing into a semantically validated device object tree. This phase performs type checking, symbol resolution, template instantiation, trait processing, and validates all language constraints. For information about the parsing phase that precedes this, see [Frontend: Parsing and Lexing](#5.2). For information about code generation that follows, see [C Code Generation Backend](#5.5).

The semantic analysis phase consists of two main entry points: `mkglobals()` which processes global declarations, and `mkdev()` which constructs the device object tree. These orchestrate type resolution, template processing, trait resolution, and comprehensive validation.

## Compilation Phase Overview

```mermaid
graph TB
    AST["AST from Parser<br/>(dmlparse.py)"]
    MKGLOBALS["mkglobals()<br/>structure.py:74"]
    MKDEV["mkdev()<br/>structure.py:96"]
    
    subgraph "Global Symbol Collection"
        CONST["Constant Evaluation<br/>structure.py:116-128"]
        EXTERN["extern Declarations<br/>structure.py:176-195"]
        TYPEDEF["Type Definitions<br/>structure.py:206-216"]
        LOGGROUP["Log Groups<br/>structure.py:217-223"]
        TMPL_PROC["process_templates()<br/>template.py:362-432"]
    end
    
    subgraph "Type Resolution"
        EVAL_TYPE["eval_type()<br/>types.py"]
        TYPE_DEPS["type_deps()<br/>structure.py:288-345"]
        SORT_TYPES["sort_type_declarations()<br/>structure.py:352-389"]
    end
    
    subgraph "Device Tree Construction"
        MKOBJ["mkobj()<br/>structure.py"]
        ADD_TMPL["add_templates()<br/>structure.py:525-576"]
        MERGE_PARAMS["merge_parameters()<br/>structure.py:604-709"]
        MERGE_METHODS["sort_method_implementations()<br/>structure.py:818-833"]
    end
    
    subgraph "Trait Processing"
        MKTRAIT["mktrait()<br/>traits.py:294-396"]
        TYPECHECK_TRAIT["typecheck_method_override()<br/>traits.py:398-438"]
        ANCESTRY["calc_minimal_ancestry()<br/>traits.py"]
    end
    
    DEVICE["Device Object Tree<br/>(objects.py)"]
    
    AST --> MKGLOBALS
    MKGLOBALS --> CONST
    MKGLOBALS --> EXTERN
    MKGLOBALS --> TYPEDEF
    MKGLOBALS --> LOGGROUP
    MKGLOBALS --> TMPL_PROC
    
    TYPEDEF --> EVAL_TYPE
    EVAL_TYPE --> TYPE_DEPS
    TYPE_DEPS --> SORT_TYPES
    
    TMPL_PROC --> MKTRAIT
    MKTRAIT --> TYPECHECK_TRAIT
    TYPECHECK_TRAIT --> ANCESTRY
    
    MKGLOBALS --> MKDEV
    MKDEV --> MKOBJ
    MKOBJ --> ADD_TMPL
    ADD_TMPL --> MERGE_PARAMS
    MERGE_PARAMS --> MERGE_METHODS
    
    MERGE_METHODS --> DEVICE
```

**Sources:** [py/dml/structure.py:74-243](), [py/dml/template.py:362-432](), [py/dml/traits.py:294-438](), [py/dml/types.py]()

## Global Symbol Collection

The `mkglobals()` function in `structure.py` processes all top-level declarations and populates the global symbol table. This phase must execute before object tree construction because objects may reference global symbols.

### Processing Pipeline

```mermaid
graph TB
    STMTS["Top-level Statements<br/>from AST"]
    
    subgraph "Symbol Classification"
        CLASSIFY["Classify by Kind<br/>structure.py:80-84"]
        NAMECOLL["Check Name Collisions<br/>structure.py:86-112"]
    end
    
    subgraph "Constant Processing"
        CONST_EVAL["codegen_expression_maybe_nonvalue()<br/>structure.py:120-121"]
        CONST_CHECK["check_constant_expr()<br/>structure.py:52-61"]
        CONST_ADD["Add to global_scope<br/>structure.py:124"]
    end
    
    subgraph "Type Processing"
        EXTERN_TYPE["extern typedef<br/>structure.py:197-205"]
        DML_TYPE["dml typedef<br/>structure.py:206-216"]
        EVAL_TYPE["eval_type()<br/>types.py"]
        ANON_STRUCT["Anonymous Structs<br/>structure.py:134,211-213"]
    end
    
    subgraph "Template Processing"
        TMPL_SPLIT["Split Method/Param/Trait<br/>structure.py:142-175"]
        PROC_TMPL["process_templates()<br/>template.py:362"]
        TRAIT_TYPE["Create Trait Types<br/>structure.py:237-242"]
    end
    
    TOPSORT["sort_type_declarations()<br/>structure.py:352"]
    TYPECHECK["Type Validation<br/>structure.py:250-271"]
    GLOBAL_SCOPE["Global Symbol Table<br/>symtab.global_scope"]
    
    STMTS --> CLASSIFY
    CLASSIFY --> NAMECOLL
    
    NAMECOLL --> CONST_EVAL
    CONST_EVAL --> CONST_CHECK
    CONST_CHECK --> CONST_ADD
    
    NAMECOLL --> EXTERN_TYPE
    NAMECOLL --> DML_TYPE
    EXTERN_TYPE --> EVAL_TYPE
    DML_TYPE --> EVAL_TYPE
    EVAL_TYPE --> ANON_STRUCT
    
    NAMECOLL --> TMPL_SPLIT
    TMPL_SPLIT --> PROC_TMPL
    PROC_TMPL --> TRAIT_TYPE
    
    CONST_ADD --> GLOBAL_SCOPE
    EVAL_TYPE --> TOPSORT
    TOPSORT --> TYPECHECK
    TYPECHECK --> GLOBAL_SCOPE
    TRAIT_TYPE --> GLOBAL_SCOPE
```

**Sources:** [py/dml/structure.py:74-287](), [py/dml/template.py:362-432]()

### Name Collision Resolution

The compiler handles various collision scenarios between global symbols:

| Collision Type | DML 1.2 Behavior | DML 1.4 Behavior | Code Reference |
|----------------|------------------|------------------|----------------|
| Multiple `extern foo;` | Redundant declarations dropped | Error reported | [py/dml/structure.py:63-93]() |
| Type vs. Value | Allowed (separate namespaces) | Allowed | [py/dml/structure.py:96-100]() |
| Duplicate `extern` with types | Type checked for compatibility | Type checked | [py/dml/structure.py:137-139,277-286]() |
| Non-extern duplicates | Error (ENAMECOLL) | Error (ENAMECOLL) | [py/dml/structure.py:106-112]() |

**Sources:** [py/dml/structure.py:63-112](), [py/dml/structure.py:277-286]()

### Constant Evaluation

Constants are evaluated during global symbol collection to enable their use in type definitions and struct declarations. The evaluation process:

1. Parse constant expression AST
2. Call `codegen_expression_maybe_nonvalue()` to evaluate in global scope
3. Verify the result is a proper constant value (not a non-value like a list of non-values)
4. Check that `expr.constant` is `True`
5. Add to `global_scope` as an `ExpressionSymbol`

**Sources:** [py/dml/structure.py:116-128](), [py/dml/structure.py:52-61]()

## Type System Resolution

The type system processes type definitions and resolves type dependencies to ensure correct declaration order in generated C code.

### Type Resolution Functions

```mermaid
graph TB
    NAMED["TNamed<br/>(Type Reference)"]
    
    subgraph "Resolution Functions"
        REALTYPE_SHALLOW["realtype_shallow()<br/>types.py:121-141"]
        REALTYPE["realtype()<br/>types.py:143-178"]
        SAFE_REALTYPE["safe_realtype()<br/>types.py:180-184"]
        SAFE_UNCONST["safe_realtype_unconst()<br/>types.py:202-214"]
    end
    
    subgraph "Type Tables"
        TYPEDEFS["typedefs dict<br/>types.py:88"]
        GLOBAL_ORDER["global_type_declaration_order<br/>types.py:88"]
        ANON_STRUCTS["global_anonymous_structs<br/>types.py:90"]
    end
    
    CONCRETE["Concrete Type<br/>(TInt, TStruct, etc.)"]
    
    NAMED --> REALTYPE_SHALLOW
    REALTYPE_SHALLOW --> TYPEDEFS
    TYPEDEFS --> CONCRETE
    
    REALTYPE_SHALLOW --> REALTYPE
    REALTYPE --> CONCRETE
    
    REALTYPE --> SAFE_REALTYPE
    SAFE_REALTYPE --> CONCRETE
    
    CONCRETE --> SAFE_UNCONST
```

**Sources:** [py/dml/types.py:86-214]()

### Type Dependency Analysis

The compiler analyzes dependencies between type definitions to determine correct declaration order. Type dependencies matter for:

- **Struct members and array types**: Require complete type definitions (not just forward declarations)
- **Pointer types**: Only require forward declarations
- **Function types**: Parameters and return types only need forward declarations

```mermaid
graph LR
    TYPE["Type Definition"]
    
    subgraph "Dependency Analysis"
        TYPE_DEPS["type_deps()<br/>structure.py:288"]
        INCLUDE_STRUCTS{"include_structs"}
        TNamed["TNamed?"]
        TStruct["TStruct?"]
        TArray["TArray?"]
        TPtr["TPtr?"]
        TFunction["TFunction?"]
    end
    
    FULL_DEPS["Full Dependencies<br/>(struct members, arrays)"]
    FWD_DEPS["Forward-decl Only<br/>(pointers, functions)"]
    
    TYPE --> TYPE_DEPS
    TYPE_DEPS --> TNamed
    TYPE_DEPS --> TStruct
    TYPE_DEPS --> TArray
    TYPE_DEPS --> TPtr
    TYPE_DEPS --> TFunction
    
    TNamed --> INCLUDE_STRUCTS
    TStruct --> FULL_DEPS
    TArray --> FULL_DEPS
    TPtr --> FWD_DEPS
    TFunction --> FWD_DEPS
    
    INCLUDE_STRUCTS -->|Yes| FULL_DEPS
    INCLUDE_STRUCTS -->|No| FWD_DEPS
```

**Sources:** [py/dml/structure.py:288-345]()

### Type Declaration Ordering

The `sort_type_declarations()` function performs topological sorting of types to ensure dependencies appear before dependents:

1. Build dependency graph using `type_deps()`
2. Perform topological sort via `topsort.topsort()`
3. Detect circular dependencies and report ETREC error
4. Return ordered list for C code generation

**Sources:** [py/dml/structure.py:352-389]()

## Template Processing

Templates are processed to establish their inheritance hierarchy and create `Template` objects with their associated `ObjectSpec` specifications.

### Template Hierarchy Analysis

```mermaid
graph TB
    DECLS["Template Declarations<br/>(site, body_asts, trait_asts)"]
    
    subgraph "Rank Analysis"
        RANK_STRUCT["rank_structure()<br/>template.py:311-360"]
        INFERIOR["Inferior Templates<br/>(parent references)"]
        UNCOND["Unconditional References"]
        IN_EACH["'in each' Structure"]
    end
    
    subgraph "Dependency Resolution"
        REQ_TMPL["required_templates<br/>template.py:369-376"]
        MISSING["Missing Templates<br/>template.py:376-388"]
        TOPSORT["topsort.topsort()<br/>template.py:389-406"]
    end
    
    subgraph "Spec Creation"
        OBJ_SPEC["object_spec_from_asts()<br/>template.py:250-309"]
        RANK["Rank Objects<br/>template.py:47-62"]
        FLATTEN["flatten_ifs()<br/>template.py:219-248"]
    end
    
    TRAIT_PROC["process_trait()<br/>traits.py:36-115"]
    TEMPLATES["Template Objects<br/>template.py:139-157"]
    
    DECLS --> RANK_STRUCT
    RANK_STRUCT --> INFERIOR
    RANK_STRUCT --> UNCOND
    RANK_STRUCT --> IN_EACH
    
    INFERIOR --> REQ_TMPL
    REQ_TMPL --> MISSING
    MISSING --> TOPSORT
    
    TOPSORT --> OBJ_SPEC
    OBJ_SPEC --> RANK
    OBJ_SPEC --> FLATTEN
    
    OBJ_SPEC --> TRAIT_PROC
    OBJ_SPEC --> TEMPLATES
    TRAIT_PROC --> TEMPLATES
```

**Sources:** [py/dml/template.py:311-432](), [py/dml/traits.py:36-115]()

### Rank System

The `Rank` class establishes override precedence between templates. Each `ObjectSpec` has an associated rank that determines which declaration wins when multiple templates define the same parameter or method.

| Rank Component | Description | Code Reference |
|----------------|-------------|----------------|
| `inferior` | Set of ranks this rank overrides | [py/dml/template.py:52-57]() |
| `desc` | Human-readable description (`RankDesc`) | [py/dml/template.py:58-59]() |
| `RankDesc.kind` | One of: 'file', 'template', 'verbatim' | [py/dml/template.py:28]() |
| `RankDesc.text` | Template name or file name | [py/dml/template.py:29]() |
| `RankDesc.in_eachs` | Nested 'in each' block structure | [py/dml/template.py:32-33]() |

**Sources:** [py/dml/template.py:24-62]()

### ObjectSpec Structure

An `ObjectSpec` represents a partial specification of a DML object, containing:

- **site**: Source location where the spec is defined
- **rank**: `Rank` object for override precedence
- **templates**: List of `(site, Template)` pairs for instantiated templates
- **in_eachs**: List of `(templates, ObjectSpec)` for 'in each' statements
- **params**: List of `ast.param` nodes
- **blocks**: Conditional blocks as `(preconds, shallow_stmts, composite_stmts)` tuples

**Sources:** [py/dml/template.py:64-128]()

## Device Tree Construction

The `mkdev()` function constructs the device object tree by instantiating templates, merging parameters, and creating DML object instances.

### Object Creation Pipeline

```mermaid
graph TB
    DEV_NAME["Device Name"]
    OBJ_SPECS["ObjectSpec List"]
    
    subgraph "Template Instantiation"
        ADD_TMPL["add_templates()<br/>structure.py:525"]
        QUEUE["Process Template Queue<br/>structure.py:529-571"]
        WRAP["wrap_sites()<br/>structure.py:464-512"]
        USED["used_templates dict"]
    end
    
    subgraph "Object Instantiation"
        MKOBJ["mkobj()<br/>structure.py"]
        EVAL_COND["Evaluate Conditions<br/>structure.py"]
        MERGE_SUB["merge_subobj_defs()<br/>structure.py:835-873"]
    end
    
    subgraph "Component Creation"
        MKDATA["mkdata()<br/>structure.py:885-900"]
        MKSAVED["mksaved()<br/>structure.py:902-922"]
        MKHOOK["mkhook()<br/>structure.py:924-943"]
        MKMETHOD["mkmethod()"]
    end
    
    subgraph "Parameter Resolution"
        MERGE_PARAMS["merge_parameters()<br/>structure.py:604-709"]
        PARAM_DML12["merge_parameters_dml12()<br/>structure.py:578-602"]
        AUTO["Handle 'auto' Params<br/>structure.py:611-621"]
        RANK_CHECK["Check Rank Precedence<br/>structure.py:647-662"]
    end
    
    DEVICE["Device Object<br/>objects.Device"]
    
    DEV_NAME --> MKOBJ
    OBJ_SPECS --> ADD_TMPL
    ADD_TMPL --> QUEUE
    QUEUE --> WRAP
    WRAP --> USED
    
    USED --> MKOBJ
    MKOBJ --> EVAL_COND
    EVAL_COND --> MERGE_SUB
    
    MERGE_SUB --> MKDATA
    MERGE_SUB --> MKSAVED
    MERGE_SUB --> MKHOOK
    MERGE_SUB --> MKMETHOD
    
    MKOBJ --> MERGE_PARAMS
    MERGE_PARAMS --> PARAM_DML12
    MERGE_PARAMS --> AUTO
    MERGE_PARAMS --> RANK_CHECK
    
    MKOBJ --> DEVICE
```

**Sources:** [py/dml/structure.py:525-873](), [py/dml/structure.py:885-943]()

### Template Instantiation

The `add_templates()` function expands template references by:

1. Maintaining a queue of `(site, Template)` pairs to process
2. For each template, wrapping its `ObjectSpec` with instantiation context via `wrap_sites()`
3. Processing 'in each' statements by checking if all referenced templates are instantiated
4. Building `used_templates` dict mapping `Template` to `ObjectSpec`

**Sources:** [py/dml/structure.py:525-576]()

### Parameter Merging

Parameter merging resolves which parameter definition to use when multiple templates define the same parameter:

```mermaid
graph TB
    PARAMS["List of (Rank, ast.param)<br/>from all templates"]
    
    AUTO{"'auto' declaration?"}
    DML12{"DML 1.2?"}
    NONDEFAULT{"Single non-default?"}
    
    CLASSIFY["Classify as decl/def<br/>structure.py:628-639"]
    DECLS["Declarations<br/>(type but no value)"]
    DEFS["Definitions<br/>(has value)"]
    
    SUPERIOR["Find Superior Ranks<br/>structure.py:646-662"]
    AMBIG{"Multiple superior?"}
    
    RESULT["Selected Parameter"]
    ERROR["Error Report"]
    
    PARAMS --> AUTO
    AUTO -->|Yes| RESULT
    AUTO -->|No| DML12
    
    DML12 -->|Yes| NONDEFAULT
    NONDEFAULT -->|Yes| RESULT
    NONDEFAULT -->|No| CLASSIFY
    
    DML12 -->|No| CLASSIFY
    CLASSIFY --> DECLS
    CLASSIFY --> DEFS
    
    DEFS --> SUPERIOR
    SUPERIOR --> AMBIG
    AMBIG -->|Yes| ERROR
    AMBIG -->|No| RESULT
```

**Sources:** [py/dml/structure.py:604-709]()

### Method Override Type Checking

The `typecheck_method_override()` function validates that method overrides have compatible signatures:

- Input parameter count and types must match
- Output parameter count and types must match
- Qualifiers (`independent`, `startup`, `memoized`) must match
- `throws` annotations must match
- Type comparison uses `eq_fuzzy()` unless `strict_typechecking` is enabled

**Sources:** [py/dml/structure.py:711-805]()

## Trait System Integration

Traits provide polymorphism and shared method definitions. The trait system processes trait declarations and builds vtables for runtime dispatch.

### Trait Creation Process

```mermaid
graph TB
    SITE["Trait Site"]
    NAME["Trait Name"]
    ANCESTORS["Ancestor Traits"]
    METHODS["Method Declarations"]
    PARAMS["Parameter Declarations"]
    
    subgraph "Validation"
        CHECK_COLL["Check Name Collisions<br/>traits.py:42-54"]
        PARENT_CHECK["Check Parent Collisions<br/>traits.py:304-326"]
        METHOD_CHECK["Validate Method Overrides<br/>traits.py:328-367"]
    end
    
    subgraph "Vtable Construction"
        MERGE_VTABLES["merge_ancestor_vtables()<br/>traits.py:277-292"]
        MERGE_IMPLS["merge_method_impl_maps()<br/>traits.py:440-503"]
    end
    
    subgraph "Trait Object"
        TRAIT["Trait<br/>traits.py"]
        METHOD_IMPLS["method_impls dict"]
        VTABLE_METHODS["vtable_methods"]
        VTABLE_PARAMS["vtable_params"]
        ANCESTRY["ancestry_paths"]
    end
    
    SITE --> CHECK_COLL
    NAME --> CHECK_COLL
    ANCESTORS --> PARENT_CHECK
    METHODS --> METHOD_CHECK
    PARAMS --> PARENT_CHECK
    
    CHECK_COLL --> MERGE_VTABLES
    PARENT_CHECK --> MERGE_VTABLES
    METHOD_CHECK --> MERGE_VTABLES
    
    MERGE_VTABLES --> MERGE_IMPLS
    MERGE_IMPLS --> TRAIT
    
    TRAIT --> METHOD_IMPLS
    TRAIT --> VTABLE_METHODS
    TRAIT --> VTABLE_PARAMS
    TRAIT --> ANCESTRY
```

**Sources:** [py/dml/traits.py:36-396](), [py/dml/traits.py:277-292](), [py/dml/traits.py:440-503]()

### Method Resolution

When multiple traits provide implementations of the same method, the compiler determines precedence using the partial order defined by trait inheritance:

1. Collect all implementations from ancestor traits
2. Filter out implementations overridden by others
3. If multiple unrelated implementations remain, check if all are overridable
4. Report ambiguity error (EAMBINH) if non-overridable implementations conflict

**Sources:** [py/dml/traits.py:440-503]()

### TraitMethod Structure

A `TraitMethod` represents a method implementation in a trait:

| Field | Description | Code Reference |
|-------|-------------|----------------|
| `site` | Source location | [py/dml/traits.py:155]() |
| `inp` | Input parameters | [py/dml/traits.py:156]() |
| `outp` | Output parameters | [py/dml/traits.py:157]() |
| `throws` | Exception specification | [py/dml/traits.py:158]() |
| `independent` | Independent method flag | [py/dml/traits.py:159]() |
| `startup` | Startup method flag | [py/dml/traits.py:160]() |
| `memoized` | Memoized method flag | [py/dml/traits.py:161]() |
| `overridable` | Can be overridden flag | [py/dml/traits.py:162]() |
| `astbody` | Method body AST | [py/dml/traits.py:163]() |
| `trait` | Parent trait | [py/dml/traits.py:164]() |
| `name` | Method name | [py/dml/traits.py:165]() |
| `default_traits` | Traits being overridden | [py/dml/traits.py:167]() |

**Sources:** [py/dml/traits.py:147-189]()

## Type Checking and Validation

After the device tree is constructed, the compiler performs comprehensive validation including type checking, unused symbol detection, and constraint verification.

### Validation Categories

```mermaid
graph TB
    DEVICE["Device Object Tree"]
    
    subgraph "Type Validation"
        NAMED_TYPES["check_named_types()<br/>types.py:92-118"]
        REALTYPE_CHECK["Verify type resolution<br/>structure.py:250-271"]
        EXTERN_COMPAT["Extern type compatibility<br/>structure.py:277-286"]
    end
    
    subgraph "Usage Validation"
        UNUSED["check_unused_and_warn()<br/>structure.py:449-462"]
        UNUSED_DEFAULT["is_unused_default()<br/>structure.py:402-422"]
        DML12_METHODS["is_dml12_method()<br/>structure.py:442-447"]
    end
    
    subgraph "Constraint Validation"
        ARRAYLEN["Array length validation"]
        SUBOBJ_COMPAT["merge_subobj_defs()<br/>structure.py:835-873"]
        CONST_DEPTH["deep_const() check<br/>types.py:223-238"]
    end
    
    ERRORS["Error/Warning Reports"]
    
    DEVICE --> NAMED_TYPES
    DEVICE --> UNUSED
    DEVICE --> ARRAYLEN
    
    NAMED_TYPES --> REALTYPE_CHECK
    REALTYPE_CHECK --> EXTERN_COMPAT
    
    UNUSED --> UNUSED_DEFAULT
    UNUSED --> DML12_METHODS
    
    ARRAYLEN --> SUBOBJ_COMPAT
    SUBOBJ_COMPAT --> CONST_DEPTH
    
    EXTERN_COMPAT --> ERRORS
    UNUSED_DEFAULT --> ERRORS
    SUBOBJ_COMPAT --> ERRORS
```

**Sources:** [py/dml/structure.py:449-462](), [py/dml/types.py:92-118](), [py/dml/structure.py:835-873]()

### Unused Symbol Detection

The compiler warns about unused methods and parameters that are defined but never referenced. Special cases include:

- **Unused field methods**: Methods like `after_read`, `before_write` in fields that only affect registers
- **Unused register methods**: Override of `read`/`write` in registers with fields
- **DML 1.2 methods**: Methods like `read_access`, `write_access` that are DML 1.2-specific

**Sources:** [py/dml/structure.py:391-460]()

## Symbol Resolution

The semantic analysis phase establishes symbol tables at multiple scopes and resolves all identifier references.

### Symbol Table Hierarchy

```mermaid
graph TB
    GLOBAL["global_scope<br/>(symtab.py)"]
    
    subgraph "Global Symbols"
        CONSTANTS["Constants<br/>ExpressionSymbol"]
        EXTERNS["Externs<br/>LiteralSymbol"]
        LOGGROUPS["Log Groups<br/>ExpressionSymbol"]
        TYPEDEFS["Type Definitions<br/>typedefs dict"]
        TEMPLATES["Templates<br/>dml.globals.templates"]
    end
    
    subgraph "Template Scope"
        TMPL_SCOPE["Template.scope()<br/>traits.py"]
        TRAIT_SYMBOLS["Trait Methods/Params"]
        RESERVED["reserved_symbols"]
    end
    
    subgraph "Object Scope"
        OBJ_SCOPE["Object.local_scope()"]
        PARAMS["Parameters"]
        METHODS["Methods"]
        SUBOBJS["Subobjects"]
    end
    
    subgraph "Method Scope"
        METHOD_SCOPE["MethodParamScope"]
        INARGS["Input Arguments"]
        DEFAULT["'default' Symbol"]
    end
    
    GLOBAL --> CONSTANTS
    GLOBAL --> EXTERNS
    GLOBAL --> LOGGROUPS
    GLOBAL --> TYPEDEFS
    GLOBAL --> TEMPLATES
    
    TEMPLATES --> TMPL_SCOPE
    TMPL_SCOPE --> TRAIT_SYMBOLS
    TMPL_SCOPE --> RESERVED
    
    OBJ_SCOPE --> PARAMS
    OBJ_SCOPE --> METHODS
    OBJ_SCOPE --> SUBOBJS
    
    METHOD_SCOPE --> INARGS
    METHOD_SCOPE --> DEFAULT
    
    TMPL_SCOPE --> OBJ_SCOPE
    OBJ_SCOPE --> METHOD_SCOPE
```

**Sources:** [py/dml/structure.py:74-230](), [py/dml/traits.py:122-137]()

### Special Symbols

Several special symbols have unique resolution behavior:

| Symbol | Context | Resolution | Code Reference |
|--------|---------|------------|----------------|
| `default` | Method override | Previous method implementation | [py/dml/traits.py:230-246]() |
| `dev` | Template scope | Reserved, disallowed in typed params | [py/dml/traits.py:394]() |
| `$` prefix | Object reference | Current object in hierarchy | [py/dml/objects.py]() |

**Sources:** [py/dml/traits.py:122-246](), [py/dml/traits.py:384-396]()

## Error Handling and Reporting

Semantic analysis reports errors through the messaging system with specific error codes for different validation failures.

### Common Error Categories

| Error Code | Description | Reported By | Code Reference |
|------------|-------------|-------------|----------------|
| ENAMECOLL | Name collision | mkglobals, mktrait | [py/dml/structure.py:108](), [py/dml/traits.py:43-54]() |
| ETYPE | Invalid type reference | Type resolution | [py/dml/types.py:96]() |
| ENTMPL | Nonexistent template | Template instantiation | [py/dml/structure.py:541]() |
| EAMBINH | Ambiguous inheritance | Parameter/method merge | [py/dml/structure.py:660-662]() |
| EMETH | Method override mismatch | Type checking | [py/dml/structure.py:727](), [py/dml/traits.py:401-424]() |
| ENPARAM | Missing parameter definition | Parameter resolution | [py/dml/structure.py:644]() |
| ETREC | Recursive type definition | Type sorting | [py/dml/structure.py:377-379]() |
| ECYCLICTEMPLATE | Cyclic template inheritance | Template processing | [py/dml/template.py:400-401]() |

**Sources:** [py/dml/structure.py:108-809](), [py/dml/traits.py:36-438](), [py/dml/template.py:389-406]()

## Integration with Code Generation

The semantic analysis phase produces validated data structures consumed by the code generation backend:

- **Device object tree**: Hierarchical structure in `dml.globals.device`
- **Type declarations**: Ordered list in `global_type_declaration_order`
- **Trait vtables**: Method implementations in `trait.method_impls`
- **Template instances**: Used templates tracking for unused warnings
- **Symbol tables**: For expression code generation

The code generation phase (see [C Code Generation Backend](#5.5)) uses these structures to emit C code, while the debug backend (see [Runtime Support](#5.6)) uses them to generate debugging information.

**Sources:** [py/dml/structure.py:1-50](), [py/dml/dmlc.py:72-96](), [py/dml/types.py:86-90]()