# Getting Started

<details>
<summary>Relevant source files</summary>

The following files were used as context for generating this wiki page:

- [MODULEINFO](MODULEINFO)
- [Makefile](Makefile)
- [deprecations_to_md.py](deprecations_to_md.py)
- [lib/1.2/dml-builtins.dml](lib/1.2/dml-builtins.dml)
- [lib/1.4/dml-builtins.dml](lib/1.4/dml-builtins.dml)
- [md_to_github.py](md_to_github.py)
- [py/dml/breaking_changes.py](py/dml/breaking_changes.py)
- [py/dml/dmlc.py](py/dml/dmlc.py)
- [py/dml/globals.py](py/dml/globals.py)
- [py/dml/toplevel.py](py/dml/toplevel.py)
- [validate_md_links.py](validate_md_links.py)

</details>



This page provides an introduction to using the DML (Device Modeling Language) compiler to create Simics device models. It covers installation, basic compilation commands, and creating a minimal device model. For detailed language reference, see [DML Language Reference](#3). For information about the standard library templates, see [Standard Library](#4).

## Overview of DML and the Compiler

DML is a domain-specific language for modeling hardware devices in the Simics simulator. The DML compiler (`dmlc`) translates `.dml` source files into C code that integrates with the Simics API. The compiler is implemented in Python and generates C code that links against the `dmllib.h` runtime library.

**Key Architecture Components:**

```mermaid
graph LR
    DML["DML Source<br/>device.dml"]
    DMLC["dmlc compiler<br/>py/dml/dmlc.py"]
    C["Generated C<br/>device.c, device.h"]
    GCC["GCC"]
    SO["Simics Module<br/>device.so"]
    SIMICS["Simics Simulator"]
    
    DML --> DMLC
    DMLC --> C
    C --> GCC
    GCC --> SO
    SO --> SIMICS
    
    LIB["Standard Library<br/>lib/1.4/dml-builtins.dml"]
    DMLLIB["Runtime Library<br/>include/simics/dmllib.h"]
    
    LIB -.-> DMLC
    DMLLIB -.-> C
```

Sources: [py/dml/dmlc.py:1-811](), [lib/1.4/dml-builtins.dml:1-100](), [Makefile:1-251]()

## Prerequisites

To use the DML compiler, you need:

- Python 3 with UTF-8 mode enabled (required by [py/dml/dmlc.py:310-311]())
- GCC or compatible C compiler
- Simics installation with appropriate API version
- Access to DML standard library files

The compiler supports multiple Simics API versions (4.8, 5, 6, 7) as defined in [py/dml/breaking_changes.py:14-24]().

## Building the Compiler

The DML compiler is built as part of the Simics module system. The build process:

1. **Python Module Compilation**: Python source files from `py/dml/` are compiled and installed to `$(HOST)/bin/dml/python/dml/` using `copy_python.py` [Makefile:102-110]().

2. **Parser Table Generation**: Grammar tables are generated for DML 1.2 and 1.4 using PLY (Python Lex-Yacc) [Makefile:123-129]().

3. **Standard Library Copying**: DML library files from `lib/1.2/` and `lib/1.4/` are copied to installation directories [Makefile:148-158]().

4. **Runtime Header Installation**: The `dmllib.h` header is processed and installed [Makefile:115-116]().

**Build Directory Structure:**

```mermaid
graph TB
    subgraph "Source Tree"
        SRC_PY["py/dml/*.py<br/>Compiler implementation"]
        SRC_LIB12["lib/1.2/*.dml<br/>DML 1.2 stdlib"]
        SRC_LIB14["lib/1.4/*.dml<br/>DML 1.4 stdlib"]
        SRC_H["include/simics/dmllib.h<br/>Runtime library"]
    end
    
    subgraph "Build Output"
        OUT_PY["HOST/bin/dml/python/dml/<br/>Compiled Python"]
        OUT_PARSE["HOST/bin/dml/python/dml/<br/>dml12_parsetab.py<br/>dml14_parsetab.py"]
        OUT_LIB["HOST/bin/dml/1.2/<br/>HOST/bin/dml/1.4/<br/>Standard libraries"]
        OUT_H["HOST/bin/dml/include/simics/<br/>dmllib.h"]
    end
    
    SRC_PY --> OUT_PY
    SRC_PY --> OUT_PARSE
    SRC_LIB12 --> OUT_LIB
    SRC_LIB14 --> OUT_LIB
    SRC_H --> OUT_H
```

Sources: [Makefile:88-97](), [MODULEINFO:28-73]()

## Command Line Interface

The compiler is invoked through `dmlc`, which is implemented as `__main__.py` calling into [py/dml/dmlc.py:308-811](). The main entry point is `main(argv)` at [py/dml/dmlc.py:308]().

**Basic Syntax:**
```
dmlc [options] input_file.dml [output_base]
```

**Essential Options:**

| Option | Description | Implementation |
|--------|-------------|----------------|
| `-I PATH` | Add import search path | [py/dml/dmlc.py:326-330]() |
| `-D NAME=VALUE` | Define compile-time parameter | [py/dml/dmlc.py:337-341]() |
| `--simics-api=VERSION` | Specify Simics API version | [py/dml/dmlc.py:441-446]() |
| `-g` | Generate debug artifacts | [py/dml/dmlc.py:382-384]() |
| `--warn=TAG` | Enable specific warning | [py/dml/dmlc.py:389-393]() |
| `--nowarn=TAG` | Disable specific warning | [py/dml/dmlc.py:398-402]() |
| `--werror` | Treat warnings as errors | [py/dml/dmlc.py:408-409]() |

The `-D` option parsing is handled by `parse_define()` at [py/dml/dmlc.py:116-147](), which accepts string, integer, float, and boolean literals.

Sources: [py/dml/dmlc.py:308-511]()

## Compilation Pipeline

The compilation process follows these stages as orchestrated by `main()` in [py/dml/dmlc.py:308-811]():

```mermaid
graph TB
    START["dmlc invoked"]
    PARSE_ARGS["Parse command line<br/>argparse"]
    PARSE_FILE["parse_main_file()<br/>py/dml/toplevel.py:359"]
    VERSION["determine_version()<br/>Check 'dml 1.4;' statement"]
    LEX["Lexical analysis<br/>dmllex12.py/dmllex14.py"]
    YACC["Parse to AST<br/>dmlparse.py"]
    SCAN["scan_statements()<br/>Process imports/headers"]
    MKGLOBALS["mkglobals()<br/>py/dml/structure.py"]
    MKDEV["mkdev()<br/>Build device tree"]
    CODEGEN["c_backend.generate()<br/>py/dml/c_backend.py"]
    OUTPUT["Write .c, .h files"]
    
    START --> PARSE_ARGS
    PARSE_ARGS --> PARSE_FILE
    PARSE_FILE --> VERSION
    VERSION --> LEX
    LEX --> YACC
    YACC --> SCAN
    SCAN --> MKGLOBALS
    MKGLOBALS --> MKDEV
    MKDEV --> CODEGEN
    CODEGEN --> OUTPUT
    
    IMPORTS["Import resolution<br/>import_file()"]
    SCAN --> IMPORTS
    IMPORTS --> LEX
```

**Key Functions:**

- `parse_main_file()` [py/dml/toplevel.py:359-459]() - Entry point for parsing
- `determine_version()` [py/dml/toplevel.py:66-112]() - Extracts DML version from source
- `parse()` [py/dml/toplevel.py:114-127]() - Invokes PLY parser
- `scan_statements()` [py/dml/toplevel.py:129-186]() - Categorizes top-level statements
- `process()` [py/dml/dmlc.py:72-96]() - Creates device structure
- `generate()` [dml/c_backend.py] - Produces C code

Sources: [py/dml/dmlc.py:691-759](), [py/dml/toplevel.py:359-459]()

## Language Version Selection

DML supports two major versions: 1.2 (legacy) and 1.4 (modern). Each file must begin with a version statement:

```dml
dml 1.4;
```

The version statement is parsed by regex patterns in [py/dml/toplevel.py:34-40]():
- `has_version` matches the `dml` keyword
- `check_version` extracts major and minor version numbers

Version-specific lexers and parsers are selected in `get_parser()` [py/dml/toplevel.py:48-64](). The global version is stored in `dml.globals.dml_version` [py/dml/globals.py:29]().

**Version-Specific Parsers:**

```mermaid
graph LR
    SOURCE["DML Source"]
    CHECK["determine_version()<br/>toplevel.py:66"]
    
    subgraph "DML 1.2"
        LEX12["dmllex12.py"]
        PARSE12["dmlparse grammars[(1,2)]"]
    end
    
    subgraph "DML 1.4"
        LEX14["dmllex14.py"]
        PARSE14["dmlparse grammars[(1,4)]"]
    end
    
    AST["Abstract Syntax Tree"]
    
    SOURCE --> CHECK
    CHECK -->|"(1, 2)"| LEX12
    CHECK -->|"(1, 4)"| LEX14
    LEX12 --> PARSE12
    LEX14 --> PARSE14
    PARSE12 --> AST
    PARSE14 --> AST
```

Sources: [py/dml/toplevel.py:31-112](), [py/dml/globals.py:28-29]()

## Standard Library Import

All DML files implicitly import `dml-builtins.dml` [py/dml/toplevel.py:376-377](), which provides:

- Core templates: `device`, `bank`, `register`, `field`, `attribute`, `connect`, `event`, `port`, `group`
- Lifecycle templates: `init`, `post_init`, `destroy`
- Universal templates: `name`, `desc`, `documentation`, `object`

The standard library location is determined by adding versioned subdirectories to the import path [py/dml/toplevel.py:389-393]():
```python
import_path = [
    path
    for orig_path in explicit_import_path + [os.path.dirname(inputfilename)]
    for path in [os.path.join(orig_path, version_str), orig_path]]
```

**Import Resolution:**

| DML Version | Library Path | Key Files |
|-------------|--------------|-----------|
| 1.2 | `HOST/bin/dml/1.2/` | `dml-builtins.dml`, `utility.dml`, `simics-api.dml` |
| 1.4 | `HOST/bin/dml/1.4/` | `dml-builtins.dml`, `utility.dml`, `dml12-compatibility.dml` |

Sources: [py/dml/toplevel.py:376-393](), [lib/1.4/dml-builtins.dml:1-50](), [MODULEINFO:95-114]()

## Creating Your First Device Model

A minimal DML 1.4 device consists of:

```dml
dml 1.4;

device simple_device;

bank regs {
    register control @ 0x00 size 4 {
        field enable @ [0];
    }
    register status @ 0x04 size 4 is read_only;
}
```

**Structure Breakdown:**

1. **Version Declaration**: `dml 1.4;` - Required at file start [py/dml/toplevel.py:85-94]()
2. **Device Declaration**: `device simple_device;` - Creates the device object with `classname` parameter [lib/1.4/dml-builtins.dml:626-722]()
3. **Bank Object**: `bank regs` - Memory-mapped register bank [lib/1.4/dml-builtins.dml:1829-2215]()
4. **Register Objects**: Declared with address (`@`) and size [lib/1.4/dml-builtins.dml:2497-2917]()
5. **Field Objects**: Bit fields within registers [lib/1.4/dml-builtins.dml:3042-3241]()

**Object Hierarchy:**

```mermaid
graph TB
    DEVICE["device simple_device<br/>Template: device<br/>lib/1.4/dml-builtins.dml:626"]
    BANK["bank regs<br/>Template: bank<br/>lib/1.4/dml-builtins.dml:1829"]
    REG1["register control @ 0x00<br/>Template: register"]
    REG2["register status @ 0x04<br/>Template: register"]
    FIELD["field enable @ [0]<br/>Template: field"]
    
    DEVICE --> BANK
    BANK --> REG1
    BANK --> REG2
    REG1 --> FIELD
    
    OBJECT["Base: object template<br/>lib/1.4/dml-builtins.dml:540"]
    DEVICE -.inherits.-> OBJECT
    BANK -.inherits.-> OBJECT
    REG1 -.inherits.-> OBJECT
```

Sources: [lib/1.4/dml-builtins.dml:540-578](), [lib/1.4/dml-builtins.dml:626-722](), [lib/1.4/dml-builtins.dml:1829-2215]()

## Compiling Your Device

**Compilation Command:**
```bash
dmlc --simics-api=6 simple_device.dml
```

This produces:
- `simple_device.c` - Main C implementation
- `simple_device.h` - Function declarations
- `simple_device-struct.h` - Device structure definition

**Generated Files Structure:**

```mermaid
graph TB
    INPUT["simple_device.dml"]
    DMLC["dmlc compiler"]
    
    subgraph "Generated C Code"
        MAIN_C["simple_device.c<br/>Implementations"]
        MAIN_H["simple_device.h<br/>Declarations"]
        STRUCT_H["simple_device-struct.h<br/>Device struct"]
    end
    
    GCC["GCC Compilation"]
    MODULE["simple_device.so<br/>Simics module"]
    
    INPUT --> DMLC
    DMLC --> MAIN_C
    DMLC --> MAIN_H
    DMLC --> STRUCT_H
    
    MAIN_C --> GCC
    MAIN_H --> GCC
    STRUCT_H --> GCC
    GCC --> MODULE
    
    DMLLIB["dmllib.h<br/>Runtime support"]
    DMLLIB -.-> MAIN_C
```

The C backend generation is handled by `generate()` in [dml/c_backend.py], which produces:
- Device structure definitions
- Method wrappers for Simics API
- Attribute registration code
- Event and hook serialization

Sources: [py/dml/dmlc.py:750-759](), [py/dml/output.py]()

## Error Handling

The compiler reports errors with file location and error codes. Common error types:

| Error Type | Code | Description | Reference |
|------------|------|-------------|-----------|
| Syntax error | `ESYNTAX` | Invalid DML syntax | [py/dml/messages.py] |
| Import error | `EIMPORT` | Cannot find imported file | [py/dml/messages.py] |
| Version error | `EVERS` | Version mismatch in imports | [py/dml/messages.py] |
| No device | `EDEVICE` | Missing device declaration | [py/dml/messages.py] |

Error reporting is centralized through `logging.report()` and controlled by `--max-errors` [py/dml/dmlc.py:450-454](). Warnings can be controlled with `--warn` and `--nowarn` flags [py/dml/dmlc.py:389-402]().

Sources: [py/dml/dmlc.py:777-799](), [py/dml/logging.py]()

## Next Steps

After successfully compiling your first device:

- **Learn DML syntax**: See [Syntax and Grammar](#3.2) for language constructs
- **Explore templates**: See [Standard Library](#4) for reusable components
- **Add functionality**: See [Methods and Parameters](#3.7) for implementing behavior
- **Handle memory access**: See [Memory-Mapped I/O](#4.5) for transaction handling
- **Manage state**: See [Serialization and Checkpointing](#6.1) for save/restore

For detailed compiler architecture, see [Compiler Architecture](#5). For testing and development workflows, see [Development Guide](#7).

Sources: [py/dml/dmlc.py:1-811](), [lib/1.4/dml-builtins.dml:1-300]()