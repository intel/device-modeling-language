# Build System and Documentation

<details>
<summary>Relevant source files</summary>

The following files were used as context for generating this wiki page:

- [MODULEINFO](MODULEINFO)
- [Makefile](Makefile)
- [md_to_github.py](md_to_github.py)
- [validate_md_links.py](validate_md_links.py)

</details>



## Purpose and Scope

This page describes the build system and documentation generation infrastructure for the DML compiler. It covers the Makefile-based build orchestration, Python module compilation, standard library deployment, parser table generation, and the complete documentation pipeline from source Markdown and code comments to HTML reference manuals and GitHub wiki archives.

For information about the compiler's internal architecture, see [Compiler Architecture](#5). For testing infrastructure, see [Testing Framework](#7.1).

## Build System Overview

The DML compiler build system is orchestrated by a GNU Makefile that performs five major build phases:

1. **Python Module Compilation**: Compiles Python source modules into the deployment structure
2. **Library Deployment**: Copies DML standard library files to version-specific directories
3. **Parser Table Generation**: Pre-generates PLY parser tables for fast startup
4. **DMLAST Precompilation**: Pre-parses standard library files into `.dmlast` format
5. **Documentation Generation**: Produces HTML reference manuals and GitHub wiki archives

```mermaid
graph TB
    subgraph "Build Inputs"
        PY_SRC["py/*.py<br/>Python source modules"]
        LIB_SRC["lib/1.2/*.dml<br/>lib/1.4/*.dml<br/>Standard library source"]
        LIB_OLD["lib-old-4.8/*/*.dml<br/>Legacy API 4.8 libraries"]
        DOC_SRC["doc/1.2/*.md<br/>doc/1.4/*.md<br/>Manual documentation"]
        LICENSE["LICENSE files<br/>MPL-2.0, BSD-0-Clause"]
    end
    
    subgraph "Build System [Makefile]"
        PYCOMPILE["PYCOMPILE target<br/>Python module compiler"]
        PARSER_GEN["Parser table generation<br/>generate_parsetabs.py"]
        LIB_COPY["Library file copying<br/>make rules for .dml"]
        DMLAST_GEN["DMLAST generation<br/>dmlast.py"]
        DOC_GEN["Documentation generation<br/>*_to_md.py scripts"]
    end
    
    subgraph "Deployment Structure"
        BIN_DML["HOST/bin/dml/python/<br/>Compiled Python modules"]
        BIN_LIB["HOST/bin/dml/1.2/<br/>HOST/bin/dml/1.4/<br/>Standard libraries"]
        BIN_OLD["HOST/bin/dml-old-4.8/<br/>API 4.8 libraries"]
        BIN_INCLUDE["HOST/bin/dml/include/<br/>dmllib.h runtime"]
        DOC_HTML["HOST/doc/html/<br/>dml-1.2-reference-manual/<br/>dml-1.4-reference-manual/"]
    end
    
    PY_SRC --> PYCOMPILE
    PY_SRC --> PARSER_GEN
    LIB_SRC --> LIB_COPY
    LIB_OLD --> LIB_COPY
    DOC_SRC --> DOC_GEN
    
    PYCOMPILE --> BIN_DML
    PARSER_GEN --> BIN_DML
    LIB_COPY --> BIN_LIB
    LIB_COPY --> BIN_OLD
    LIB_COPY --> DMLAST_GEN
    DMLAST_GEN --> BIN_LIB
    DMLAST_GEN --> BIN_OLD
    DOC_GEN --> DOC_HTML
    
    LICENSE --> BIN_DML
    LICENSE --> BIN_LIB
    LICENSE --> BIN_OLD
    LICENSE --> BIN_INCLUDE
```

**Diagram: DML Compiler Build System Architecture**

Sources: [Makefile:1-251](), [MODULEINFO:1-134]()

## Module Structure and Deployment

The MODULEINFO file defines five module groups that organize the compiler's components for Simics package management:

| Group | Purpose | Key Components |
|-------|---------|----------------|
| `dmlc` | Main compiler module | All sub-groups, release notes |
| `dmlc-py` | Python source modules | 31 Python files in `dml/` package |
| `dmlc-lib` | Standard library files | DML libraries, runtime headers, licenses |
| `dml-1.2-reference-manual` | DML 1.2 documentation | HTML reference manual |
| `dml-1.4-reference-manual` | DML 1.4 documentation | HTML reference manual |

The deployment structure supports multiple DML versions and Simics API versions:

```mermaid
graph TB
    subgraph "Deployment: HOST/bin/dml/"
        PYTHON["python/<br/>__main__.py<br/>dml/*.py packages<br/>port_dml.py<br/>dead_dml_methods.py"]
        LIB12["1.2/<br/>dml-builtins.dml<br/>utility.dml<br/>etc."]
        LIB14["1.4/<br/>dml-builtins.dml<br/>utility.dml<br/>etc."]
        INCLUDE["include/simics/<br/>dmllib.h"]
    end
    
    subgraph "Deployment: HOST/bin/dml-old-4.8/"
        LIB12_OLD["1.2/<br/>Legacy API 4.8<br/>libraries"]
        LIB14_OLD["1.4/<br/>Legacy API 4.8<br/>libraries"]
    end
    
    subgraph "Deployment: HOST/doc/html/"
        DOC12["dml-1.2-reference-manual/<br/>HTML files"]
        DOC14["dml-1.4-reference-manual/<br/>HTML files"]
    end
    
    PYTHON --> LIB12
    PYTHON --> LIB14
    PYTHON --> LIB12_OLD
    PYTHON --> LIB14_OLD
```

**Diagram: Deployed Module Directory Structure**

The `@6-only()` directive in MODULEINFO conditionally includes API 4.8 libraries only in Simics 6 builds. This allows the compiler to support legacy devices while encouraging migration to modern APIs.

Sources: [MODULEINFO:1-134](), [Makefile:8-97]()

## Python Module Compilation

Python modules are compiled using the `copy_python.py` script, which copies source files while optionally compiling to bytecode:

```mermaid
graph LR
    subgraph "Source Modules"
        DMLC_PY["py/dml/*.py<br/>46 source files"]
        MAIN_PY["py/__main__.py<br/>Entry point"]
        SCRIPTS["py/port_dml.py<br/>py/dead_dml_methods.py"]
    end
    
    subgraph "Generated Modules"
        PARSETAB12["dml12_parsetab.py<br/>PLY parser table"]
        PARSETAB14["dml14_parsetab.py<br/>PLY parser table"]
        ENV["env.py<br/>Build environment info"]
    end
    
    subgraph "Build Process"
        PYCOMPILE["PYCOMPILE := python copy_python.py<br/>Makefile:102"]
        GEN_PARSE["generate_parsetabs.py<br/>Makefile:123-129"]
        GEN_ENV["generate_env.py<br/>Makefile:131-132"]
    end
    
    DMLC_PY --> PYCOMPILE
    MAIN_PY --> PYCOMPILE
    SCRIPTS --> PYCOMPILE
    
    DMLC_PY --> GEN_PARSE
    GEN_PARSE --> PARSETAB12
    GEN_PARSE --> PARSETAB14
    
    GEN_ENV --> ENV
    
    PYCOMPILE --> OUT_PY["HOST/bin/dml/python/<br/>Compiled modules"]
    PARSETAB12 --> OUT_PY
    PARSETAB14 --> OUT_PY
    ENV --> OUT_PY
```

**Diagram: Python Module Compilation Pipeline**

The Makefile defines three sets of Python files:

1. **PYFILES** [Makefile:11-46](): 46 source modules copied from `py/` directory
2. **GEN_PYFILES** [Makefile:51-53](): 3 generated modules created during build
3. **SCRIPTS** [Makefile:82](): 2 standalone scripts copied without modification

The compilation rules ensure dependencies are respected:

- `OUT_PYFILES` depends on source files [Makefile:108-110]()
- `OUT_GEN_PYFILES` depends on source files [Makefile:104-106]()
- Parser tables depend on lexer and parser modules [Makefile:123-129]()

Sources: [Makefile:11-142]()

## Standard Library Management

The build system manages three sets of DML library files with version-specific deployment:

### Library Source Organization

| Source Directory | Target Directory | DML Versions | Description |
|-----------------|------------------|--------------|-------------|
| `lib/1.2/` | `$(HOST)/bin/dml/1.2/` | All APIs | DML 1.2 standard library |
| `lib/1.4/` | `$(HOST)/bin/dml/1.4/` | All APIs | DML 1.4 standard library |
| `lib-old-4.8/` | `$(HOST)/bin/dml-old-4.8/` | API 4.8 only | Legacy API-specific libraries |

### DMLAST Precompilation

The build system pre-parses all standard library files into `.dmlast` format for faster compilation. This process is managed by dependency markers:

```mermaid
graph TB
    subgraph "DMLAST Generation System"
        MARKER_NEW["dml/1.2.d<br/>dml/1.4.d<br/>Dependency markers"]
        MARKER_OLD["dml-old-4.8/1.2.d<br/>dml-old-4.8/1.4.d<br/>Legacy markers"]
        MARKER_API["dml/api/6/1.2.d<br/>dml/api/7/1.2.d<br/>etc.<br/>API-specific markers"]
    end
    
    subgraph "Inputs"
        DML_FILES["lib/**/*.dml<br/>Standard library source"]
        DMLAST_GEN_MARKER["dmlast-generator<br/>Build system marker"]
    end
    
    subgraph "Process [dmlast.py]"
        DMLC_CMD["DMLC_CMD --save-asts<br/>Pre-parse all .dml files"]
    end
    
    subgraph "Outputs"
        DMLAST_NEW["HOST/bin/dml/1.2/*.dmlast<br/>HOST/bin/dml/1.4/*.dmlast"]
        DMLAST_OLD["HOST/bin/dml-old-4.8/**/*.dmlast"]
    end
    
    DML_FILES --> MARKER_NEW
    DML_FILES --> MARKER_OLD
    DMLAST_GEN_MARKER --> MARKER_NEW
    DMLAST_GEN_MARKER --> MARKER_OLD
    
    MARKER_NEW --> DMLC_CMD
    MARKER_OLD --> DMLC_CMD
    MARKER_API --> DMLC_CMD
    
    DMLC_CMD --> DMLAST_NEW
    DMLC_CMD --> DMLAST_OLD
```

**Diagram: DMLAST Precompilation System**

The dependency markers serve dual purposes [Makefile:182-198]():
1. **Markers**: Track when precompilation is needed (when any `.dml` file changes)
2. **Depfiles**: Make-style dependency files that trigger rebuilds

The `dmlast-generator` marker [Makefile:177-178]() is touched whenever the parser or compiler changes, forcing recompilation of all `.dmlast` files.

Sources: [Makefile:66-198](), [MODULEINFO:8-133]()

## Parser Table Generation

The compiler uses PLY (Python Lex-Yacc) for parsing, which requires pre-generated parser tables for performance. The build system generates two sets of tables for DML 1.2 and 1.4:

```mermaid
graph TB
    subgraph "Input Dependencies"
        GEN_SCRIPT["generate_parsetabs.py"]
        DMLPARSE["dml/dmlparse.py<br/>Grammar rules"]
        DMLLEX["dml/dmllex.py<br/>Base lexer"]
        DMLLEX12["dml/dmllex12.py<br/>DML 1.2 lexer"]
        DMLLEX14["dml/dmllex14.py<br/>DML 1.4 lexer"]
        AST["dml/ast.py<br/>AST definitions"]
    end
    
    subgraph "Generation Process"
        INVOKE12["python generate_parsetabs.py<br/>PYTHONPATH 12 dml12_parsetab parser.out"]
        INVOKE14["python generate_parsetabs.py<br/>PYTHONPATH 14 dml14_parsetab parser.out"]
    end
    
    subgraph "Generated Outputs"
        PARSETAB12["dml/dml12_parsetab.py<br/>Parser state tables"]
        PARSETAB14["dml/dml14_parsetab.py<br/>Parser state tables"]
        PARSER_OUT12["dml12_parser.out<br/>Debug information"]
        PARSER_OUT14["dml14_parser.out<br/>Debug information"]
    end
    
    GEN_SCRIPT --> INVOKE12
    GEN_SCRIPT --> INVOKE14
    DMLPARSE --> INVOKE12
    DMLPARSE --> INVOKE14
    DMLLEX --> INVOKE12
    DMLLEX --> INVOKE14
    DMLLEX12 --> INVOKE12
    DMLLEX14 --> INVOKE14
    AST --> INVOKE12
    AST --> INVOKE14
    
    INVOKE12 --> PARSETAB12
    INVOKE12 --> PARSER_OUT12
    INVOKE14 --> PARSETAB14
    INVOKE14 --> PARSER_OUT14
```

**Diagram: Parser Table Generation Process**

The Makefile rule [Makefile:123-129]() ensures parser tables are regenerated when:
- Grammar rules in `dmlparse.py` change
- Lexer definitions in `dmllex*.py` change
- AST definitions in `ast.py` change

The `parser.out` debug files are also used by documentation generation to extract grammar rules for the reference manual [Makefile:63-64]().

Sources: [Makefile:51-53](), [Makefile:123-132]()

## Documentation Generation Pipeline

The DML reference manual combines hand-written Markdown with auto-generated content extracted from multiple sources:

### Documentation Sources

```mermaid
graph TB
    subgraph "Manual Documentation"
        LANG_MD["doc/1.4/language.md<br/>Language specification"]
        OVERVIEW_MD["doc/1.4/*.md<br/>Tutorial, guides"]
        TOC["doc/1.4/toc.json<br/>Table of contents"]
        RELEASE["RELEASENOTES-1.4.md<br/>Version history"]
    end
    
    subgraph "Auto-Generated Content"
        GRAMMAR["generated-md-1.4/grammar.md<br/>from parser.out"]
        MESSAGES["generated-md-1.4/messages.md<br/>from messages.py"]
        CHANGES["generated-md-1.4/changes-auto.md<br/>from breaking_changes.py"]
        DEPREC["generated-md-1.4/deprecations-auto.md<br/>from messages.py"]
        PROVIS["generated-md-1.4/provisional-auto.md<br/>from provisional.py"]
        BUILTINS["generated-md-1.4/dml-builtins.md<br/>from lib/1.4/dml-builtins.dml"]
        UTILITY["generated-md-1.4/utility.md<br/>from lib/1.4/utility.dml"]
    end
    
    subgraph "Generation Scripts"
        GRAMMAR_GEN["grammar_to_md.py"]
        MESSAGES_GEN["messages_to_md.py"]
        PORTING_GEN["porting_to_md.py"]
        DEPREC_GEN["deprecations_to_md.py"]
        PROVIS_GEN["provisional_to_md.py"]
        COMMENT_GEN["dmlcomments_to_md.py"]
    end
    
    GRAMMAR_GEN --> GRAMMAR
    MESSAGES_GEN --> MESSAGES
    PORTING_GEN --> CHANGES
    DEPREC_GEN --> DEPREC
    PROVIS_GEN --> PROVIS
    COMMENT_GEN --> BUILTINS
    COMMENT_GEN --> UTILITY
```

**Diagram: Documentation Generation Sources**

### Build Pipeline

The documentation build follows this sequence:

```mermaid
graph LR
    subgraph "Stage 1: Generate Markdown"
        GEN_MD["Generate auto-content<br/>*_to_md.py scripts"]
    end
    
    subgraph "Stage 2: Validate"
        VALIDATE["validate_md_links.py<br/>Check anchors, links"]
    end
    
    subgraph "Stage 3: Convert to HTML"
        DODOC["dodoc tool<br/>MD → HTML converter"]
    end
    
    subgraph "Stage 4: GitHub Export"
        GITHUB["md_to_github.py<br/>Create wiki archive"]
    end
    
    subgraph "Outputs"
        HTML["doc/html/dml-1.4-reference-manual/<br/>filelist.json + HTML"]
        WIKI["github-wiki/<br/>Tarball for GitHub"]
    end
    
    GEN_MD --> VALIDATE
    VALIDATE --> DODOC
    VALIDATE --> GITHUB
    DODOC --> HTML
    GITHUB --> WIKI
```

**Diagram: Documentation Build Pipeline**

The validation step [Makefile:232-233]() ensures:
- All internal links point to valid anchors [validate_md_links.py:62-79]()
- Referenced HTML files exist in the documentation set
- No en-dashes (`--`) are used instead of em-dashes (`&mdash;`) [validate_md_links.py:99-102]()

Sources: [Makefile:200-250](), [validate_md_links.py:1-107]()

## Documentation Generation Scripts

Each auto-generated documentation file is produced by a specialized Python script:

| Script | Input | Output | Purpose |
|--------|-------|--------|---------|
| `grammar_to_md.py` | `parser.out` | `grammar.md` | Extract grammar rules from PLY parser |
| `messages_to_md.py` | `messages.py` | `messages.md` | Document compiler error/warning messages |
| `porting_to_md.py` | `breaking_changes.py` | `changes-auto.md` | List breaking changes by API version |
| `deprecations_to_md.py` | `messages.py` | `deprecations-auto.md` | Document deprecated features |
| `provisional_to_md.py` | `provisional.py` | `provisional-auto.md` | List provisional (unstable) features |
| `dmlcomments_to_md.py` | `lib/1.4/*.dml` | `dml-builtins.md`, `utility.md` | Extract API docs from DML comments |

### Grammar Extraction

The `grammar_to_md.py` script parses PLY's parser debug output to generate human-readable grammar documentation [Makefile:204-205]():

```
dmlparse.py → PLY → parser.out → grammar_to_md.py → grammar.md
```

This ensures the documented grammar exactly matches the implemented parser.

### Library Documentation Extraction

The `dmlcomments_to_md.py` script extracts documentation from special comments in DML library files [Makefile:227-228]():

```dml
// Documentation comment format:
/// This is a documented template
template my_template {
    /// This parameter controls...
    param my_param;
}
```

Sources: [Makefile:204-228]()

## GitHub Wiki Export

The `md_to_github.py` script converts the reference manual to GitHub wiki format by:

1. **Rewriting Links** [md_to_github.py:90-96](): Converts relative HTML links to wiki-style links
   ```
   [text](language.html#section) → [text](Language#section)
   ```

2. **Adding Copyright Headers** [md_to_github.py:48-52](): Prepends MPL-2.0 license notice to each file

3. **Creating Front Page** [md_to_github.py:68-69](): Generates table of contents with hierarchical section numbering

4. **Packaging** [md_to_github.py:88-105](): Creates `.tar.gz` archive for upload to GitHub

The script uses the same `toc.json` structure as the main documentation build [md_to_github.py:15-31](), ensuring consistency between HTML and wiki versions.

Sources: [md_to_github.py:1-106]()

## Link Validation System

The `validate_md_links.py` script ensures documentation integrity through comprehensive link checking:

### Anchor Detection

The validator detects three types of anchors [validate_md_links.py:36-53]():

1. **Explicit anchors**: `<a id="anchor-name"/>`
2. **Header anchors**: Generated from `#` header lines (normalized to lowercase, hyphenated)
3. **Ambiguous anchors**: Headers with identical text (excluded from valid anchors)

### Link Validation

For each link, the validator [validate_md_links.py:62-79]():

1. Parses the link format: `[text](file.html#anchor)`
2. Resolves the target file from multiple source directories
3. Verifies the anchor exists in the target file's content
4. Reports errors with file and line number information

### Special Validations

- **External links**: URLs starting with `https://` are skipped [validate_md_links.py:67-68]()
- **Typography**: Detects `--` that should be `&mdash;` [validate_md_links.py:99-102]()
- **Character restrictions**: Headers with special characters cannot be referenced [validate_md_links.py:30-33](), [validate_md_links.py:45-47]()

Sources: [validate_md_links.py:1-107]()

## License Management

The build system manages three license types across the codebase:

| License | Files | Purpose |
|---------|-------|---------|
| MPL-2.0 | `$(PYTHONPATH)/LICENSE` | Python compiler modules |
| BSD-0-Clause | `$(DMLLIB_DEST)/*/LICENSE` | DML standard library (public domain) |
| BSD-0-Clause | `$(DMLLIB_DEST)/include/simics/LICENSE` | Runtime header `dmllib.h` |

License deployment rules [Makefile:84](), [Makefile:164-173]():

```makefile
MPL_LICENSE := $(PYTHONPATH)/LICENSE
BSD0_LICENSES := $(addsuffix /LICENSE,$(DMLLIB_DESTDIRS) 
                              $(OLD_DMLLIB_DESTDIRS_4_8) 
                              $(DMLLIB_DEST)/include/simics)
```

The separation reflects different usage models:
- **Compiler (MPL-2.0)**: Requires derivative works to remain open source
- **Libraries (BSD-0-Clause)**: No restrictions, suitable for embedding in proprietary devices

Sources: [Makefile:83-84](), [Makefile:160-173](), [MODULEINFO:88-93]()

## Build Targets and Phases

The Makefile defines several key targets that can be built independently:

### Primary Target: `all`

The default target [Makefile:90-97]() builds all components:

```makefile
all: $(DMLC_BIN)                    # Compiler binaries
     $(SCRIPTS)                     # Utility scripts
     $(DMLFILES)                    # Standard libraries
     $(OLD_DMLFILES_4_8)            # Legacy libraries
     $(BSD0_LICENSES)               # Library licenses
     $(MPL_LICENSE)                 # Compiler license
     $(PYUNIT_TESTED)               # Unit test results
     $(PARSER_DEBUGFILES)           # Parser debug info
```

### Dependency Chain

```mermaid
graph TB
    ALL["all target"]
    
    DMLC_BIN["DMLC_BIN<br/>OUT_PYFILES + OUT_GEN_PYFILES + HFILES"]
    SCRIPTS["SCRIPTS<br/>port_dml.py, dead_dml_methods.py"]
    DMLFILES["DMLFILES<br/>Standard library .dml files"]
    PREPARSE["PREPARSE_MARKERS<br/>.d dependency markers"]
    LICENSES["LICENSE files"]
    TESTS["PYUNIT_TESTED<br/>Unit test markers"]
    DOC12["DOC_MARKER_12<br/>DML 1.2 documentation"]
    DOC14["DOC_MARKER_14<br/>DML 1.4 documentation"]
    
    ALL --> DMLC_BIN
    ALL --> SCRIPTS
    ALL --> DMLFILES
    ALL --> PREPARSE
    ALL --> LICENSES
    ALL --> TESTS
    ALL --> DOC12
    ALL --> DOC14
    
    PREPARSE --> DMLFILES
    PREPARSE --> DMLAST_GEN["dmlast-generator"]
    DMLAST_GEN --> DMLC_BIN
    
    TESTS --> DMLC_BIN
```

**Diagram: Build Target Dependencies**

Sources: [Makefile:90-97](), [Makefile:177-198](), [Makefile:235](), [Makefile:250]()

## Unit Test Integration

The build system runs Python unit tests as part of the build process [Makefile:48-50](), [Makefile:138-142]():

```makefile
PYUNIT_FILES := $(wildcard $(DMLC_DIR)/py/dml/*_test.py)
PYUNIT_TESTED := $(foreach x,$(PYUNIT_FILES),
                    $(basename $(notdir $x))-pyunit-tested)

%-pyunit-tested: $(DMLC_DIR)/py/dml/%.py $(OUT_PYFILES)
	$(RUN_PY_UNIT_TEST) $(SIMICS_PROJECT)/$(HOST_TYPE) $<
	touch $@
```

The build creates marker files (e.g., `ctree_test-pyunit-tested`) that track successful test execution. Tests are re-run when:
- The tested module changes
- Any compiled Python module changes (dependency on `$(OUT_PYFILES)`)

Some tests require external tools [Makefile:134-136]():
```makefile
# needed by ctree_test.py
export CC
export SIMICS_BASE
```

Sources: [Makefile:48-50](), [Makefile:134-142]()

## Build Environment Variables

The build system configures several environment variables and paths:

| Variable | Definition | Purpose |
|----------|-----------|---------|
| `DMLC_DIR` | `$(SRC_BASE)/$(TARGET)` | Source directory location |
| `LIBDIR` | `$(SIMICS_PROJECT)/$(HOST_TYPE)/bin` | Deployment base directory |
| `PYTHONPATH` | `$(LIBDIR)/dml/python` | Python module search path |
| `DMLC_CMD` | `$(PYTHON) $(PYTHONPATH)/__main__.py` | Compiler invocation command |
| `PYCOMPILE` | `$(PYTHON) $(SIMICS_BASE)/scripts/copy_python.py` | Python compilation script |
| `DODOC` | `$(SIMICS_BASE)/$(HOST_TYPE)/bin/dodoc$(EXE_SUFFIX)` | Documentation tool |

The `generate_env.py` script creates `env.py` [Makefile:131-132]() with build-time configuration that the compiler can query at runtime.

Sources: [Makefile:8-10](), [Makefile:57-58](), [Makefile:100-102](), [Makefile:200]()