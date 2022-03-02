<!--
  © 2021-2022 Intel Corporation
  SPDX-License-Identifier: MPL-2.0
-->

# Device Modeling Language

The Device Modeling Language (DML) is a domain-specific language for
writing fast functional or transaction-level device models for virtual
platforms.  DML provides high-level abstractions suitable for
functional device models, including constructs like register banks,
registers, bit fields, event posting, interfaces between models, and
logging. DML code is compiled by the DML Compiler (DMLC), producing C
code with API calls tailored for a particular simulator.

Currently, the compiler supports building models for the Intel®
[Simics®](https://www.intel.com/content/www/us/en/developer/articles/tool/simics-simulator.html)
simulator, but other back-ends may be added in the
future.


## Building and testing DMLC 

To build DMLC, you need to have a Simics
simulator installation and a Simics project set up.


### Using the Public Release of the Intel Simics Simulator

If you do not already have a Simics simulator installation or access to the
Simics simulator via commercial channels, install the [Public Release of
the Intel Simics simulator](https://software.intel.com/simics-simulator) 
and create a Simics project (automatic in the default installation flow).

### Building DMLC from a Simics project
In your Simics project, check out the DML repository into the `modules/dmlc`
directory.  At the top-level of the project, do `make dmlc`
(or `bin\make dmlc` on Windows). 

### Testing DMLC from a Simics project
To run the unit tests provided with DMLC, run `make test-dmlc` or
`bin/test-runner --suite modules/dmlc/test` from the top-level
of the project.


## Environment variables
The following environment variables are handy when developing DMLC.

### DMLC_DIR
After building DMLC, you need to set `DMLC_DIR` to `<your-project>/<hosttype>/bin`
in subsequent invocations of `make` in order to build devices with the locally
build compiler. `<hosttype>` is either `linux64` or `win64` depending on your 
host type. 

### T126_JOBS
When set, the given number of tests are run in parallel.

### DMLC_PATHSUBST
The DMLC build copies a few DML library files, e.g. `dml-builtins.dml`, into
`<hosttype>/bin`. When a compile error happens, error messages will normally point
to this copy rather than the source. By setting `DMLC_PATHSUBST` to
`<hosttype>/bin/dml=modules/dmlc/lib`, error messages will be rewritten to point
to the source file instead.  `<hosttype>` is either `linux64` or `win64` 
depending on your host type. 

### PY_SYMLINKS
When set to `1`, `make dmlc` will symlink Python files instead of copying
them. This has two effects: Python tracebacks will bring you to the source file
in the repository, and you don't need to re-run `make` after editing Python
files.

### DMLC_DEBUG
When set to `1`, unexpected exceptions in the compiler are echoed to
stderr. The default is to hide tracebacks in a file `dmlc-error.log`.

### DMLC_CC
Override the default compiler in unit tests.

### DMLC_PROFILE
When set, DMLC does self-profiling and writes the profile to a .prof file.
