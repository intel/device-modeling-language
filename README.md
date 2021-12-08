<!--
  © 2021 Intel Corporation
  SPDX-License-Identifier: MPL-2.0
-->

# Device Modeling Language

DML is a language for writing device models for computer architecture
simulators. Currently, the compiler only supports building models for
the
[Simics®](https://www.intel.com/content/www/us/en/download/645996/simics-simulator-public-release-preview.html)
simulator, but other back-ends may be added in the future.

## Building and testing
In your Simics project, check out the DML repository into the `modules/dmlc`
directory, `make dmlc`. To run unit tests, run `make test-dmlc` or
`bin/test-runner --suite modules/dmlc/test`.

## Environment variables
The following environment variables are handy when developing DMLC.

### DMLC_DIR
After building DMLC, you need to set `DMLC_DIR` to `<your-project>/linux64/bin`
in subsequent invocations of `make` in order to build devices with the locally
build compiler.

### T126_JOBS
When set, the given number of tests are run in parallel.

### DMLC_PATHSUBST
The DMLC build copies a few DML library files, e.g. `dml-builtins.dml`, into
`linux64/bin`. When a compile error happens, error messages will normally point
to this copy rather than the source. By setting `DMLC_PATHSUBST` to
`linux64/bin/dml=modules/dmlc/lib`, error messages will be rewritten to point
to the source file instead.

### PY_SYMLINKS
When set to `1`, `make dmlc` will symlink Python files instead of copying
them. This has two effects: Python tracebacks will bring you to the source file
in the repository, and you don't need to re-run `make` after editing Python
files.

### DMLC_DEBUG
When set to `1`, unexpected exceptions in the compiler are echoed to
stderr. The default is to hide tracebacks in a file `dmlc-error.log`.
