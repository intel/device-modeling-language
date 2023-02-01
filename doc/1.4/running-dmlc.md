<!--
  © 2021-2023 Intel Corporation
  SPDX-License-Identifier: MPL-2.0
-->

# The DML compiler

A DML source file can be compiled into a runnable device model using
the DML compiler, `dmlc`. The main output of the compiler is a C file, that
can be compiled into a Simics module.

The DML compiler and its libraries are available as part of the
*Simics Base* package.

## Building dmlc

The `dmlc` compiler can be build locally. This requires an
installation of the Simics 6 base package.

In order to build the compiler, checkout [the DML repository
](https://github.com/intel/device-modeling-language) into the
into the `modules/dmlc` subdirectory of your Simics project. The compiler
can be built using the `make dmlc` command. The build result ends up
in <code><em>host</em>/bin/dml</code> (where *`host`* is `linux64` or
`win64`), and consists of three parts:

* <code><em>host</em>/bin/dml/python</code> contains the Python module that
  implements the compiler

* <code><em>host</em>/bin/dml/1.4</code> contains the standard libraries
  required to compile a device

* <code><em>host</em>/bin/dml/api</code> contains `.dml` files that
  expose the Simics API

In order to use a locally built version of `dmlc` to compile your
devices, you can add the following line to your `config-user.mk` file:

```
DMLC_DIR = $(SIMICS_PROJECT)/$(HOST_TYPE)/bin
```

## Running dmlc

The syntax for running `dmlc` from the command line is:

<pre>
dmlc [<em>options</em>] <em>input</em> [<em>output-base</em>]
</pre>

where *input* should be the name of a DML source file. If
*output-base* is provided, it will be used to name the created
files. The name of a DML source file should normally have the suffix
"`.dml`". The *input* file must contain a
[`device` declaration](language.html#device-declaration).

The main output of `dmlc` is a C file named
<code><em>&lt;output-base&gt;</em>.c</code>, which that can be compiled and
linked into a Simics module using the `gcc` compiler. The compiler
also produces some other helper files with `.h` and `.c` suffix, which
the main output file includes.

## Command Line Options

The following are the available command line options to
`dmlc`:

<dl><dt>

-h, --help
</dt><dd>

Print usage help.
</dd><dt>

-I *path*
</dt><dd>

Add *path* to the search path for imported
modules.
</dd><dt>

-D *name*=*definition*
</dt><dd>

Define a compile-time parameter.  The definition
must be a literal expression, and can be a quoted
string, a boolean, an integer, or a floating point
constant. The parameter will appear in the top-level scope.
</dd><dt>

--dep
</dt><dd>

Output makefile rules describing dependencies.
</dd><dt>

-T
</dt><dd>

Show tags on warning messages. The tags can be used with
the `--nowarn` and `--warn` options.
</dd><dt>

-g
</dt><dd>

Generate artifacts that allow for easier source-level debugging.
This generates a DML debug file leveraged by debug-simics, and
causes generated C code to follow the DML code more closely.
</dd><dt>

--coverity
</dt><dd>

Adds Synopsys® Coverity® analysis annotations to suppress common false positives
in generated C code created from DML 1.4 device models.
</dd><dt>

--warn=*tag*
</dt><dd>

Enable selected warnings. The tags can be found using
the `-T` option.
</dd><dt>

--nowarn=*tag*
</dt><dd>

Suppress selected warnings. The tags can be found using
the `-T` option.
</dd><dt>

--werror
</dt><dd>

Turn all warnings into errors.
</dd><dt>

--strict
</dt><dd>

Report errors for some constructs that will be forbidden in
future versions of the DML language
</dd><dt>

--noline
</dt><dd>

Suppress line directives for the C preprocessor so
that the C code can be debugged.
</dd><dt>

--info
</dt><dd>

Enable the output of an XML file describing register layout.
</dd><dt>

--version
</dt><dd>

Print version information.
</dd><dt>

--simics-api=*version*
</dt><dd>

Use Simics API version *version*.
</dd><dt>

--max-errors=*N*
</dt><dd>

Limit the number of error messages to *N*.
</dd></dl>


