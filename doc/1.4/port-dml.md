<!--
  © 2021 Intel Corporation
  SPDX-License-Identifier: MPL-2.0
-->

# Porting DML 1.2 to DML 1.4
When porting a DML 1.2 file to DML 1.4, most differences can be taken
care of by the automatic conversion script `port-dml`.

The porting process consists of two phases: An *analysis phase*, where the
`dmlc` compiler is run to produce a *tag file*, which describes the required
changes; this is followed by a *conversion phase* where the `port-dml`
script reads the tag file and applies changes.

The easiest way to perform conversion is through the wrapper script
`port-dml-module`. This script automatizes the porting process by first running an
analysis step using `make` to build one or more modules, and then running a
conversion phase by applying `port-dml` on the DML files that were compiled.

During the analysis phase, if some code is shared between multiple modules,
then it often happens that some unused code is discarded by the compiler before
it has been fully analysed. This happens in particular for unused templates and
top-level `if` statements. Such unused code will only get basic syntactic
conversions, like transforming `parameter` to `param`, and miss conversions
that depend on semantic analysis, like transforming `read_access`
to `read_register`.  This
can be solved by including multiple modules in the analysis step; the
conversion phase for common code will then combine the analyses of all modules,
and utilize semantic analysis from any code paths that is active in *any* of the
included modules.

The `port-dml-module` script relies on
rather crude heuristics which often may be
incorrect; for this reason, the script also prints exactly how it invokes the
lower level `make` and `port-dml` commands. This allows each step to be
individually rerun manually with tweaked settings.

The `port-dml` script can also be used directly without `port-dml-module`; this
mode of operation has a steeper learning curve but provides greater control
which can be advantageous when porting a large code base.

## Using the port-dml script
In order to port a DML 1.2 file to DML 1.4, first pass the <code>-P
<em>tagfile</em></code> command-line argument to `dmlc` when compiling
the file. This causes `dmlc` to output an analysis to the file
*tagfile*. The file contains one message per line, with a
machine-readable description of something that needs to be
changed. You can run the `port-dml` script to interpret and apply the
changes:

<pre>
bin/port-dml --src <em>file</em>.dml --dest <em>file</em>-1.4.dml --tags <em>tagfile</em>
</pre>

This will create a new DML 1.4 file
<code><em>file</em>-1.4.dml</code>. This should normally be a correct
DML 1.4 file, but the script has a number of [known
limitations](changes-manual.html) that can cause the file to be
broken. In most cases, broken conversions should be detected as a
compiler error when trying to compile the converted file.

Note that if the tag file exists, then DMLC will *append*
messages to the file, so if you need to re-run the analysis, you
should normally remove the file first.

If you build your device from a Simics project, you can use the variable
`DMLC_PORTING_TAG_FILE` to create a tag file. The variable should
be set to the absolute path of a file; `make` will pass that in the
-P flag to `dmlc`. Note that if you want to re-run an analysis,
then you need to first run <code>make clean-<em>module</em></code> to force
DMLC to re-run on all devices in the Simics module.

If parts of the device source code is unused, e.g. if a template is
never instantiated within the device, then DMLC can not perform a full
analysis on that code; only simple syntactical transformations will be
applied. The `port-dml` will detect this and emit a warning.
You can choose to either port the code manually, or perform an analysis of
an additional device that does use the code, and append that to the tagfile.

<div class="note">

**Note:** The `port-dml` script might fail because of a bug in the
script. When this kind of bug is encountered, the script will normally
print a traceback and point out a line in the tag file that it failed
to apply; you can get further in your conversion by manually removing
this line and re-running the script. After the script has succeeded,
you can look up the failed tags in the [list of porting
tags](changes-auto.html) and apply the change manually.

</div>

## Porting common code still used from DML 1.2 code

When porting a large code base to DML 1.4, you likely want to work
incrementally, porting some devices at a time. It can then happen that some of
your newly ported 1.4 files share common code with devices that are still in DML 1.2.

It is not allowed to import a DML 1.2 file from a DML 1.4 device, but a DML 1.2 device may import a DML 1.4 file with some caveats. Thus, any code common between DML 1.2 and 1.4 must be ported to 1.4 before any device can be converted.
There are two possible strategies for this: Either convert the common file in place,
or duplicate it into separate 1.2 and 1.4 versions.

### Keep a separate DML 1.4 copy

After letting conversion tools convert `foo.dml` to DML 1.4, you can rename the
converted file into `foo-dml14.dml`, and restore the original 1.2 version as
`foo-dml12.dml`, and finally create a trampoline file `foo.dml` containing:

```
dml 1.4;
#if (dml_1_2) {
  import "foo-dml12.dml";
} #else {
  import "foo-dml14.dml";
}
```

This way, existing `import "foo.dml";` statements from both DML 1.2 and 1.4 devices
will continue to work.

The apparent downside of this approach is that the logic of the common code is
duplicated across two files, which is a problem if the DML 1.2 variant is expected
to be maintained over a longer period of time. However, if all uses from DML 1.2 of
the common code are expected to be ported within a short migration period,
then this is likely the preferred approach.

After the last DML 1.2 use of the common code has been ported, `foo-dml12.dml` can be removed, and `foo-dml14.dml` can be moved back to `foo.dml`, overwriting the trampoline.

Note that the `#if` trick in the `foo.dml` trampoline above utilizes an
otherwise undocumented DML feature: DML normally doesn't allow `import`
statements within `#if` blocks, but a special exception was added to permit it
specifically within `#if (dml_1_2)`, in order to support this use case.

### In-place conversion, preserving DML 1.2 compatibility

A common file can be ported to DML 1.4 and still be useful from DML 1.2, with a
number of caveats. For instance, devices often implement functionality by
overriding standard methods, and some methods have been renamed between DML 1.2
and 1.4. For instance, an override of the `read_access` register method in a
DML 1.2 device roughly corresponds to a `read_register` override in a DML 1.4
device, and an attribute with `parameter allocate_type = "uint64"` in DML 1.2
corresponds to an event with `is uint64_attr` in DML 1.4. Much of this can be
taken care of by the `dml12-compatibility.dml` layer: A shared DML 1.4 file can
say `import "dml12-compatibility.dml";`. This does nothing when imported from a
DML 1.4, but when imported from DML 1.2, it provides some glue that ties DML
1.4 constructs to the DML 1.2 API. For instance, it defines templates such that
`is uint64_attr` in the DML 1.4 file will expand to define `allocate_type` when
imported from DML 1.2. This file also provides some templates for explicit
instantiation. In particular, the `dml12_compat_read_register` template can be
instantiated on a DML 1.4 register that overrides the `read_register` method;
this has no effect in a DML 1.4 device, but in a DML 1.2 device it overrides
the DML 1.2 method `read_access` to call the provided override. Similarly,
the `dml12_compat_write_register` template can be used on registers that override
`write_register`; `dml12_compat_read_field` and
`dml12_compat_write_field` can be used on field that override the `read_field` or `write_field` method; and `dml12_compat_io_memory_access` can be used on banks that override the `io_memory_access` method.

Sometimes, the facilities in `dml12-compatibility.dml` are not sufficient for
full DML 1.2 compatibility. For instance, suppose you want to use the `shared` annotation on a `read` method when writing the DML 1.4 version of a template. There are fundamental limitations in DML 1.2 that prevent such overrides. This can be overcome with an `#if (dml_1_2)` block on the top level:
```
dml 1.4;

#if (dml_1_2) {
    template read_twelve {
        method read() -> (uint64) {
            log info: "read";
            return 12;
        }
    }
} #else {
    template read_twelve is read {
        shared method read() -> (uint64) {
            log info: "read";
            return 12;
        }
    }
}
```
This is somewhat similar to the `foo-dml14.dml` trampoline approach discussed above, with
the difference that it can be applied selectively only on problematic parts of the
file.

If the flag `--compat` is passed to the `port-dml` script, then the script will
automatically detect some cases where similar `#if` clauses are needed for
compatibility, and insert them automatically. The script will also add an
`dml12-compatibility.dml` import. The `--compat` flag can also be passed to the
`port-dml-module` script; in this case, the script will pass on `--compat` to
`port-dml` when converting DML files that don't reside in the directory of any
of the ported modules.
