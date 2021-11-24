<!--
  © 2021 Intel Corporation
  SPDX-License-Identifier: MPL-2.0
-->

# Porting DML 1.2 to DML 1.4
When porting a DML 1.2 file to DML 1.4, most differences can be taken
care of by the automatic conversion script `port-dml`. The
script relies on the `dmlc` compiler to produce information on
what changes need to be applied.

The easiest way to invoke `port-dml` is through a wrapper
script `port-dml-module`. The script ports all devices in one
SIMICS module, and all imported files it depends on. The scripts works by
invoking `make` and `port-dml`, and prints how they are invoked, which
is instructive for understanding how to use `port-dml`
standalone.

The `port-dml-module` script works well for converting most DML
devices, but has some limitations when it comes to common code that is
shared between many devices. In particular, if some parts of common
code is unused, e.g. if a provided template is never instantiated,
then it may skip some conversions. For this reason, it can be better
to use `port-dml` directly for common code.



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
DMLC to re-run on all devices in the SIMICS module.

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

