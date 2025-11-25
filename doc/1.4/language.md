<!--
  © 2021 Intel Corporation
  SPDX-License-Identifier: MPL-2.0
-->

# Device Modeling Language, version 1.4

This chapter describes the Device Modeling Language (DML), version 1.4. It will
help to have read and understood the object model in the previous
chapter before reading this chapter.

## Overview

DML is not a general-purpose programming language, but a modeling
language, targeted at writing Simics device models. The algorithmic part
of the language is similar to ISO C; however, the main power
of DML is derived from its simple object-oriented constructs for
defining and accessing the static data structures that a device model
requires, and the automatic creation of bindings to Simics.

Furthermore, DML provides syntax for *bit-slicing*, which much simplifies the
manipulation of bit fields in integers; [`new`](#new-expressions) and
[`delete`](#delete-statements) operators for allocating and deallocating
memory; a basic [`try`](#try-statements)/[`throw`](#throw-statements) mechanism
for error handling; built-in [`log`](#log-statements) and
[`assert`](#assert-statements) statements; and a powerful metaprogramming
facility using [*templates*](#templates) and [*in each
statements*](#in-each-declarations).

Most of the built-in Simics-specific logic is implemented directly in
DML, in standard library modules that are automatically imported; the
`dmlc` compiler itself contains as little knowledge as possible
about the specifics of Simics.

## Lexical Structure

A major difference from C is that names do not generally need to
be defined before their first use. This is quite useful, but might
sometimes appear confusing to C programmers.

<dl><dt>

Character encoding
</dt><dd>

DML source files are written using UTF-8 encoding.  Non-ASCII characters are
only allowed in comments and in string literals.  Unicode BiDi control
characters (U+2066 to U+2069 and U+202a to U+202e) are not allowed.  String
values are still handled as byte arrays, which means that a string value
written with a literal of three characters may actually create an array of more
than three bytes.  </dd><dt>

Reserved words
</dt><dd>

All ISO/ANSI C reserved words are reserved words in DML (even if
currently unused). In addition, the C99 and C++ reserved words
`restrict`, `inline`, `this`, `new`,
`delete`, `throw`, `try`, `catch`, and
`template` are also reserved in DML. The C++ reserved words
`class`, `namespace`, `private`,
`protected`, `public`, `using`, and
`virtual`, are reserved in DML for future use; as are
identifiers starting with an underscore (`_`).

The following words are reserved specially by DML: `after`,
`assert`, `call`, `cast`, `defined`, `each`,
`error`, `foreach`, `in`, `is`,
`local`, `log`, `param`, `saved`, `select`,
`session`, `shared`, `sizeoftype`, `typeof`, `undefined`,
`vect`, `where`, `async`, `await`,
`with`, and `stringify`.

</dd><dt>

Identifiers
</dt><dd>

Identifiers in DML are defined as in C; an identifier may begin
with a letter or underscore, followed by any number of letters,
numbers, or underscores.

<a id="discard-identifier"/>
Identifiers that begin with an underscore (`_`) are reserved by the DML language
and standard library and should not be used, with the exception of the single
underscore `_`; this is considered to be the *discard identifier*, and is only
permitted as the name of a declaration in specific contexts, where it gives the
declaration special semantics. Currently, these contexts are:
* Method-local bindings, e.g. [local variables](#local-statements) &mdash;
see that section for more information.
* Index parameters for object arrays. See the documentation for the
  [`object` template](dml-builtins.html#object) for more information.
* As the name of one or more members of a [layout type](#layouts).

</dd><dt>

Constant Literals
</dt><dd>

DML has literals for strings, characters, integers, booleans, and
floating-point numbers.  The integer literals can be written in
decimal (`01234`), hexadecimal (`0x12af`), or binary
(`0b110110`) form.

Underscores (`_`) can be used between digits, or immediately
following the `0b`, `0x` prefixes, in integer literals
to separate groups of digits for improved readability. For example,
`123_456`, `0b10_1110`, `0x_eace_f9b6` are valid
integer constants, whereas `_78`, `0xab_` are not.

String literals are surrounded by double quotes (`"`). To
include a double quote or a backslash (`\`) in a string
literal, precede them with a backslash (`\"` and `\\`,
respectively). Newline, carriage return, tab and backspace characters
are represented by `\n`, `\r`, `\t` and
`\b`. Arbitrary byte values can be encoded as `\x`
followed by exactly two hexadecimal digits, such as `\x1f`.
Such escaped byte values are restricted to 00-7f for strings
containing Unicode characters above U+007F.

Character literals consist of a pair of single quotes (`'`)
surrounding either a single printable ASCII character or one of the
escape sequences `\'`, `\\`, `\n`, `\r`,
`\t` or `\b`. The value of a character literal is
the character's ASCII value.
</dd><dt>

Comments
</dt><dd>

C-style comments are used in DML.  This includes both in-line
comments (`/*`...`*/`) and comments
that continue to the end of the line (`//`...).
</dd></dl>

## Module System

DML employs a very simple module system, where a *module* is any source file
that can be imported using the [`import` directive](#import-declarations).
Such files may not contain a [`device` declaration], but otherwise look like
normal DML source files. The imported files are merged into the main model
as if all the code was contained in a single file (with some exceptions). This
is similar to C preprocessor `#include` directives, but in DML each imported
file must be possible to parse in isolation, and may contain declarations (such
as [`bitorder`](#bitorder-declarations)) that are only effective for that file.
Also, DML imports are automatically idempotent, in the sense that importing the
same file twice does not yield any duplicate definitions.

The import hierarchy has semantic significance in DML: If a module
defines some method or parameter declarations that can be overridden,
then *only* files that explicitly import the module are allowed
to override these declarations. It is however sufficient to import the
module indirectly via some other module. For instance, if A.dml
contains a default declaration of a method, and B.dml wants to
override it, then B.dml must either import A.dml, or some file C.dml
that in turn imports A.dml. Without that import, it is an error to
import both A.dml and B.dml in the same device.

## Source File Structure

A DML source file describes both the structure of the modeled device and
the actions to be taken when the device is accessed.

A DML source file defining a device starts with a *language version declaration*
and a *device declaration*. After that, any number of *parameter declarations*,
*methods*, *data fields*, *object declarations*, or *global declarations* can be
written. A DML file intended to be *imported* (by an [`import`
statement](#import-declarations) in another DML file) has the same layout except
for the device declaration.

### Language Version Declaration

Every DML source file should contain a version declaration, on the form
`dml 1.4;`. The version
declaration allows the `dmlc` compiler to select the proper
versions of the DML parser and standard libraries to be used for the
file. A file can not
import a file with a different language version than its own.

The version declaration must be the first declaration in the file,
possibly preceded by comments. For example:

```
// My Device
dml 1.4;
...
```

### Device Declaration

Every DML source file that contains a device declaration is a *DML model*, and
defines a Simics device class with the specified name. Such a file may *import*
other files, but only the initial file may contain a device declaration.

The device declaration must be the first proper declaration in the file,
only preceded by comments and the language version declaration. For
example:

```
/*
 *  My New Device
 */
dml 1.4;
device my_device;
...
```

## Pragmas

DML has a syntax for pragmas: directives to the DML compiler that are orthogonal
to DML as a language, both in the sense of that they are not considered part of
DML proper, and that their use do not affect the semantics of DML (unless by
accident.) The syntax for pragmas are as follows:
<pre>
/*% <em>tag</em> ... %*/
</pre>
Where _`tag`_ specifies the pragma used, and which determines the syntax of
everything following it before the pragma is closed. Tags are case insensitive,
but are fully capitilized by convention. DMLC will print a warning if a pragma
is given with a tag that the compiler does not recognize.

A pragma may be given anywhere an inline comment may; however, the meaning of
a pragma is dependent on its placement, and a specified pragma can be completely
meaningless if not properly placed.

DMLC supports the following pragmas:

### COVERITY pragma
The `COVERITY` pragma provides a means to manually suppress defects reported by
Synopsys® Coverity® stemming from a particular DML line. A `COVERITY` pragma
has no effect unless `--coverity` is passed to DMLC, in which case it will cause
an analysis annotation to be specified for every generated C line corresponding
to the DML line that the pragma applies to.

The syntax for the `COVERITY` pragma is as follows:
<pre>
/*% COVERITY <em>event</em> <em>classification</em> %*/
</pre>
where _`classification`_ is optional. This corresponds to the following
analysis annotation in generated C:
<pre>
/* coverity[<em>event</em> : <em>classification</em>] */
</pre>
or, if _`classification`_ is omitted:
<pre>
/* coverity[<em>event</em>] */
</pre>

A DML line will be affected by every `COVERITY` pragma specified in preceding
lines, up until the first line not containing any `COVERITY` pragma. For
example:
```
/*% COVERITY unreachable %*/

/*% COVERITY var_deref_model %*/
/*% COVERITY check_return %*/ /*% COVERITY copy_paste_error FALSE %*/
some_function(...);
```
Any C line corresponding to the call to `some_function(...)` will receive
analysis annotations for `var_deref_model`, `check_return`, and
`copy_paste_error` (with `copy_paste_error` specifically being classified as a
false positive), but not any analysis annotation for `unreachable`, as the empty
line breaks the consecutive specifications of COVERITY pragmas.

## The Object Model

DML is structured around an *object model*, where each DML model
describes a single *device object*, which can contain a number of
*member objects*. Each member object can in its turn have a number of
members of its own, and so on in a nested fashion.

Every object (including the device itself) can also have
[*methods*](#methods-brief), which implement the functionality of the object,
and [*parameters*](#parameters), which are members that describe static
properties of the object.

Each object is of a certain *object type*, e.g., `bank` or
`register`. There is no way of adding user-defined object types in
DML; however, each object is in general locally modified by adding (or
overriding) members, methods and parameters - this can be viewed as
creating a local one-shot subtype for each object.

A DML model can only be instantiated as a whole: Individual
objects can not be instantiated standalone; instead, the whole
hierarchy of objects is instantiated atomically together with the model. This
way, it is safe for sibling objects in the hierarchy to assume each
other's existence, and any method can freely access state from any
part of the object hierarchy.

Another unit of instantiation in DML is the *template*. A template
contains a reusable block of code that can be instantiated in an
object, which can be understood as expanding the template's code into
the object.

Many parts of the DML object model are automatically mapped onto the
Simics *configuration object* model; most importantly, the device
object maps to a Simics configuration class, such that configuration
objects of that class correspond to instances of the DML model, and
the attribute and interface objects of the DML model map to Simics
attributes and interfaces associated with the created Simics
configuration class (See *Simics Model Builder User's Guide* for
details.)

### Device Structure

A device is made up of a number of member objects and methods, where any object
may contain further objects and methods of its own. Many object types only make
sense in a particular context and are not allowed elsewhere:

* There is exactly one [`device`](#the-device) object. It resides on the top
  level.

* Objects of type [`bank`](#register-banks), [`port`](#ports) or
  [`subdevice`](#subdevices) may only appear as part of a `device` or
  `subdevice` object.

* Objects of type [`implement`](#implements) may only appear as part of a
  `device`, `port`, `bank`, or `subdevice` object.

* Objects of type [`register`](#registers) may only appear as part of a `bank`.

* Objects of type [`field`](#fields) may only appear as part of a `register`.

* Objects of type [`connect`](#connects) may only appear as part of a `device`,
  `subdevice`, `bank`, or `port` object.

* Objects of type [`interface`](#interfaces) may only appear directly below a
  `connect` object.

* Objects of type [`attribute`](#attributes) may only appear as part of a
  `device`, `bank`, `port`, `subdevice`, or `implement` object.

* Objects of type [`event`](#events) may appear anywhere **except** as part of a
  `field`, `interface`, `implement`, or another `event`.

* Objects of type [`group`](#groups) are neutral: Any object may contain a
  `group` object, and a `group` object may contain any object that its parent
  object may contain, with the exception that a `group` cannot contain an
  object of type `interface` or `implement`.

### Parameters

Parameters (shortened as "`param`") are a kind of object members that *describe
expressions*. During compilation, any parameter reference will be expanded to
the definition of that parameter. In this sense, parameters are similar to
macros, and indeed have some usage patterns in common - in particular,
parameters are typically used to represent constant expressions.

Like macros, no type declarations are necessary for parameters, and every usage
of a parameter will re-evaluate the expression. Unlike macros, any parameter
definition must be a syntactically valid expression, and every unfolded
parameter expression is always evaluated using the scope in which the
parameter was defined, rather than the scope in which the parameter is
referenced.

Parameters cannot be dynamically updated at run-time; however, a parameter
can be declared to allow it being overridden by later definitions -
see [Parameters detailed](#parameters-detailed).

Within DML's built-in modules and standard library, parameters are used to
describe static properties of objects, such as names, sizes, and offsets. Many
of these are overridable, allowing some properties to be configured by users.
For example, every bank object has a `byte_order` parameter that controls the
byte order of registers within that bank. By default, this parameter is defined
to be `"little-endian"` - but by overriding it, users may specify the byte
order on a bank-by-bank basis.

### Methods
<a id="methods-brief"/>

Methods are object members providing implementation of the functionality of the
object. Although similar to C functions, DML methods can have any number of
input parameters and return values. DML methods also support a basic exception
handling mechanism using `throw` and `try`.

[In-detail description of method declarations are covered in a separate
section.](#methods-detailed)

### The Device

The *device* defined by a DML model corresponds
directly to a Simics *configuration object*, i.e., something
that can be included in a Simics configuration.

In DML's object hierarchy, the device object is represented by the
top-level scope.

The DML file passed to the DML compiler must *start* with a `device`
declaration following the language version specification:

<pre>
dml 1.4;
device <em>name</em>;
</pre>

A `device` declaration may not appear anywhere else, neither in the
main file or in imported files. Thus, the device declaration is
limited to two purposes:
* to give a *name* to the configuration class registered with Simics
* to declare which DML file is the top-level file in a DML model

### Register Banks

A *register bank* (or simply *bank*) is an abstraction that is used to
group *registers* in DML, and to expose these to the outside
world. Registers are exposed to the rest of the simulated system
through the Simics interface `io_memory`, and exposed to scripting and
user interfaces through the `register_view`, `register_view_read_only`,
`register_view_catalog` and `bank_instrumentation_subscribe` Simics interfaces.

It is possible to define *bank arrays* to model a row of similar banks. Each
element in the bank array is a separate configuration object in Simics, and can
thus be individually mapped in a memory space.

Simics configuration objects for bank instances are named like the bank but
with a `.bank` prefix. For instance, if a device model has a declaration `bank
regs[i < 2]` on top level, and a device instance is named `dev` in Simics, then
the two banks are represented in Simics by configuration objects named
`dev.bank.regs[0]` and `dev.bank.regs[1]`.

### Registers

A *register* is an object that contains an integer value. Normally, a register
corresponds to a segment of consecutive locations in the address space of the
bank; however, it is also possible (and often useful) to have registers that are
not mapped to any address within the bank. All registers must be part of a
register bank.

Every register has a fixed *size*, which is
an integral, nonzero number of 8-bit bytes. A single register cannot
be wider than 8 bytes. The size of the register is given by the
`size` parameter,
which can be specified either by a normal parameter assignment, as in

```
register r1 {
    param size = 4;
    ...
}
```

or, more commonly, using the following short-hand syntax:

```
register r1 size 4 {
    ...
}
```

which has the same meaning. The default size is provided by the
`register_size`
parameter of the containing register bank, if that is defined.

There are multiple ways to manipulate the value of a register: the simplest
approach is to make use of the `val` member of registers, as in:

```
log info: "the value of register r1 is %d", r1.val;
```

or

```
++r1.val;
```

For more information, see Section
[Register Objects](dml-builtins.html#register-objects).

#### Mapping Addresses To Registers

For a register to be mapped into the internal address space of the
containing bank, its starting address within the bank must be given by
setting the
<code>offset</code>
parameter. The address range occupied by the register is then from
`offset` to `offset` + `size` - 1. The offset
can be specified by a normal parameter assignment, as in

```
register r1 {
    param offset = 0x0100;
    ...
}
```

or using the following short-hand syntax:

```
register r1 @ 0x0100 {
    ...
}
```

similar to the `size` parameter above. Usually, a normal
read/write register does not need any additional specifications apart
from the size and offset, and can simply be written like this:

```
register r1 size 4 @ 0x0100;
```

or, if the bank contains several registers of the same size:

```
bank b1 {
    param register_size = 4;
    register r1 @ 0x0100;
    register r2 @ 0x0104;
    ...
}
```

The translation from the bank address space to the actual value of the
register is controlled by the `byte_order` parameter. When it is set to
`"little-endian"` (the default), the lowest address, i.e., that
defined by `offset`, corresponds to the least significant byte in
the register, and when set to `"big-endian"`, the lowest address
corresponds to the most significant byte in the register.

#### Not Mapping Addresses To Registers

An important thing to note is that registers do not have to be mapped at all.
This may be useful for internal registers that are not directly accessible from
software. By using an unmapped register, you can get the advantages of using
register, such as automatic checkpointing and register fields. This internal
register can then be used from the implementations of other registers, or other
parts of the model.

Historically, unmapped registers were commonly used to store simple device
state, but this usage is no longer recommended &mdash;
[Saved Variables](#saved-variables) should be preferred if possible.
Unmapped registers should only be used if saved variables do not fit a
particular use case.

To make a register unmapped, set the offset to `unmapped_offset`
or use the standard template `unmapped`:

```
register r is (unmapped);
```

#### Register Attributes

For every register, an attribute of integer type is automatically added
to the Simics configuration class generated from the device model. The
name of the attribute corresponds to the hierarchy in the DML model;
e.g., a register named `r1` in a bank named `bank0` will
get a corresponding attribute named `bank0_r1`.

The register value is automatically saved when Simics creates a checkpoint,
unless the `configuration` parameter indicates otherwise.

The value of a register is stored in a member named `val`. E.g., the `r1`
register will store its value in `r1.val`. This is normally the value that is
saved in checkpoints; however, checkpointing is defined by the `get` and `set`
methods, so if they are overridden, then some other value can be saved instead.

#### Fields

Real hardware registers often have a number of *fields* with
separate meaning. For example, the lowest three bits of the register
could be a status code, the next six bits could be a set of flags, and
the rest of the bits could be reserved.

To make this easy to express, a `register` object can
contain a number of `field` objects. Each field is defined
to correspond to a bit range of the containing register.

The value of a field is stored in the corresponding bits of the containing
register's storage. The easiest way to access the value of a register or field
is to use the `get` and `set` methods.

The read and write behaviour of registers and fields is in most cases
controlled by instantiating *templates*. There are three
categories of templates:

* Registers and fields where a read or write just updates the
  value with no side-effects, should use the `read`
    and `write` templates, respectively.

* Custom behaviour can be supplied by instantiating
  the `read` or `write` template. The template leaves
    a simple method `read` (or `write`) abstract;
    custom behaviour is provided by overriding the method. There is
    also a pair of templates `read_field`
    and `write_field`, which similarly provide abstract
    methods `read_field` and `write_field`. These
    functions have some extra parameters, making them less convenient
    to use, but they also offer some extra information about the
    access.


* There are many pre-defined templates with for common specialized
  behaviour. The most common ones are `unimpl`, for registers
    or fields whose behaviour has not yet been implemented,
    and `read_only` for registers or fields that cannot be
    written.



A register or field can often instantiate two templates, one for reads
and one for writes; e.g., `read` to supply a read method
manually, and `read_only` to supply a standard write method. If
a register with fields instantiates a read or write template, then the
register will use that behaviour *instead* of descending into
fields. For instance, if a register instantiates
the `read_only` template, then all writes will be captured, and
only reads will descend into its fields.

The register described above could be modeled as follows,
using the default little-endian bit numbering.

```
bank b2 {
    register r0 size 2 @ 0x0000 {
        field status @ [2:0];
        field flags @ [8:3];
        field reserved @ [15:9];
    }
    ...
}
```

Note that the most significant bit number is always the first number (to
the left of the colon) in the range, regardless of whether little-endian
or big-endian bit numbering is used. (The bit numbering convention used
in a source file can be selected by a <code>bitorder</code>
declaration.)

The value of the field can be accessed by using the `get`
and `set` methods, e.g.:

```
log info: "the value of the status field is %d", r0.status.get();
```

### Attributes

An [`attribute` object](dml-builtins.html#attribute-objects) in DML represents a
Simics configuration object attribute of the device. As mentioned above, Simics
attributes are created automatically for [`register`](#registers) and
[`connect`](#connects) objects to allow external inspection and modification;
explicit `attribute` objects can be used to expose additional data. There are
mainly three use cases for explicit attributes:

* Exposing a parameter for the end-user to configure or
  tweak. Such attributes can often be *required* in order to
  instantiate a device, and they usually come with documentation.

* Exposing internal device state, required for checkpointing to work correctly.
  Most device state is usually saved in registers or saved variables, but
  attributes may sometimes be needed to save non-trivial state such as FIFOs.

* Attributes can also be created as synthetic back-doors for
  additional control or inspection of the device. Such attributes
  are called *pseudo attributes*, and are not saved in
  checkpoints.


An attribute is basically a name with an associated pair of `get` and `set`
functions. The type of the value read and written through the get/set functions
is controlled by the `type` parameter. More information about configuration
object attributes can be found in *Simics Model Builder User's Guide*.

The [`init`](dml-builtins.html#init) template and associated method is often
useful together with `attribute` objects to initialize any associated state.

Four standard templates are provided for attributes: `bool_attr`, `int64_attr`,
`uint64_attr` and `double_attr`. They provide overridable `get` and `set`
methods, and store the attribute's value in a session variable named `val`,
using the corresponding type. For example, if `int64_attr` is used in the
attribute `a`, then one can access it as follows:

```
log info: "the value of attribute a is %d", dev.a.val;
```

These templates also provide an overridable implementation of
[`init()`](dml-builtins.html#init) that initializes the `val` session variable.
The value that `val` is initialized to is controlled by the `init_val`
parameter, whose default definition simply causes `val` to be zero-initialized.

Defining `init_val` is typically the most convenient way of initializing any
attribute instantiating any one of the these templates &mdash; however,
overriding the default `init()` implementation with a custom one may still be
desirable in certain cases. In particular, the definition of `init_val` must be
constant, so a custom `init()` implementation is necessary if `val` should be
initialized to a non-constant value.

Note that using an attribute object purely to store and checkpoint simple
internal device state is not recommended; prefer
[Saved Variables](#saved-variables) for such use cases.

### Connects

A [`connect` object](dml-builtins.html#connect-objects)
is a container for a reference to an
arbitrary Simics configuration object. An attribute with the same name
as the connect is added to the Simics configuration class generated
from the device model. This attribute can be assigned a value of type
"Simics object".

A `connect` declaration is very similar to a simple
`attribute` declaration, but specialized to handle
connections to other objects.

Typically, the connected object is expected to implement one or more
particular Simics interfaces, such as `signal`
or `ethernet_common` (see
*Simics Model Builder User's Guide* for details). This is described
using `interface` declarations inside the
`connect`.

Initialization of the connect (i.e., setting the object reference) is
done from outside the device, usually in a Simics configuration
file. Just like other attributes, the parameter
<code>configuration</code> controls whether the value must
be initialized when the object is created, and whether it is
automatically saved when a checkpoint is created.

The actual object pointer, which is of type
<code>conf_object_t*</code> is stored in a `session`
member called `obj`.  This means that to access the current
object pointer in a connect called *otherdev*, you need to
write `otherdev.obj`.

If the `configuration` parameter is not `required`,
the object pointer may have a null value, so any code that tries to
use it must check if it is set first.

This is an example of how a connect can be declared and used:

```
connect plugin {
    param configuration = "optional";
}

method mymethod() {
    if (plugin.obj)
        log info: "The plugin is connected";
    else
        log info: "The plugin is not connected";
}
```

#### Interfaces

In order to use the Simics interfaces that a connected object
implements, they must be declared within the `connect`.
This is done through [`interface` objects](dml-builtins.html#interface-objects).
These name the expected interfaces and may also specify additional properties.

An important property of an interface object is whether or not a
connected object is *required* to implement the interface. This
can be controlled through the interface parameter `required`,
which is `true` by default. Attempting to connect an object
that does not implement the required interfaces will cause a runtime
error. The presence of optional interfaces can be verified by testing
if the `val` member of the interface object has a null
value.

By default, the C type of the Simics interface corresponding to a
particular interface object is assumed to be the name of the object
itself with the string `"_interface_t"` appended. (The C type is
typically a `typedef`:ed name for a struct containing function
pointers).

The following is an example of a connect with two interfaces, one of
which is not required:

```
connect plugin {
    interface serial_device;
    interface rs232_device { param required = false; }
}
```

Calling interface functions is done in the same way as any C function
is called, but the first argument which is the target object
pointer is omitted.

The `serial_device` used above has a function with the
following definition:

```
int (*write)(conf_object_t *obj, int value);
```

This interface function is called like this in DML:

```
method call_write(int value) {
    local int n = plugin.serial_device.write(value);
    // code to check the return value omitted
}
```

### Implements

When a device needs to export a Simics interface, this is specified by an
`implement` object, containing the methods that implement
the interface. The name of the object is also used as the name of the
Simics interface registered for the generated device, and the names and
signatures of the methods must correspond to the C functions of the
Simics interface. (A device object pointer is automatically added as the
first parameter of the generated C functions.)

In most cases, a device exposes interfaces by adding `implement` object as
subobjects of named [`port` objects](#ports). A port object often represents a
hardware connection

The C type of the Simics interface is assumed to be the
value of the object's `name` parameter (which defaults to the name of
the object itself), with the string `"_interface_t"` appended.  The C
type is typically a `typedef`:ed name for a struct containing function
pointers.

For example, to implement the `ethernet_common` Simics interface, we can write:

```
implement ethernet_common {
    method frame(const frags_t *frame, eth_frame_crc_status_t crc_status) {
        ...
    }
}
```

This requires that `ethernet_common_interface_t` is defined as a struct type
with a field `frame` with the function pointer type
`void (*)(conf_object_t *, const frags_t *, eth_frame_crc_status_t)`.

Definitions of all standard Simics interface types are available as DML files named like the corresponding C header files;
for instance, the `ethernet_common` interface can be imported as follows:
```
import "simics/devs/ethernet.dml"
```

### Events

An *event* object is an encapsulation of a Simics event that can
be posted on a processor time or step queue. The location of event
objects in the object hierarchy of the device is not important, so an
event object can generally be placed wherever it is most convenient.

An event has a built-in `post` method, which inserts the
event in the default queue associated with the device. An event also
defines an abstract method `event`, which the user must
implement. That method is called when the event is triggered.

An event must instantiate one of six predefined
templates: `simple_time_event`, `simple_cycle_event`,
`uint64_time_event`, `uint64_cycle_event`,
`custom_time_event` or `custom_cycle_event`. The choice
of template affects the signature of the `post`
and `event` methods: In a time event, the delay is specified
as a floating-point value, denoting number of seconds, while in a
cycle event, the delay is specified in CPU cycles.

A posted event may have data associated with it. This data is given to
the `post` method and is provided to the `event`
callback method. They type of data depends on the template used: No
data is provided in simple events, and in uint64 events it is provided
as a uint64 parameter. In custom events, data is provided as
a `void *` parameter, and extra
methods `get_event_info` `set_event_info`
and `destroy` must be provided in order to provide proper
checkpointing of the event.

### Groups

Objects of type `attribute`, `connect`, `event`, `field`, `register`, `bank`,
`port` and `subdevice` can be organized into *groups*. A group is a neutral
object, which can be used just for namespacing, or to help structuring an array
of a collection of objects. Groups may appear anywhere, but are most commonly
used to group registers: If a bank has a sequence of blocks, each containing
the same registers, it can be written as a group array. In the following
example eight homogeneous groups of registers are created, resulting in
8&#215;6 instances of register `r3`.

```
bank regs {
    param register_size = 4;
    group blocks[i < 8] {
        register r1 @ i * 32 + 0;
        register r2 @ i * 32 + 4;
        register r3[j < 6] @ i * 32 + 8 + j * 4;
    }
}
```

Another typical use of `group` is in combination with a
template for the group that contains common registers and
more that are shared between several groups, as in the following
example.

```
template weird {
    param group_offset;
    register a size 4 @ group_offset is (read, write);
    register b size 4 @ group_offset + 4 is (read, write) {
        method read() -> (uint64) {
            // When register b is read, return a
            return a.val;
        }
    }
}

bank regs {
    group block_a is (weird) { param group_offset = 128; }
    group block_b is (weird) { param group_offset = 1024; }
}
```

In addition, groups can be nested.

```
bank regs {
    param register_size = 4;
    group blocks[i < 8] {
        register r1 @ i * 52 + 0;
        group sub_blocks[j < 4] {
            register r2 @ i * 52 + j * 12 + 4;
            register r3[k < 3] @ i * 52 + j * 12 + k * 4 + 8;
        }
    }
}
```

Banks, ports and subdevices can be placed inside groups; in this case, the
Simics configuration object that represents the bank, port or subdevice will be
placed under a namespace object; for instance, if a device with `group g { bank
regs; }` is instantiated as `dev`, then the bank is represented by an object
`dev.g.bank.regs`, where `g` and `bank` are both `namespace` objects.

As groups have no special properties or restrictions, they can be used as a tool
for building abstractions &mdash; in particular in combination with templates.

For example, a template can be used to create an abstraction for finite state
machine objects, by letting users create FSMs by declaring group objects
instantiating that template. FSM states can also be represented through a
template instantiated by groups.

#### Finite State Machines using groups

The following demonstrates a simple example of how a finite state machine may be implemented using templates and group objects:

```
// Template for finite state machines
template fsm is init {
    saved fsm_state curr_state;

    // The initial FSM state.
    // Must be defined by any object instantiating this template.
    param init_fsm_state : fsm_state;

    shared method init() default {
        curr_state = init_fsm_state;
    }

    // Execute the action associated to the current state
    shared method action() {
        curr_state.action();
    }
}

// Template for states of an FSM. Such states must be sub-objects
// of an FSM.
template fsm_state {
    param parent_fsm : fsm;
    param parent_fsm = cast(parent, fsm);

    // The action associated to this state
    shared method action();

    // Transitions the parent FSM to this state
    shared method set() {
        parent_fsm.curr_state = cast(this, fsm_state);
    }
}
```

These templates can then be used as follows:

```
group main_fsm is fsm {
    param init_fsm_state = cast(init_state, fsm_state);

    group init_state is fsm_state {
        method action() {
          log info: "init_state -> second_state";
          // Transition to second_state
          second_state.set();
        }
    }

    group second_state is fsm_state {
        method action() {
          log info: "second_state -> final_state";
          // Transition to final_state
          final_state.set();
        }
    }

    group final_state is fsm_state {
        method action() {
            log info: "in final_state";
        }
    }
}

method trigger_fsm() {
    // Execute the action of main_fsm's current state.
    main_fsm.action();
}
```

### Ports

An interface port is a structural element that groups implementations
of one or more interfaces. When one configuration object connects to
another, it can connect to one of the target object's ports, using the
interfaces in the port. This way, the device model can expose
different interfaces to different objects.

Sometimes a port is as simple as a single pin, implementing
the `signal` interface, and sometimes it can be more complex,
implementing high-level communication interfaces.

It is also possible to define port arrays that are indexed
with an integer parameter, to model a row of similar connectors.

In Simics, a port is represented by a separate configuration object, named like
the port but with a `.port` prefix. For instance, if a device model has a
declaration `port p[i<2]` on top level, and a device instance is named `dev` in
Simics, then the two ports are represented in Simics by objects named
`dev.port.p[0]` and `dev.port.p[1]`.

### Subdevices

A subdevice is a structural element that represents a distinct subsystem of the
device. Like a `group`, a subdevice can be used to group a set of related
banks, ports and attributes, but a subdevice is presented to the end-user as a
separate configuration object. If a subdevice contains `attribute` or `connect`
objects, or `saved` declarations, then the corresponding configuration
attributes appears as members of the subdevice object rather than the device.

## Templates

<pre>
template <em>name</em> { ... }
</pre>

Defines a *template*, a piece of code that can be reused in
multiple locations. The body of the template contains a number of
declarations that will be added to any object that uses the template.

Templates are imported into an object declaration body using
`is` statements, written as

<pre>
is <em>name</em>;
</pre>
for example:
```
field F {
    is A;
}
```

It is also possible to use templates when declaring an object, as in

<pre>
field F is (<em>name1</em>, <em>name2</em>);
</pre>

These can be used in any context where an object declaration may be written, and
has the effect of expanding the body of the template at the point of the `is`.
Using `is` together with object declarations is typically more idiomatic than
the standalone `is` object statement; however, the latter is useful in order
to instantiate templates in the top-level device object, and also for use in
conjunction with [`in each` declarations](#in-each-declarations); for example:

```
register r {
    in each field {
        is A;
    }

    field F1 @ [7:6];
    ...
}
```

If two templates define methods or parameters with the same name, then the
template instantiation hierarchy is used to deduce which method overrides the
other: If one template *B* instantiates another template *A*, directly or
indirectly, then methods from *B* override methods from *A*. Note, however, that
overrides can only happen on methods and parameters that are declared `default`.
Example:

```
template A {
    method hello() default {
        log info: "hello";
    }
}
template B is A {
    // this method overrides the
    // method from A
    method hello() default {
        default();
        log info "world";
    }
}
```

See [Resolution of Overrides](#resolution-of-overrides) for a formal
specification of override rules.

### Templates as types

Each template defines a *type*, which is similar to a class
in an object oriented language like Java. The type allows you to
store references to a DML object in a variable. Some, but not all,
top-level declarations inside a template appear as members of the template type.
A template type has the following members:

* All [session](#session-variables) and [saved](#saved-variables) variables
  declared within the template. E.g., the declaration `session int val;` gives a
  type member `val`.

* All declarations of typed parameters, further discussed below.
  E.g., the declaration `param foo : uint64;` gives a type member `foo`.

* All method declarations declared with the `shared` keyword,
  further discussed below. E.g., the declaration
  `shared method fun() { ... }`
  gives a type member `fun`, which can be called.

* Every `shared` [hook](#hook-declarations) declared within the template.
  E.g. the declaration `shared hook(int, bool) h;` gives a type member `h`.

* All type members of inherited templates. E.g., the declaration
  `is simple_time_event;`
  adds two type members `post` and `next`, since
  `post` and `next` are members of
  the `simple_time_event` template type.

* The `templates` member, which permits [template-qualified method
  implementation calls](#template-qualified-method-implementation-calls) to
  the `shared` method implementations of the template type's ancestor templates.

Template members are dereferenced using the `.` operator,
much like struct members.

A template's type is named like the template, and an object
reference can be converted to a value using the `cast`
operator.  For instance, a reference to the
register `regs.r0` can be created and used as follows
(all register objects automatically implement the template `register`):

```
local register x = cast(regs.r0, register);
x.val = 14;  // sets regs.r0.val
```

Two values of the same template type can be compared for equality, and are
considered equal when they both reference the same object.

A value of a template type can be upcast to an ancestor template type; for
example:
```
local uint64_attr x = cast(attr, uint64_attr);
local attribute y = cast(x, attribute);
```
In addition, a value of any template type can be cast to the template type
`object`, even if `object` is not an ancestor of the template.

### Shared methods
If a method is declared in a template, then one copy of the method
will appear in each object where the template is instantiated;
therefore, the method can access all methods and parameters of that
object. This is often convenient, but comes with a cost; in
particular, if a template is instantiated in many objects, then this
gives unnecessarily large code size and slow compilation. To address
this problem, a method can be declared *shared* to operate on
the template type rather than the implementing object. The
implementation of a shared method is compiled once and shared between
all instances of the template, rather than duplicated between
instances.

Declaring a method as shared imposes restrictions on its
implementation, in particular which symbols it is permitted to access:
Apart from symbols in the global scope, a shared method may only
access members of the template's type; it is an error to access any
other symbols defined by the template. Members can be referenced
directly by name, or as fields of the automatic `this`
variable.  When accessed in the scope of the shared method's body,
the `this` variable evaluates to a value whose type is the
template's type.

Example:

```
template base {
    // abstract method: must be instantiated in sub-template or object
    shared method m(int i) -> (int);
    shared method n() -> (int) default { return 5; }
}
template sub is base {
    // override
    shared method m(int i) -> (int) default {
        return i + this.n();
    }
}
```

If code duplication is not a concern, it is possible to define a shared method
whose implementation is not subject to above restrictions while still retaining
the benefit of having the method be a member of the template type.
This is done by defining the implementation separately from the declaration
of the shared method, for example:
```
template get_qname {
    shared method get_qname() -> (const char *);
    method get_qname() -> (const char *) {
        // qname is an untyped parameter, and would thus not be accessible
        // within a shared implementation of get_qname()
        return this.qname;
    }
}
```

## Parameters detailed

Parameters may be typed or untyped.
Typed parameter declarations may only appear in template definitions.

Parameters are declared using one of the forms

```
param name;
param name: type
```

and assigned using the `=` operator. Parameters may also be given default values using the form `param name default expr`.
For example:

```
param offset = 8;
param byte_order default "little-endian";
```

A default value is overridden by an assignment (`=`).
There can be at most one assignment for each parameter.
Typically, a default value for a parameter is specified in a template, and the programmer may then choose to override it where the template is used.
See [Resolution of overrides](#resolution-of-overrides) for the resolution order when there are multiple definitions of the same parameter.

A parameter that is declared without an assignment or a default value must eventually be assigned elsewhere, or the model will not compile.
This pattern is sometimes useful in templates, as in:

```
template constant is register {
    param value;
    method get() -> (uint64) {
        return value;
    }
}
```

so that wherever the template `constant` is used, the programmer
is also forced to define the parameter `value`. E.g.:

```
register r0 size 2 @ 0x0000 is (constant) {
    param value = 0xffff;
}
```

> [!IMPORTANT]
> When writing templates, always declare parameters that are referenced.
> Enabling the provisional feature `explicit_param_decls` enforces this.
>
> Leaving out the parameter declaration from the template
> definition can have unwanted effects if the programmer forgets to
> specify its value where the template is used. At best, it will only
> cause a more obscure error message, such as "unknown identifier"; at
> worst, the scoping rules will select an unrelated definition of the same
> parameter name.

### `explicit_param_decls` provisional feature

There is a shorthand syntax for combined declaration and definition of a parameter, currently enabled by the [`explicit_param_decls` provisional feature](provisional-auto.html#explicit_param_decls):

```
param NAME: TYPE = value;
param NAME: TYPE default value;
param NAME := value;
param :default value;
```

`explicit_param_decls` enforces that parameters are declared before they are assigned, or that the combined syntax is used.
This distinguishes between the intent to declare a new parameter, and the intent to override an existing parameter.
This distinction allows DML to capture misspelled parameter overrides as compile errors.

DMLC signals an error if the combined declaration and definition syntax is used to override an existing parameter.
This guards against unintentional reuse of a parameter name. An example:

```
// Included file not using explicit_param_decls
template foo_capability {
    param foo_offset; // parameter which must be assigned by the device instantiating the template
    param has_foo_feature default false; // overridable parameter with a default value
    param id default 1; // overridable parameter with a generic name
}
```

```
// Device model file including the above
provisional explicit_param_decls;

template extended_foo_capability is foo_capability {
    param has_bar_feature: bool default false;

    // unintentional reuse of parameter name:
    param id := 5; // error: the parameter 'id' has already been declared
}

bank foo_config {
    is extended_foo_capability;

    // correct assignment to template parameter:
    param foo_offset = 0x10;

    // misspelled parameter override:
    param has_foo_featur = true;  // error: parameter 'has_foo_featur' not declared previously.
}
```

It is recommended to enable `explicit_param_decls` in new DML source files and to use the new combined syntax when applicable to reduce the risk of bugs caused by misspelled parameters.

In some rare cases, you may need to declare a parameter without
knowing if it's an override or a new parameter. In this case, one
can accompany a `param NAME = value;` or `param NAME default
value;` declaration with a `param NAME;` declaration in the same
scope/rank. This marks that the parameter assignment may be either
an override or a new parameter, and no error will be printed.

### Typed Parameters detailed
A typed parameter declaration adds a member to the template type with the same
name as the specified parameter, and with the specified type. That member is
associated with the specified parameter, in the sense that the definition of the
parameter is used as the value of the template type member.

A typed parameter declaration places a number of requirements on the
named parameter:
* The named parameter must be defined (through a regular [parameter
  declaration](#parameters-detailed)). This can be done either within the
  template itself, within sub-templates, or within individual objects
  instantiating the template.
* The parameter definition must be a valid expression of the specified type.
* The parameter definition must be free of side-effects, and must not rely on
  the specific device instance of the DML model &mdash; in particular, the
  definition must be independent of device state.

  This essentially means that the definition must be a constant expression,
  except that it may also make use of device-independent expressions whose
  values are known to be constant. For example, index parameters, [`each`-`in`
  expressions](#each-in-expressions), and object references cast to template
  types are allowed. It is also allowed to reference other parameters that obey
  this rule.

  Examples of expressions that may *not* be used include method calls and
  references to `session`/`saved` variables.
* The parameter definition must not contain calls to
  [independent methods](#independent-methods).

Typed parameters are most often used to allow a shared method defined within
the template to access parameters of the template. For example:

```
template max_val_reg is write {
    param max_val : uint64;

    shared method write(uint64 v) {
        if (v > max_val) {
            log info: "Ignoring write to register exceeding max value %u",
                      max_val;
        } else {
            default(v);
        }
    }
}

bank regs {
    register reg[i < 2] size 8 @0x0 + i*8 is max_val_reg {
        param max_val = 128 * (i + 1) - 1;
    }
}
```

### Special parameters in the DML standard library

You may see the following special form in some standard library files:

<pre>
param <em>name</em> auto;
</pre>

for example,

```
param parent auto;
```

This is used to explicitly declare the built-in automatic parameters,
and should never be used outside the libraries.

## Data types

The type system in DML builds on the type system in C, with a few
modifications.  There are eight kinds of data types.  New names for
types can also be assigned using a `typedef` declaration.

<dl><dt>

Integers
</dt><dd>

Integer types guarantee a certain *minimum* bit width and
may be signed or unsigned.  The basic integer types are
named `uint1`,
`uint2`, ..., `uint64` for the unsigned types, and
`int1`, `int2`, ..., `int64` for the signed
types. Note that the size of the integer type is only a hint and the
type is guaranteed to be able to hold at least that many
bits. Assigning a value that would not fit into the type is
undefined, thus it is an error to assume that values will be
truncated. For bit-exact types, refer to `bitfields`
and `layout`.

The familiar integer types `char` and `int` are
available as aliases for `int8` and `int32`,
respectively. The C keywords `short`, `signed` and `unsigned`
are reserved words in DML and not allowed in type
declarations.

The types `size_t` and `uintptr_t`, `long`, `uint64_t`, `int64_t`, are defined
as in C. The types `long`, `uint64_t` and `int64_t` are provided mainly for
compatibility with third party libraries; they are needed because they are
incompatible with the corresponding Simics types (`uint64`, etc) on some
platforms.  </dd><dt>

Endian integers
</dt><dd>

Endian integer types hold similar values as
integer types, but in addition have the following attributes:

* *They are guaranteed to be stored in the exact number of bytes
  required for their bitsize, without padding.*

* *They have a defined byte order.*

* *They have a natural alignment of 1 byte.*

Endian integer types are named after the integer type
with which they share a bitsize and sign but in addition
have a `_be_t` or `_le_t` suffix, for big-endian
and little-endian integers, respectively. The full list of endian types is:

<!-- A markdown table would have been nice here, but that's currently (2021)
too poorly rendered (SIMICS-18374) -->
```
int8_be_t    int8_le_t    uint8_be_t    uint8_le_t
int16_be_t   int16_le_t   uint16_be_t   uint16_le_t
int24_be_t   int24_le_t   uint24_be_t   uint24_le_t
int32_be_t   int32_le_t   uint32_be_t   uint32_le_t
int40_be_t   int40_le_t   uint40_be_t   uint40_le_t
int48_be_t   int48_le_t   uint48_be_t   uint48_le_t
int56_be_t   int56_le_t   uint56_be_t   uint56_le_t
int64_be_t   int64_le_t   uint64_be_t   uint64_le_t
```

These types can be transparently
used interchangeably with regular integer types, values of one
type will be coerced to the other as needed. Note that operations
on integers will always produce regular integer types, even
if all operands are of endian integer type.

</dd><dt>

Floating-point numbers
</dt><dd>

There is only one floating-point type, called `double`.
It corresponds to the C type `double`.
</dd><dt>

Booleans
</dt><dd>

The boolean type `bool` has two values, `true` and
`false`.
</dd><dt>

Arrays
</dt><dd>

An array is a sequence of elements of another type, and works as
in C.
</dd><dt>

Pointers
</dt><dd>

Pointers to types, work as in C. String literals have the
type `const char *`. A pointer has undefined meaning
if the pointer target type is an integer whose bit-width is neither 8,
16, 32, nor 64.
</dd><dt>

Structures
</dt><dd>

A `struct` type defines a composite type that contains
named members of different types.  DML makes no assumptions about the
data layout in struct types, but see the layout types below for that.
Note that there is no struct label as in C, and struct member declarations
are permitted to refer to types that are defined further down in the file.
Thus, new struct types can always be declared using the following syntax:
<pre>
typedef struct { <em>member declarations</em> } <em>name</em>;
</pre>

</dd><dt>

Layouts
</dt><dd>
<a id="layouts"/>

A layout is similar to a struct in many ways.  The important
difference is that there is a well-defined mapping between a layout
object and the underlying memory representation, and layouts may
specify that in great detail.

A basic layout type looks like this:

```
layout "big-endian" {
    uint24 x;
    uint16 y;
    uint32 z;
}
```

By casting a pointer to a piece of host memory to a pointer of this
layout type, you can access the fourth and fifth byte as a 16-bit
unsigned integer with big-endian byte order by simply writing
`p->y`.

The allowed types of layout members in a layout type declaration
are integers, endian integers, other layout types,
bitfields (see below), and arrays of these.

The byte order declaration is mandatory, and is
either `"big-endian"` or `"little-endian"`.

Access of layout members do not always provide a value of the
type used for the member in the declaration. Bitfields and
integer members (and arrays of similar) are translated
to endian integers (or arrays of such) of similar size,
with endianness matching the layout. Layout and endian integer
members are accessed normally.

The *discard identifer* `_` may be used as the name of any number of members
within a layout, making these *anonymous*. Anonymous layout members cannot be
referenced within DML code, but will still influence the underlying memory
representation of the layout in the same way as regular members.
This is useful to represent reserved or padding bytes, or bytes that the device
otherwise doesn't study or manipulate.

Note that when a compound initializer is given for a variable of layout type,
an initializer must still be given for each anonymous member:
```
local layout "little-endian" { uint32 x; uint32 _; uint32 y} = {1,0,2};
```
... unless designated initializers are used, in which case anonymous members
can (and must) be omitted:
```
local layout "little-endian" { uint32 x; uint32 _; uint32 y} = {.x = 1, .y = 2};
```
</dd><dt>

Bitfields
</dt><dd>

A bitfield type works similar to an integer type where you use bit
slicing to access individual bits, but where the bit ranges are
assigned names. A `bitfields` declaration looks like this:

```
bitfields 32 {
    uint3  a @ [31:29];
    uint16 b @ [23:8];
    uint7  c @ [7:1];
    uint1  d @ [0];
}
```

The bit numbering is determined by the `bitorder` declaration
in the current file.

Accessing bit fields is done as with a struct or layout, but the whole
bitfield can also be used as an unsigned integer. See the following
example:

```
local bitfields 32 { uint8 x @ [7:0] } bf;
bf = 0x000000ff;
bf.x = bf.x - 1;
local uint32 v = bf;
```

</dd></dl>

### Serializable types
_Serializable types_ are types that the DML compiler knows how to serialize and
deserialize for the purposes of checkpointing. This is important for the use of
[`saved` variables](#saved-variables) and the [`after`
statement](#after-statements).

All primitive non-pointer data types (integers, floating-point types, booleans,
etc.) are considered serializable, as is any struct, layout, or array type
consisting entirely of serializable types. [Template types](#templates-as-types)
and [hook reference types](#hook-declarations) are also considered serializable.

Any type not fitting the above criteria is not considered serializable:
in particular, any pointer type is not considered serializable, nor is any
[`extern`](#typedef-declarations) struct type; the latter is because it's
impossible for the compiler to ensure it's aware of all members of the struct
type.

## Methods
<a id="methods-detailed"/>

Methods are similar to C functions, but also have an implicit
(invisible) parameter which allows them to refer to the current device
instance, i.e., the Simics configuration object representing the device.
Methods also support exception handling in DML, using `try` and
`throw`. The body of the method is a compound statement in an
[extended subset of C](#comparison-to-c).
It is an error to have more than one method declaration using the same
name within the same scope.

### Input Parameters and Return Values

A DML method can have any number of return values, in contrast to C
functions which have at most one return value. DML methods do not use
the keyword `void` — an empty pair of parentheses always
means "zero parameters". Furthermore, lack of return value can even be
omitted. Apart from this, the parameter declarations of a method are
ordinary C-style declarations.

For example,

```
method m1() -> () {...}
```

and

```
method m1() {...}
```

are equivalent, and define a method that takes no input parameters and
returns nothing.

```
method m2(int a) -> () {...}
```

defines a method that takes a single input parameter, and also returns
nothing.

```
method m3(int a, int b) -> (int) {
    return a + b;
}
```

defines a method with two input parameters and a single return value.
A method that has a return value must end with a return statement.

```
method m4() -> (int, int) {
    ...;
    return (x, y);
}
```

has no input parameters, but two return values.

A method that can throw an exception must declare so, using
the `throws` keyword:

```
method m5(int x) -> (int) throws {
    if (x < 0)
        throw;
    return x * x;
}
```

### Default Methods

A parameter or method can now be overridden more than once.

When there are multiple declarations of a parameter, then the template
and import hierarchy are used to deduce which declaration to use: A
declaration that appears in a block that instantiates a template will
override any declaration in that template, and a declaration that
appears in a file that imports another file will override any
declaration from that file. The declarations of one parameter must
appear so that one declaration overrides all other declarations of he
parameter; otherwise the declaration is considered ambiguous and an
error is signalled.

Examples: A file `common.dml` might contain:

```
param num_banks default 2;
bank banks[num_banks] {
    ...
}
```

Your device `my-dev.dml` can then contain:

```
device my_dev;
import "common.dml";
// overrides the declaration in common.dml
param num_banks = 4;
```

The assignment in `my-dev.dml` takes precedence,
because `my-dev.dml` imports `common.dml`.

Another example: The following example gives an compile error:

```
template my_read_constant {
    param value default 0;
    ...
}
template my_write_constant {
    param value default 0;
    ...
}
bank b {
    // ERROR: Two declarations exist, and neither takes precedence
    register r is (my_read_constant, my_write_constant);
}
```

The conflict can be resolved by declaring the parameter a third time,
in a location that overrides both the conflicting declarations:

```
bank b {
    register r is (my_read_constant, my_write_constant) {
        param value default 0;
    }
}
```

Furthermore, an assignment (`=`) of a parameter may not be
overridden by another declaration.

If more than one declaration of a method appears in the same object,
then the template and import hierarchies are used to deduce the
override order. This is done in a similar way to how parameters are
handled:

* A method declaration that appears in a block that instantiates a
  template will override any declaration from that template

* A method declaration that appears in a file that imports another
  file will override any declaration from that file.

* The declarations of one method must appear so that one
  declaration overrides all other declarations of the method; otherwise
  the declaration is considered ambiguous and an error is signalled.

* A method can only be overridden by another method if it is declared
  `default`.

> [!NOTE]
> An overridable built-in method is defined by a template
> named as the object type. So, if you want to write a template that
> overrides the `read` method of a register, and want to make
> your implementation overridable, then your template must explicitly
> instantiate the `register` template using a statement `is
> register;`.

### Calling Methods

In DML, a method call looks much like in C, with some exceptions. For instance,

```
(a, b) = access(...);
```

calls the method 'access' in the same object, assigning the return values to
variables `a` and `b`.

If one method overrides another, it is possible to refer to the overridden
method from within the body of the overriding method using the identifier
`default`:

```
x = default(...);
```

In addition to `default`, there exists the [`templates` member of
objects](#template-qualified-method-implementation-calls) which allows for
calling the particular implementation of a method as provided by a specified
template. This is particularly useful when `default` _can't_ be used due to the
method overriding implementations provided by multiple hierarchically unrelated
templates, such that `default` can't be unambiguously resolved (see [Resolution
of overrides](#resolution-of-overrides).) Unlike `default`, `templates` can also
be used even outside the body of the overriding method.

DML supports _compound initializer syntax_ for the arguments of called methods,
meaning arguments of struct-like types can be constructed using
<tt>{<em>...</em>}</tt>. For example:
```
typedef struct {
    int x;
    int y;
} struct_t;

method copy_struct(struct_t *tgt, struct_t src) {
    *tgt = src
}

method m() {
    local struct_t s;
    copy_struct(&s, {1, 4});
    copy_struct(&s, {.y = 1, .x = 4});
    copy_struct(&s, {.y = 1, ...}); // Partial designated initializer
}
```
This syntax can't be used for variadic arguments or [inline
arguments](#inline-methods).

### Inline Methods

Methods can also be defined as inline, meaning that at
least one of the input arguments is declared `inline` instead
of declaring a type. The method body is re-evaluated every time it is
invoked, and when a constant is passed for an inline argument, it will
be propagated into the method as a constant.

Inline methods were popular in previous versions of the language, when
constant folding across methods was a useful way to reduce the size of
the compiled model. DML 1.4 provides better ways to reduce code size,
and inline methods remain mainly for compatibility reasons.

### Exported Methods

In DML 1.4, methods can be `exported` using the
[`export` declaration](#export-declarations).

### Retrieving Function Pointers to Methods

In DML 1.4, [method references can be converted to function pointers using
`&`](#method-references-as-function-pointers).

### Independent Methods

Methods that do not rely on the particular instance of the device model may
be declared `independent`:
```
independent method m(...) -> (...) {...}
```
[Exported](#export-declarations) independent methods do not have the input
parameter corresponding to the device instance, allowing them to be called
in greater number of contexts. The body of independent methods may not contain
statements or expressions that rely on the device instance in any way; for
example, `session` or `saved` variables may not be referenced, `after` and `log`
statements may not be used, and non-`independent` methods may not be called.

Within a template, `shared` independent methods may be declared.

When independent methods are used as callbacks, it can sometimes be desirable to
mutate device state. In order to do this safely, device state should be mutated
within a method not declared `independent`, which can called from independent
methods [through the use of `&`](#method-references-as-function-pointers).
Device state should not be mutated directly within an independent method as this
could cause certain Simics breakpoints to not function correctly; for example,
an independent method should not mutate a session variable through a pointer to
that variable.

#### Independent Startup Methods

Independent methods may also be declared `startup`, which causes them to be
called when the model is loaded into the simulation, *before* any device is
created. In order for this to be possible, `independent startup methods` may not
have any return values nor be declared `throw`s. In addition, independent
startup methods may not be declared with an overridable definition due to
technical limitations &mdash; this restriction can be worked around by having an
independent startup method call an overridable independent method. Note that
abstract `shared` independent startup methods are allowed.

The order in which independent startup methods are implicitly called at model
load is not defined, with the exception that independent startup methods not
declared memoized are called before any independent startup methods that are.

#### Independent Startup Memoized Methods
Independent startup methods may also be declared `memoized`. Unlike regular
`independent startup` methods, `independent startup memoized` methods may
&mdash; indeed, are required to &mdash; have return values and/or be
declared `throws`.

After the first call of a memoized method, all subsequent calls for the
simulation session return the results of the first call without executing the
body of the method. If a memoized method call throws, then subsequent calls will
throw without executing the body.

The first call to an independent startup memoized method will typically be the
one implicitly performed at model load, but it may also occur beforehand (for
example, if the method is called as part of another independent startup method).

Result caching is shared across all instances of the device model. This
mechanism can be used to compute device-independent data which is then shared
across all instances of the device model.

The results of `shared` memoized methods are cached per template instance, and
are not shared across all objects instantiating the template.

(Indirectly) recursive memoized method calls are not allowed; the result of
such a call is a run-time critical error.

## Session variables

A *session* declaration creates a number of named storage locations for
arbitrary run-time values. The names belongs to the same namespace as
objects and methods. The general form is:

<pre>
session <em>declarations</em> = <em>initializer</em>;
</pre>

where *`= initializers`* is optional
and *`declarations`* is a variable declaration similar to C, or
a sequence of such declarations; for example,

```
session int id = 1;
session bool active;
session double table[4] = {0.1, 0.2, 0.4, 0.8};
session (int x, int y) = (4, 3);
session conf_object_t *obj;
```

In the absence of explicit initializer expressions, a default
"all zero" initializer will be applied to each declared object.

Note that the number of initializers &mdash; together given as a tuple
&mdash; must match the number of declared variables.
In addition, the number of elements in each initializer must match with
the number of elements or fields of the type of the declared *session*
variable. This also implies that each sub-element, if itself being a
compound data structure, must also be enclosed in braces.

C99-style designated initializers are supported for `struct`, `layout`, and
`bitfields` types:
```
typedef struct { int x; struct { int i; int j; } y; } struct_t;
session struct_t s = { .x = 1, .y = { .i = 2, .j = 3 } }
```
Unlike C, partial initialization is not allowed implicitly; a designated
initializer for each member must be specified.
However, partial initialization can be done explicitly through the use of
trailing `...` syntax:
```
session struct_t s = { .y = { .i = 2, ... }, ... }
```

Also unlike C, designator lists are not supported, and designated initializers
for arrays are not supported.

> [!NOTE]
> Previously `session` variables were known as `data`
> variables.

## Saved variables

A *saved* declaration creates a named storage location for an
arbitrary run-time value, and automatically creates an attribute
that checkpoints this variable. Saved variables can be declared in
object or statement scope, and the name will belong to the namespace
of other declarations in that scope. The general form is:

<pre>
saved <em>declaration</em> = <em>initializer</em>;
</pre>

where *`= initializer`* is optional
and *`declaration`* is similar to a C variable
declaration; for example,

```
saved int id = 1;
saved bool active;
saved double table[4] = {0.1, 0.2, 0.4, 0.8};
```

In the absence of explicit initializer expression, a default
"all zero" initializer will be applied to the declared object.

Note that the number of elements in the initializer must match with
the number of elements or fields of the type of the *saved*
variable. This also implies that each sub-element, if itself being a
compound data structure, must also be enclosed in braces.

C99-style designated initializers are supported for `struct`, `layout`, and
`bitfields` types:
```
typedef struct { int x; struct { int i; int j; } y; } struct_t;
saved struct_t s = { .x = 1, .y = { .i = 2, .j = 3 } }
```
Unlike C, partial initialization is not allowed implicitly; a designated
initializer for each member must be specified.
However, partial initialization can be done explicitly through the use of
trailing `...` syntax:
```
session struct_t s = { .y = { .i = 2, ... }, ... }
```

Also unlike C, designator lists are not supported, and designated initializers
for arrays are not supported.

In addition, the types of saved declaration variables are currently
restricted to primitive data types, or structs or arrays containing
only data types that could be saved. Such types are called
[*serializable*](#serializable-types).

> [!NOTE]
> Saved variables are primarily intended for making checkpointable
> state easier. For configuration, `attribute` objects should
> be used instead. Additional data types for saved declarations are planned to
> be supported.

## Hook Declarations
<pre>
hook(<em>msgtype1</em>, ... <em>msgtypeN</em>) <em>name</em>;
</pre>
A *hook* declaration defines a named object member to which *suspended
computations* may be attached for execution at a later point. By sending a
*message* through the hook, every computation suspended on the hook will become
detached from the hook, and then executed &mdash; receiving the message as data.
Computations suspended on a hook are executed in order of least recently
attached; in other words, FIFO semantics.

Currently, the only computations that can be suspended and attached to hooks are
single method calls, which is done through the use of the [`after`
statement](#after-statements). This will later be expanded upon: hooks will play
a central role in the future introduction of *coroutines*, as hooks will serve
as the primitive mechanism through which coroutines suspend themselves and
become resumed.

Every hook has an associated list of *message component types*, specified during
declaration through the <tt>(<em>msgtype1</em>, ... <em>msgtypeN</em>)</tt>
syntax. This specifies what form of data is sent and received via the hook. Any
number of message component types can be given, including zero, in which case a
message sent via the hook has no associated data.

Example declarations:
```
// Hook with no associated message component types
hook() h1;
// Hook with a single message component type
hook(int) h2;
// Hook with two message component types
hook(int *, bool) h3;
```

Beyond suspending computations on it, a hook <tt><em>h</em></tt> has two
associated operations:

* <pre><em>h</em>.send(<em>msg1</em>, ... <em>msgN</em>)</pre>
  Sends a message through the hook, with message components
  <tt><em>msg1</em></tt> through <tt><em>msgN</em></tt>. The number of message
  components must match the number of message component types of the hook,
  and each message component must be compatible with the corresponding message
  component type of the hook.

  `send` is *asynchronous*: the message will only be sent &mdash; and suspended
  computations executed &mdash; once all current device entries on the call
  stack have been completed. It is exactly equivalent to
  <tt>after: <em>h</em>.send_now(<em>msg1</em>, ... <em>msgN</em>)</tt>, except
  it's not possible to prevent the message from being sent via `cancel_after()`.
  For more information, see [Immediate After
  Statements](#immediate-after-statements).

  Like immediate after statements, pointers to stack-allocated data **must not**
  be passed as message components to a `send`. If you must use pointers to
  stack-allocated data, then `send_now` should be used instead of `send`. If you
  want the message to be delayed to avoid ordering bugs, create a method which
  wraps the `send_now` call together with the declarations of the local
  variable(s) which are pointed to, and then use an immediate after statement
  (`after: m(...)`) to delay the call to that method.

* <pre><em>h</em>.send_now(<em>msg1</em>, ... <em>msgN</em>)</pre>
  Sends a message through the hook, with message components
  <tt><em>msg1</em></tt> through <tt><em>msgN</em></tt>. The number of message
  components must match the number of message component types of the hook,
  and each message component must be compatible with the corresponding message
  component type of the hook.

  `send_now` is *synchronous*: every computation suspended on the hook will
  execute before `send_now` completes.

  `send_now` returns the number of suspended computations that were successfully
  resumed from the message being sent. Currently, every suspended computation is
  guaranteed to successfully be resumed unless cancelled by a preceding
  computation resumed by the `send_now`. This will not remain true in the
  future: coroutines are planned to be able to reject a message and reattach
  themselves to the hook.

* <pre><em>h</em>.suspended</pre>
  Evaluates to the number of computations currently suspended on the hook.

References to hooks are valid run-time values: a reference to a hook with
message component types <tt><em>msgtype1</em></tt> through
<tt><em>msgtypeN</em></tt> will have the hook reference type
<tt>hook(<em>msgtype1</em>, ... <em>msgtypeN</em>)</tt>. This means hook
references can be stored in variables, and can be passed around as method
arguments or return values. In fact, hook references are even
[serializable](#serializable-types).

Two hook references of the same hook reference type can be compared for
equality, and are considered equal when they both reference the same hook.

> [!NOTE]
> Hooks have a notable shortcoming in their lack of configurability;
> for example, there is no way to configure a hook to log an error when a message
> is sent through the hook and there is no computation suspended on the hook to
> act upon the message. Proper hook configurability is planned to be added by the
> time or together with coroutines being introduced to DML. Until then, the
> suggested approach is to create wrappers around usages of <tt>send_now()</tt>.
> Hook reference types can be leveraged to cut down on the needed number of such
> wrappers, for example:
> <pre>
> method send_now_checked_no_data(hook() h) {
>     local uint64 resumed = h.send_now();
>     if (resumed == 0) {
>         log error: "Unhandled message to hook";
>     }
> }
>
> method send_now_checked_int(hook(int) h, int x) {
>     local uint64 resumed = h.send_now(x);
>     if (resumed == 0) {
>         log error: "Unhandled message to hook";
>     }
> }
> </pre>

## Object Declarations

The general form of an object declaration is "<code><em>type</em>
<em>name</em> <em>extras</em> is (<em>template</em>, ...) <em>desc</em> {
... }</code>" or "<code><em>type</em> <em>name</em> <em>extras</em> is
(<em>template</em>, ...) <em>desc</em>;</code>", where *`type`*
is an object type such as `bank`, *`name`* is an
identifier naming the object, and *`extras`* is optional
special notation which depends on the object type. The <code>is
(<em>template</em>, ...)</code> part is optional and will make the object
inherit the named templates. The surrounding parenthesis can be omitted if
only one template is inherited. The *`desc`* is an optional
string constant giving a very short summary of the object. It can consist
of several string literals concatenated by the '+' operator. Ending the
declaration with a semicolon is equivalent to ending with an empty
pair of braces. The *body* (the section within the braces) may
contain *parameter declarations*, *methods*, *session
variable declarations*, *saved variable declarations*,
*in each declarations* and
*object declarations*.

For example, a `register` object may be declared as

```
register r0 @ 0x0100 "general-purpose register 0";
```

where the "<code>@ <em>offset</em></code>" notation is particular for the
`register` object type; see below for details.

Using <code>is (<em>template1</em>, <em>template2</em>)</code> is equivalent to
using `is` statements in the body, so the following two
declarations are equivalent:

```
register r0 @ 0x0100 is (read_only,autoreg);

register r0 @ 0x0100 {
    is read_only;
    is autoreg;
}
```

An object declaration with a *`desc`* section, on the form

<pre>
<em>type</em> <em>name</em> ... <em>desc</em> { ... }
</pre>

is equivalent to defining the parameter `desc`, as in

<pre>
<em>type</em> <em>name</em> ... {
    param desc = <em>desc</em>;
    ...
}
</pre>

In the following sections, we will leave out *`desc`* from
the object declarations, since it is always optional. Another parameter,
`documentation` (for which there is no short-hand), may also be
defined for each object, and for some object types it is used to give a
more detailed description.
See Section [Universal Templates](dml-builtins.html#universal-templates)
for details.)

If two object declarations with the same name occur within the same
containing object, and they specify the same object type, then the
declarations are concatenated; e.g.,

<pre>
bank b {
    register r size 4 { ...<em>body1</em>... }
    ...
    register r @ 0x0100 { ...<em>body2</em>... }
    ...
}
</pre>

is equivalent to

<pre>
bank b {
    register r size 4 @ 0x0100  {
        ...<em>body1</em>...
        ...<em>body2</em>...
    }
    ...
}
</pre>

However, it is an error if the object types should differ.

Most object types (`bank`, `register`,
`field`,
`group`, `attribute`, `connect`,
`event`, and `port`) may be used
in *arrays*. The general form of an object array declaration is

<pre>
<em>type</em> <em>name</em>[<em>var</em> &lt; <em>size</em>]... <em>extras</em> { ... }
</pre>

Here each <code>[<em>var</em> &lt; <em>size</em>]</code> declaration defines
a dimension of resulting array. *var* defines the name of the
index in that dimension, and *size* defines the size of the dimension.
Each *variable* defines a parameter in the object scope, and thus must
be unique.
The size must be a compile time constant. For instance,

```
register regs[i < 16] size 2 {
    param offset = 0x0100 + 2 * i;
    ...
}
```

or written more compactly

```
register regs[i < 16] size 2 @ 0x0100 + 2 * i;
```

defines an array named `regs` of 16 registers (numbered from 0 to
15) of 2 bytes each, whose offsets start at 0x0100.
See Section [Universal Templates](dml-builtins.html#universal-templates)
for details about arrays and index parameters.

The size specification of an array dimension may be replaced with `...` if the
size has already been defined by a different declaration of the same object
array. For example, the following is valid:

```
register regs[i < 16][j < ...] size 2 @ 0x0100 + 16 * i + 2 * j;
register regs[i < ...][j < 8] is (read_only);
```

The following sections give further details on declarations for object
types that have special conventions.

### Register Declarations

The general form of a `register` declaration is

<pre>
register <em>name</em> size <em>n</em> @ <em>d</em> is (<em>templates</em>) { ... }
</pre>

Each of the "<code>size <em>n</em></code>", "<code>@ <em>d</em></code>", and "<code>is
(<em>templates</em>)</code>" sections is optional, but if present, they must
be specified in the above order.

* A declaration

  <pre>
  register <em>name</em> size <em>n</em> ... { ... }
  </pre>

  is equivalent to

  <pre>
  register <em>name</em> ... { param size = <em>n</em>; ... }
  </pre>

* A declaration

  <pre>
  register <em>name</em> ... @ <em>d</em> ... { ... }
  </pre>

  is equivalent to

  <pre>
  register <em>name</em>  ... { param offset = <em>d</em>; ... }
  </pre>

### Field Declarations

The general form of a [`field` object](dml-builtins.html#field-objects)
declaration is

<pre>
field <em>name</em> @ [<em>highbit</em>:<em>lowbit</em>] is (<em>templates</em>) { ... }
</pre>

or simply

<pre>
field <em>name</em> @ [<em>bit</em>] ... { ... }
</pre>

specifying a range of bits of the containing register, where the syntax
<code>[<em>bit</em>]</code> is short for <code>[<em>bit</em>:<em>bit</em>]</code>.
Both the "`@ [...]`" and the <code>is (<em>templates</em>)</code>
sections are optional; in fact, the "`[...]`" syntax is merely a
much more convenient way of defining the (required) field parameters
`lsb` and `msb`.

For a range of two or more bits, the first (leftmost) number always
indicates the *most significant bit*, regardless of the bit
numbering scheme used in the file. This corresponds to how bit fields
are usually visualized, with the most significant bit to the left.

The bits of a register are always numbered from zero to *n* - 1,
where *n* is the width of the register. If the default
little-endian bit numbering is used, the least significant bit has index
zero, and the most significant bit has index *n* - 1. In this case,
a 32-bit register with two fields corresponding to the high and low
half-words may be declared as

```
register HL size 4 ... {
    field H @ [31:16];
    field L @ [15:0];
}
```

If instead big-endian bit numbering is selected in the file, the most
significant bit has index zero, and the least significant bit has the
highest index. In that case, the register above may be written as

```
register HL size 4 ... {
    field H @ [0:15];
    field L @ [16:31];
}
```

This is useful when modeling a system where the documentation uses
big-endian bit numbering, so it can be compared directly to the model.

## Conditional Objects

It is also possible to conditionally include or exclude one or more
object declarations, depending on the value of a boolean
expression. This is especially useful when reusing source files
between several similar models that differ in some of the details.

The syntax is very similar to the [`#if` statements](#if-else-statements)
used in methods.

```
#if (enable_target) {
    connect target (
        interface signal;
    }
}
```

One difference is that the braces are required.  It is also possible
to add else branches, or else-if branches.

```
#if (modeltype == "Mark I") {
    ...
} #else #if (modeltype == "Mark II" {
    ...
} #else {
    ...
}
```

The general syntax is

<pre>
#if ( <em>conditional</em> ) { <em>object declarations</em> ... }
#else #if ( <em>conditional</em> ) { <em>object declarations</em> ... }
...
#else { <em>object declarations</em> ... }
</pre>

The *conditional* is an expression with a constant boolean value.  It
may reference parameters declared at the same level in the object
hierarchy, or in parent levels.

The *object declarations* are any number of declarations of objects, session
variables, saved variables, methods, or other `#if` statements, but not
parameters, `is` statements, or `in each` statements . When the conditional is
`true` (or if it's the else branch of a false conditional), the object
declarations are treated as if they had appeared without any surrounding *#if*.
So the two following declarations are equivalent:

```
#if (true) {
    register R size 4;
} #else {
    register R size 2;
}
```

is equivalent to

```
register R size 4;
```

## In Each Declarations

In Each declarations are a convenient mechanism to apply a
pattern to a group of objects. The syntax is:

`in each` (`template-name`, ...) `{`
`body` `}`

where `template-name` is the name of a template
and `body` is a list of object statements, much like the body
of a template. The statements in `body` are expanded in any
subobjects that instantiate the template `template-name`,
either directly or indirectly. If more than
one `template-name` is given, then the body will be expanded
only in objects that instantiate *all* the listed templates.

The `in each` construct can be used as a convenient way to
express when many objects share a common property. For
example, a bank can contain the following to conveniently set the size
of all its registers:
```
in each register { param size = 2; }
```

Declarations in an `in each` block will override any
declarations in the extended template. Furthermore, declarations in
the scope that contains an `in each` statement, will override
declarations from that `in each` statement. This can be used
to define exceptions for the `in each` rule:
```
bank regs {
    in each register { param size default 2; }
    register r1 @ 0;
    register r2 @ 2;
    register r3 @ 4 { param size = 1; }
    register r4 @ 5 { param size = 1; }
}
```

An `in each` block is only expanded in subobjects; the
object where the `in each` statement is present is
unaffected even if it instantiates the extended template.

An `in each` statement with multiple template names can be used
to cause a template to act differently depending on context:
```
template greeting { is read; }
template field_greeting is write {
    method write(uint64 val) {
        log info: "hello";
    }
}
in each (greeting, field) { is field_greeting; }
template register_greeting is write {
    method write(uint64 val) {
        log info: "world";
    }
}
in each (greeting, register) { is register_greeting; }

bank regs {
    register r0 @ 0 {
        // logs "hello" on write
        field f @ [0] is (greeting);
    }
    // logs "world" on write
    register r1 @ 4 is (greeting);
}
```

## Global Declarations

The following sections describe the global declarations in DML. These
can only occur on the top level of a DML model, i.e., not within an
object or method. Unless otherwise noted, their scope is the entire
model.

### Import Declarations

<pre>
import <em>filename</em>;
</pre>

Imports the contents of the named file. *filename* must be a string
literal, such as `"utility.dml"`. The `-I` option to the
`dmlc` compiler can be used to specify directories to be searched
for import files.

If *filename* starts with `./` or `../`, the
compiler disregards the `-I` flag, and the path is instead
interpreted relative to the directory of the importing file.

Note that imported files are parsed as separate units, and use their
own language version and bit order declarations. A DML 1.4 file is not
allowed to import a DML 1.2 file, but a DML 1.2 file may import a DML
1.4 file.

### Template Declarations

[Templates](#templates) may only be declared on the top level, and the syntax
and semantics for such declarations have been described previously.

Templates share the same namespace as types, as each template declaration
defines a corresponding template type of the same name. It is illegal to define
a template whose name conflicts with that of another type.

### Bitorder Declarations

<pre>
bitorder <em>order</em>;
</pre>

Selects the default bit numbering scheme to be used for interpreting
bit-slicing expressions and bit field declarations in the file. The
*`order`* is one of the identifiers `le` or
`be`, implying little-endian or big-endian, respectively.  The
little-endian numbering scheme means that bit zero is the least
significant bit in a word, while in the big-endian scheme, bit zero is
the most significant bit.

A `bitorder` declaration should be placed before any other
global declaration in each DML-file, but must follow immediately after
the `device` declaration if such one is present.
The scope of the declaration is the whole of the file it
occurs in. If no `bitorder` declaration is present in a file, the
default bit order is `le` (little-endian). The bitorder does not
extend to imported files; for example, if a file containing a
declaration "`bitorder be;`" imports a file with no bit order
declaration, the latter file will still use the default `le`
order.

### Constant Declarations

<pre>
constant <em>name</em> = <em>expr</em>;
</pre>

Defines a named constant.
*`expr`* must be a constant-valued expression.

Parameters have a similar behaviour as constants but are more
powerful, so constants are rarely useful. The only advantage of
constants over parameters is that they can be used in `typedef`
declarations.

### Loggroup Declarations

<pre>
loggroup <em>name</em>;
</pre>

Defines a log group, for use in [`log` statements](#log-statements).
More generally,
the identifier *`name`* is bound to an unsigned integer
value that is a power of 2, and can be used anywhere in C context; this
is similar to a `constant` declaration, but the value is
allocated automatically so that all log groups are represented by
distinct powers of 2 and can be combined with bitwise *or*.

A maximum of 63 log groups may be declared per device (61 excluding the built-in
`Register_Read` and `Register_Write` log groups.)

### Typedef Declarations

<pre>
typedef <em>declaration</em>;
extern typedef <em>declaration</em>;
</pre>

Defines a name for a [data type](#data-types).

When the `extern` form is used, the type is assumed to exist in
the C environment. No definition of the type is added to the
generated C code, and the generated C code blindly assume that the
type exists and has the given definition.

An `extern typedef` declaration may not contain a `layout` or
`endian int` type.

If a `struct` type appears within an `extern typedef`
declaration, then DMLC will assume that there is a corresponding C
type, which has members of given types that can be accessed with
the `.` operator. No assumptions are made on completeness or
size; so the C struct may have additional fields, or it might be
a `union` type. An empty member list is even allowed; this can
make sense for opaque structs. DML variables of `extern` struct type are
initialized such that any members of the C struct which are unknown to DML are
initialized to 0.

Nested struct definitions are permitted in an `extern typedef`
declaration, but an inner struct type only supports member access; it
cannot be used as a standalone type. For instance, if you have:
```
extern typedef struct {
    struct { int x; } inner;
} outer_t;
```

then you can declare `local outer_t var;` and access the member
`var.inner.x`, but the inner type is unknown to DML so you cannot
declare a variable `local typeof var.inner *inner_p;`.

### Extern Declarations

<pre>
extern <em>declaration</em>;
</pre>

Declares an external identifier, similar to a C `extern`
declaration; for example,

```
extern char *motd;
extern double table[16];
extern conf_object_t *obj;
extern int foo(int x);
extern int printf(const char *format, ...);
```

Multiple `extern` declarations for the same identifier are permitted as long as
they all declare the same type for the identifier.

### Header Declarations

```
header %{
...
%}
```

Specifies a section of C code which will be included verbatim in the
generated C header file for the device. There must be no whitespace
between the `%` and the corresponding brace in the `%{`
and `%}` markers. The contents of the header section are not
examined in any way by the `dmlc` compiler; declarations made
in C code must also be specified separately in the DML code proper.

This feature should only be used to solve problems that cannot easily be
handled directly in DML. It is most often used to make the generated
code include particular C header files, as in:

```
header %{
#include "extra_defs.h"
%}
```

The expanded header block will appear in the generated C file, which
usually is in a different directory than the source DML
file. Therefore, when including a file with a relative path, the C
compiler will not automatically look for the `.h` file in
the directory of the `.dml` file, unless a corresponding
`-I` flag is passed. To avoid this problem, DMLC inserts a C
macro definition to permit including a *companion header
file*. For instance, if the
file `/path/to/hello-world.dml` includes a header block,
then the macro `DMLDIR_HELLO_WORLD_H` is defined as the
string `"/path/to/hello-world.h"` within this header
block. This allows the header block to contain `#include
DMLDIR_HELLO_WORLD_H`, as a way to include `hello-world.h`
without passing `-I/path/to` to the C compiler.

DMLC only defines one such macro in each header block, by taking the
DML file name and substituting the `.dml` suffix
for `.h`. Furthermore, the macro is undefined after the
header. Hence, the macro can only be used to access one specific
companion header file; if other header files are desired, then
`#include` directives can be added to the companion header
file, where relative paths are expanded as expected.

See also `footer` declarations, below.

### Footer Declarations

```
footer %{
...
%}
```

Specifies a piece of C code which will be included verbatim at the end
of the generated code for the device. There must be no whitespace
between the `%` and the corresponding brace in the `%{`
and `%}` markers. The contents of the footer section are not
examined in any way by the `dmlc` compiler.

This feature should only be used to solve problems that cannot easily be
handled directly in DML. See also `header` declarations, above.

### Export Declarations

<pre>
export <em>method</em> as <em>name</em>;
</pre>

Exposes a method specified by *`method`* to other C modules within the
same Simics module under the name *`name`* with external linkage. Note
that inline methods, shared methods, methods that throw, methods with
more than one return argument, and methods declared inside object
arrays cannot be exported. It is sometimes possible to write wrapper
methods that call into non-exportable methods to handle such cases,
and export the wrapper instead.

Exported methods are rarely used; it is better to use Simics
interfaces for communication between devices. However, exported
methods can sometimes be practical in tight cross-language
integrations, when the implementation of one device is split between
one DML part and one C/C++ part.

Example: the following code in DML:

```
method my_method(int x) { ... }
export my_method as "my_c_function";
```

will export `my_method` as a C function with external linkage,
using the following signature:

```
void my_c_function(conf_object_t *obj, int x);
```

The `conf_object_t *obj` parameter corresponds to the device instance, and is
omitted when the referenced method is [independent](#independent-methods).

## Resolution of overrides

This section describes in detail the rules for how DML handles when there are
multiple definitions of the same parameter or method. A less technical but
incomplete description can be found in the [section on templates](#templates).

* Each declaration in every DML file is assigned a *rank*. The set of ranks
  form a partial order, and are defined as follows:
  * The top level of each file has a rank.
  * Each template definition has a rank.
  * The block in an `in each` declaration has a rank.
  * If one object declaration has rank *R*, then any subobject
    declaration inside it, also those inside an `#if` block, has rank *R*.
  * `param` and `method` declarations has the rank of the object they
    are declared within. This includes shared methods.
  * If an object declaration contains <code>is <em>T</em></code>, then
    that object declaration has higher rank than the body of the
    template *`T`*.
  * If one file *F<sub>1</sub>* imports another file *F<sub>2</sub>*,
    then the top level of *F<sub>1</sub>* has higher rank than the top
    level of *F<sub>2</sub>*.
  * A declaration has higher rank than the block of any `in each`
    declaration it contains.
  * An `in each` block has higher rank than the templates it applies to
  * If there are three declarations *D<sub>1</sub>*, *D<sub>2</sub>*
    and *D<sub>3</sub>*, where *D<sub>1</sub>* has higher rank than
    *D<sub>2</sub>* and *D<sub>2</sub>* has higher rank than
    *D<sub>3</sub>*, then *D<sub>1</sub>* has higher rank than
    *D<sub>3</sub>*.
  * A declaration may not have higher rank than itself.
* In a set of `method` or `param` declarations that declare the same
  object in the hierarchy, then we say that one declaration
  *dominates* the set if it has higher rank than all other
  declarations in the set.  Abstract `param` declarations (<code>param
  <em>name</em>;</code> or <code>param <em>name</em> :
  <em>type</em>;</code>) and abstract method definitions (<code>method
  <em>name</em>(<em>args...</em>);</code>) are excluded here; they
  cannot dominate a set, and a dominating declaration in a set does
  not need to have higher declaration than any abstract `param` or
  `method` declaration in the set.
* There may be any number of *untyped* abstract definitions of a
  parameter (<code>param <em>name</em>;</code>).
* There may be at most one *typed* abstract definition of a parameter
  (<code>param <em>name</em> : <em>type</em>;</code>)
* There may be at most one abstract shared definition of a method. Any
  other *shared* definition of this method must have higher rank than
  the abstract definition, but any rank is permitted for non-shared
  definitions. For instance:

  ```
  template a {
      method m() default {}
  }
  template b {
      shared method m() default {}
  }
  template aa is a {
      // OK: overrides non-shared method
      shared method m();
  }
  template bb is b {
      // Error: abstract shared definition overrides non-abstract
      shared method m();
  }
  ```
* When there is a set of declarations of the same a `method` or
  `param` object in the hierarchy, then there must be (exactly) one of
  these declarations that dominates the set; it is an error if there
  is not.
* If there is a `method` or `param` that is *not* declared `default`,
  then it must dominate the set of declarations of that method or
  parameter; it is an error if it does not.
* In the above two rules, "the set of declarations" of an object does
  not include declarations that are disabled through an `#if`
  statement, or definitions that appear in a template that never is
  instantiated in an object. However, the rules *do* also apply to
  *shared* method declarations in templates, regardless whether the
  templates are used. For instance:
  ```
  template t1 {
      method a() {}
      shared method b() {}
  }
  template t2 is t1 {
      // OK, as long as t2 never is instantiated
      method a default {}
      // Error, even if t2 is unused
      shared method b() default {}
  }
  ```

* If the set of declarations *D<sub>1</sub>*, *D<sub>2</sub>*, ...,
  *D<sub>n</sub>* of a method *M* is dominated by the declaration
  *D<sub>n</sub>*, then:
  * If there is a *k*, 1 ≤ *k* ≤ n-1, such that *D<sub>k</sub>* dominates
    the set *D<sub>1</sub>*, ..., *D<sub>n-1</sub>*, then the symbol
    `default` refers to the method implementation of *D<sub>k</sub>*
    within the scope of the method implementation of *D<sub>n</sub>*.
  * If not, then `default` is an illegal value within the method
    implementation of *D<sub>n</sub>*.

It follows that:
* The following code is illegal, because it would otherwise give T a higher
  rank than itself:

  ```
  template T {
      #if (p) {
          group g is T {
              param p = false;
          }
      }
  }
  ```

* Cyclic imports are not permitted, for the same reason.
* If an object is declared twice on the top level in the same file,
  then both declarations have the same rank. Thus, the following
  declarations of the parameter `p` count as conflicting, because
  neither has a rank that dominates the other:

  ```
  bank b {
      register r {
          param p default 3;
      }
  }
  bank b {
      register r {
          param p = 4;
      }
  }
  ```

## Comparison to C/C++
<a id="comparison-to-c"/>

The algorithmic language used to express method bodies in DML is an extended
subset of ISO C, with some C++ extensions such as `new` and `delete`. The
DML-specific statements and expressions are described in Sections
[Method Statements](#method-statements) and [Expressions](#expressions).

DML defines the following additional built-in data types:

<dl><dt>

`int1`, ..., `int64`, `uint1`, ...,
`uint64`
</dt><dd>

Signed and unsigned specific-width integer types. Widths from 1 to
64 are allowed.
</dd><dt>

`bool`
</dt><dd>

The generic boolean datatype, consisting of the values `true`
and `false`. It is not an integer type, and the only implicit
conversion is to `uint1`
</dd></dl>

DML also supports the non-standard C extension
<code>typeof(<em>expr</em>)</code> operator, as provided by some modern C
compilers such as GCC.

DML deviates from the C language in a number of ways:

* All integer arithmetic is performed on 64-bit numbers in DML,
  and truncated to target types on assignment. This is similar to how
  arithmetic would work in C on a platform where the `int` type
  is 64 bits wide (though in DML, `int` is an alias
  of `int32`). Similarly, all floating-point arithmetic is
  performed on the `double` type.

  For instance, consider the following:

  ```
  local int24 x = -3;
  local uint32 y = 2;
  local uint64 sum = x + y;
  ```

  In C, the expression `x + y` would cast both operands up to unsigned
  32-bit integers before performing a 32-bit addition; overflow gives
  the result is 2<sup>32</sup> - 1, which is promoted without sign
  extension into a 64-bit integer before stored in the `sum`
  variable. In DML, both operands are instead promoted to 64-bit
  signed integers, so the addition evaluates to -1, which is stored as
  2<sup>64</sup> - 1 in the `sum` variable.

  Formally, if any of the two operands of an arithmetic binary
  operator (including bitwise operators) has the type `uint64`, then
  both operands are promoted into `uint64` before the operation;
  otherwise, both operands are promoted into `int64` before the
  operation. If any operand has floating-point type, then both
  operands are promoted into the `double` type.

* Comparison operators
  (`==`, `!=`, `<`, `<=`, `>`
  and `>=`) do *not* promote signed integers to
  unsigned before comparison. Thus, unlike in C, the following
  comparison yields `true`:

  ```
  int32 x = -1;
  uint64 val = 0;
  if (val > x) { ... }
  ```

* The shift operators (`<<` and `>>`) have well-defined semantics when
  the right operand is large: Shifting by more than 63 bits gives zero
  (-1 if the left operand is negative). Shifting a negative number of
  bits is an error.

* Division by zero is an error.

* Signed overflow in arithmetic operations (`+`, `-`, `*`, `/`, `<<`)
  is well-defined. The overflow value is calculated assuming two's
  complement representation; i.e., the result is the unique value *v*
  such that *v* ≡ *r* (mod 2<sup>64</sup>), where *r* is the result of
  operation using arbitrary precision arithmetic.

* Local variable declarations must use the keyword [local](#local-statements),
[session](#session-statements), or [saved](#saved-statements); as in

  ```
  method m() {
      session int call_count = 0;
      saved bool called = false;
      local int n = 0;
      local float f;
      ...
  }
  ```

  Session and saved variables have a similar meaning to static variables as in
  C: they retain value over function calls.
  However, such variables in DML are allocated per device object, and are not
  globally shared between device instances.

  Unlike C, multiple simultaneous variable declaration and
  initialization is done through tuple syntax:
  <pre>
  method m() {
      local (int n, bool b) = (0, true);
      local (float f, void *p);
      ...
  }
  </pre>
* Plain C functions (i.e., not DML methods) can be called using normal
  function call syntax, as in `f(x)`.

  In order to call a C function from DML, three steps are needed:

  * In order for DML to recognize an identifier as a C function, it
    must be declared in DML, using an [`extern`
    declaration](#extern-declarations).

  * In order for the C *compiler* to recognize the identifier when
    compiling generated C code, a function declaration must also be
    declared in a [`header`](#header-declarations) section, or in a
    header file included from this section.

  * In order for the C *linker* to resolve the symbol, a function
    definition must be present, either in a separate C file or in a header or
    [`footer`](#footer-declarations) section.


  **foo.c**

  ```
  int foo(int i)
  {
      return ~i + 1;
  }
  ```

  **foo.h**

  ```
  int foo(int i);
  ```

  **bar.dml**

  ```
  // tell DML that these functions are available
  extern int foo(int);
  extern int bar(int);

  header %{
      // tell generated C that these functions are available
      #include "foo.h"
      int bar(int);  // defined in the DML footer section
  %}

  footer %{
      int bar(int i)
      {
          return -i;
      }
  %}
  ```

  **Makefile**

  ```
  SRC_FILES=foo.c bar.dml
  ```

* Assignments (`=`) are required to be separate statements.
  You are still allowed to assign multiple variables in one statement, as in:
  ```
  i = j = 0;
  ```

* Multiple simultaneous assignment can be performed in one statement
  through tuple syntax, allowing e.g. the following:
  ```
  (i, j) = (j, i);
  ```
  However, such assignments are not allowed to be chained.

* If a method can throw exceptions, or if it has more than one return argument, then the call must be a separate statement. If it has one or more return values, these must be assigned. If a method has multiple return arguments, these are enclosed in a parenthesis, as in:
  ```
  method divmod(int x, int y) -> (int, int) {
      return (x / y, x % y);
  }
  ...
  (quotient, remainder) = divmod(17, 5);
  ```

* Type casts must be written as <code>cast(<em>expr</em>,
  <em>type</em>)</code>.

* Comparison operators and logical operators produce results of type
  `bool`, not integers.

* Conditions in `if`, `for`, `while`, etc. must
  be proper booleans; e.g., `if (i == 0)` is allowed, and `if
  (b)` is allowed if `b` is a boolean variable, but `if
  (i)` is not, if `i` is an integer.

* The `sizeof` operator can only be used on lvalue expressions. To
  take the size of a datatype, the `sizeoftype` operator must be
  used.

* Comma-expressions are only allowed in the head of
  `for`-statements, as in

  ```
  for (i = 10, k = 0; i > 0; --i, ++k) ...
  ```

* `delete` and `throw` can only be used as statements
  in DML, not as expressions.

* `throw` does not take any argument, and `catch` cannot
  switch on the type or value of an exception.

* Type declarations do not allow the use of `union`.
  However, the `extern typedef` construct can be used to achieve
  the same result.  For example, consider the union data type declared in C
  as:

  ```
  typedef union { int i; bool b; } u_t;
  ```

  The data type can be exposed in DML as follows:

  ```
  header %{
      typedef union { int i; bool b; } u_t;
  %}
  extern typedef struct { int i; bool b; } u_t;
  ```

  This will make `u_t` look like a struct to DML, but since union
  and struct syntax is identical in C, the C code generated from uses
  of `u_t` will work correctly together with the definition from
  the `header` declaration.

## Method Statements

All ISO C statements are available in DML, and have the same semantics
as in C. Like ordinary C expressions, all DML expressions can also be
used in expression-statements.

DML adds the following statements:

### Assignment Statements
<pre>
<em>target1</em> [= <em>target2</em> = <em>...</em>] = <em>initializer</em>;
(<em>target1</em>, <em>target2</em>, ...) = <em>initializer</em>;
</pre>

Assign values to targets according to an initializer. Unlike C, assignments are
not expressions, and the right-hand side can be any initializer &mdash; such as
compound initializers (<tt>{<em>...</em>}</tt>) for struct-like types.

The first form is chaining assignments. The initializer is executed once and
the value it evaluates to is assigned to each target.

The second form is multiple simultaneous assignment. The initializer describes
multiple values &mdash; one for each target. This can be done either through:
* Providing an initializer for each target through tuple syntax, e.g.:
```
(a, i) = (false, 4);
```
* Performing a method call where each target is a return value recipient, e.g.:
```
method m() -> (bool, int) {
    ...
}
```
```
(a, i) = m();
```

Targets are updated simultaneously, meaning it's possible to e.g. swap the
contents of variables through the following:
```
(a, b) = (b, a)
```

### Local Statements
<pre>
local <em>type</em> <em>identifier</em> [= <em>initializer</em>];
local (<em>type1</em> <em>identifier1</em>, <em>type2</em> <em>identifier2</em>, <em>...</em>) [= <em>initializer</em>];
</pre>

Declares one or multiple local variables in the current scope. The right-hand
side is an initializer, meaning, for example, that compound initializers
(<tt>{<em>...</em>}</tt>) can be used.

The initializer must provide the exact number of values needed to initialize
the variables, and they must be of compatible type. Multiple values can be
provided either through:
* Providing an initializer for each variable through tuple syntax, e.g.:
```
local (bool a, int i) = (false, 4);
```
* Performing a method call where each return value initializes a variable, e.g.:
```
method m() -> (bool, int) {
    ...
}
```
```
local (bool a, int i) = m();
```

In the absence of explicit initializer expressions, a default
"all zero" initializer will be applied to each declared object.

The *discard identifier* `_` may be used as an identifier for local variables,
as well as other method-local bindings such as the method parameters, the bound
identifier in `foreach`/`#foreach`/`#select` statements, and message component
parameters of [hook-bound after statements](#hook-bound-after-statements).
Any method-local binding named "`_`" *will not be added to scope*. This is
useful for when a method parameter is unused, or if you perform a method call
where only a subset of returned values are of interest:
```
local (bool a, int _) = m();
// No conflicts since "_" is not added to scope
local (bool a, int _, float _) = returns_three_vals();
```

An alternative to this pattern is to leverage the [discard
reference](#discard-reference):
```
local bool a;
(a, _, _) = returns_three_vals();
```
... which does not require you to specify the types of the discarded values,
but may result in more lines of code.

### Session Statements
<pre>
session <em>type</em> <em>identifier</em> [= <em>initializer</em>];
session (<em>type1</em> <em>identifier1</em>, <em>type2</em> <em>identifier2</em>, <em>...</em>) [= (<em>initializer1</em>, </em>initializer2</em>, <em>...</em>)];
</pre>

Declares one or multiple [session variables](#session-variables) in the current
scope.
Note that initializers of such variables are evaluated *once* when
initializing the device, and thus must be a compile-time constant.

### Saved Statements
<pre>
saved <em>type</em> <em>identifier</em> [= <em>initializer</em>];
sabed (<em>type1</em> <em>identifier1</em>, <em>type2</em> <em>identifier2</em>, <em>...</em>) [= (<em>initializer1</em>, </em>initializer2</em>, <em>...</em>)];
</pre>

Declares one or multiple [saved variables](#saved-variables) in the current
scope.
Note that initializers of such variables are evaluated *once* when
initializing the device, and thus must be a compile-time constant.


### Return Statements
<pre>
return [<em>initializer</em>];
</pre>

Returns from method with the value(s) specified by the argument.
Unlike C, the argument is an *initializer*, meaning, for example, return
values of struct-like type can be constructed using <tt>{<em>...</em>}</tt>.

The initializer must provide the exact number of values corresponding as the
return values of the method, and they must be of compatible type. Multiple
values can be provided either through:
* Providing an initializer for each return value through tuple syntax, e.g.:
```
method m() -> (bool, int) {
    return (false, 4);
}
```
* Performing a method call and propagating the return values:
```
method n() -> (bool, int) {
    return m();
}
```

### Delete Statements

<pre>
delete <em>expr</em>;
</pre>

Deallocates the memory pointed to by the result of evaluating
*`expr`*. The memory must have been allocated with the
`new` operator, and must not have been deallocated previously.
Equivalent to `delete` in C++; however, in DML, `delete`
can only be used as a statement, not as an expression.

### Try Statements

<pre>
try <em>protected-stmt</em> catch <em>handle-stmt</em>
</pre>

Executes *`protected-stmt`*; if that completes normally,
the whole `try`-statement completes normally. Otherwise,
*`handle-stmt`* is executed. This is similar to exception
handling in C++, but in DML there is only one kind of exception. Note
that Simics C-exceptions are not handled. See also `throw`.

### Throw Statements

```
throw;
```

Throws (raises) an exception, which may be caught by a
`try`-statement. This is
similar to `throw` in C++, but in DML it is not possible to
specify a value to be thrown. Furthermore, in DML,
`throw` is a statement, not an expression.

If an exception is not caught inside a method body, then the method
must be declared as `throws`, and the exception is propagated
over the method call boundary.

### Method Calls

<pre>
(<em>d1</em>, ... <em>dM</em>) = <em>method</em>(<em>e1</em>, ... <em>eN</em>);
</pre>

A DML method is called similarly as a C function, with the exception that you
must have assignment destinations according to the number of return values of
the method. Here a DML method is called with input arguments *`e1`*, ... *`eN`*,
assigning return values to destinations *`d1`*, ... *`dM`*. The destinations are
usually variables, but they can be arbitrary L-values (even bit slices) as long
as their types match the method signature.

If the method has no return value, the call is simply expressed as:

```
p(...);
```

A method with exactly one return value can also be called in any
expression, unless it is an inline method, or a method that can throw
exceptions. For example:

```
method m() -> (int) { ... }
...
if (m() + i == 13) { ... }
```

A method call (even if it is throwing or has multiple return values) can be used
as an initializer in any context that accepts non-constant initializers; i.e.,
in [assignment statements](#assignment-statements) (as shown above), [local
variable declarations](#local-statements), and [return
statements](#return-statements). For example:
```
// declare multiple variables, and initialize them from one method call
local (int i, uint8 j) = m(e1);

// Propagate all return values from a method call as the return values of the
// caller.
return m(e1)
```

### Template-Qualified Method Implementation Calls
Every object, as well as [every template type](#templates-as-types), has a
`templates` member to allow for calling _particular_ implementations of that
object's methods, as opposed to only the final overriding implementations
that are reachable directly. Specifically, `templates` allows for invoking any
particular implementation as provided by a specified template instantiated by
the object. Such invocations are called _template-qualified method
implementation calls_, and are made as follows:
```
template t {
    method m() default {
        log info: "implementation from 't'"
    }
}

template u is t {
    method m() default {
        log info: "implementation from 'u'"
    }
}

group g is u {
    method m() {
        log info: "final implementation"
    }
}

method call_ms() {
    // Logs "final implementation"
    g.m();
    // Logs "implementation from 'u'"
    g.templates.u.m();
    // Logs "implementation from 't'"
    g.templates.t.m();
}
```

Template-qualified method implementation calls are primarily meant as a way
for an overriding method to reference overridden implementations, *even when*
the implementations are provided by hierarchically unrelated templates such that
`default` can't be used (see [Resolution of
overrides](#resolution-of-overrides).) In particular, this typically allows for
ergonomically resolving conflicts introduced when multiple orthogonal templates
are instantiated, as long as all conflicting implementations are overridable.

The following example demonstrates the most common kind of conflict that
hierarchically unrelated templates may introduce, and how template-qualified
method implementation calls may be leveraged to resolve it. Consider the
following templates:
```
template gated_write is write {
    method write_allowed(uint64 val) -> (bool) default {
        return true;
    }

    method write(uint64 val) default {
        if (write_allowed(val)) {
            default(val);
        }
    }
}

template write_1_clears is write {
    method write(uint64 val) default {
        default(get() & ~val);
    }
}
```

If one would like to instantiate both templates for a particular `field`,
attempting to do so would cause DMLC to reject the model, as the choice of
`write` implementation then becomes ambiguous.
The typical solution to implementation conflicts between templates &mdash;
making one template inherit from the other &mdash; is not appropriate in this
situation, as the operation of each template is orthogonal to the other,
and they may be used individually in other contexts.

Instead, what one may do is to modify one or both templates to offer an
overridable "base" method that is called instead of `default` within the
template's implementation of `write`. This additional flexibility enables a way
to situationally resolve the conflict: if both templates are in play, override
the base method of one template to call the `write` implementation of the
other. This effectively defines the chain in which the conflicting
implementations are to be called from one another, combining their behaviour.

The default implementations of the base methods can be to invoke the `write`
implementation of their parent template, which makes calling them the same as
calling `default`. This captures the regular case where no conflicting templates
are in play.

The below shows this approach being applied to the example above, modifying
`gated_write` to offer a base method, and leveraging a new template and an
`in each` declaration to automatically resolve the conflict wherever it would
occur in the model.
```
template gated_write is write {
    method write_allowed(uint64 val) -> (bool) default {
        return true;
    }

    method write(uint64 val) default {
        if (write_allowed(val)) {
            base_write_of_gated_write(val); // instead of default()
        }
    }

    method base_write_of_gated_write(uint64 val) default {
        // This is the implementation that default() would have resolved to
        this.templates.write.write(val);
    }
}

template write_1_clears is (write, get) { // unchanged
    ...
}

template gated_write_1_clears is (gated_write, write_1_clears) {
    // Resolve the conflict by providing an unambiguously most specific
    // write implementation
    method write(uint64 val) default {
        // This makes gated_write the first link in the call chain...
        this.templates.gated_write.write(val);
    }

    // ... and by overriding gated_write's base method, we make write_1_clears
    // the second (and final) link in the chain
    method base_write_of_gated_write(uint64 val) default {
        this.templates.write_1_clears.write(val);
    }
}

in each (gated_write, write_1_clears) { is gated_write_1_clears; }
```
> [!NOTE]
> The example given above applies regardless of whether the method
> implementations in the original templates are `shared` or not. However, the
> implementations in the template defined to resolve the conflicts might need to
> be non-`shared` if some of the implementations involved are not `shared`;
> see the final paragraph of this subsection.

This approach can be applied to resolve conflicts across more than two
conflicting templates; however, this scales poorly if used to support arbitrary
combinations of many conflicting templates. It should instead be applied only
for the specific conflicts that become observed and can't easily be resolved
through other means.

Note that the order of implementations in the call chain might matter. If, in
the example above, `write_1_clears` were instead modified to offer a base method
and been made the first link in the chain, then what it would have passed down
as the written value to `gated_write` would not have been the original value
&mdash; but rather, the register's current value with bits cleared, which might
violate the expectations of `gated_write`'s implementation.

The approach given above is not the only way in which template-qualified method
implementation calls may be utilized to resolve conflicts, only the most
commonly applicable one. In fact, the most simple approach, if viable, is to
have the implementation introduced to resolve the conflict simply call each
conflicting implementation in turn, if that doesn't cause side-effects to be
duplicated in an undesirable way. It may even be viable to call only one
particular implementation, if it makes sense to prefer it ahead of every other.

A template-qualified method implementation call is resolved by using
the method implementation provided to the object by the named template.
If no such implementation exists (whether it be because the template does not
specify one, or specifies one which is not provided to the object due to its
definition being eliminated by an [`#if`](#conditional-objects)), then the
ancestor templates of the named template are recursively searched for the
highest-ranking (most specific) implementation provided by them. If the ancestor
templates provide multiple hierarchically unrelated implementations, then the
choice is ambiguous and the call will be rejected by the compiler. In this case,
the modeller must refine the template-qualified method implementation call to
name the ancestor template whose implementation they would like to use.

A template-qualified method implementation call done via [a value of template
type](#templates-as-types) functions differently compared to compile-time
object references. In particular, `this.templates` within the bodies of `shared`
methods functions differently. The specified template must either be an ancestor
template of the value's template type, the <tt>object</tt> template, or the
template type itself; furthermore, the specified template **must provide or
inherit a `shared` implementation of the named method**. It is not sufficient
that the method is simply _declared_ `shared` such that it is part of the
template type: the implementation itself must also be `shared`. For more
information, see the documentation of the [`ENSHAREDTQMIC` error
message.](messages.html#ENSHAREDTQMIC)

### After Statements

<pre>
after ...: <em>method</em>(<em>e1</em>, ... <em>eN</em>);
</pre>

The `after` statement sets up the given method call (the _callback_) such that
it will be performed with the provided arguments at a specified point in the
future. There are three different forms of the `after` statement, syntactically
determined through what appears before the `:` &mdash; each form corresponds
to different specifications of at what future point the method should be called.

A method call suspended using an `after` statement will be performed at most
once per execution of the `after` statement; it will not recur. If it's
desirable to have a suspended method call recur, then the called method must
itself make use of `after` to set up a method call to itself.

The referenced method must be a regular or [independent](#independent-methods)
method with no return values. It may not be a C function, or a [`shared`
method](#shared-methods). The only exception to this is that the [`send_now`
operation of hooks](#hook-declarations) is also supported for use as a callback.

All method calls suspended via an `after` statement are *associated* with the
object that contains the method containing the statement. It is possible to
cancel all suspended method calls associated with an object through that
object's `cancel_after()` method, as provided by the [`object`
template](dml-builtins.html#object).

> [!NOTE]
> We plan to extend the `after` statement to allow for users to
> explicitly state what objects the suspended method call is to be associated
> with.

#### After Delay Statements
<pre>
after <em>scalar</em> <em>unit</em>: <em>method</em>(<em>e1</em>, ... <em>eN</em>);
</pre>

In this form, the specified point in the future is given through a time delay
(in simulated time, measured in the specified time unit) relative to the time
when the after delay statement is executed. The currently supported time units
are `s` for seconds (with type `double`), `ps` for picoseconds
(with type `uint64`), and `cycles` for cycles (with type `uint64`).

Every argument to the called method is evaluated at the time the `after`
statement is executed, and stored so that they may be used when the method call
is to be performed. In order to allow the suspended method call to be
represented in checkpoints, every input parameter of the method must be of
[*serializable type*](#serializable-types). This means that after delay
statements cannot be used with methods that e.g. have pointer input parameters.
unless the arguments for those input parameters are message component parameters
of the `after`.

Example:

```
after 0.1 s: my_callback(1, false);
```

The after delay statement is equivalent to creating a named [`event`](#events)
object with an event-method that performs the specified call, and posting
that event at the given time, with associated data corresponding to the
provided arguments.

#### Hook-Bound After Statements
<pre>
after <em>hookref</em>[-> (<em>msg1</em>, ... <em>msgN</em>)]: <em>method</em>(<em>e1</em>, ... <em>eM</em>);
</pre>

In this form, the suspended method call is bound to the
[hook](#hook-declarations) specified by <tt><em>hookref</em></tt>. The point in
the future when the method call is executed is thus the next time a message is
sent through the specified hook.

The *binding syntax* <tt>-> (<em>msg1</em>, ... <em>msgN</em>)</tt> is used to
bind each component of the message received to a corresponding identifier,
called a *message component parameter*. These message component parameters can
be used as arguments of the called method, thus propagating the contents of the
message to the method call.

Every argument to the called method which isn't a message component parameter is
evaluated at the time the `after` statement is executed, and stored so that they
may be used when the method call is to be performed. In order to allow the
suspended method call to be represented in checkpoints, every input parameter of
the method must be of [*serializable type*](#serializable-types), unless that
input parameter receives a message component. This means that hook-bound after
statements cannot be used with methods that e.g. have pointer input parameters,
unless the arguments for those input parameters are message component parameters
of the `after`.

Example use:
```
hook(int, float) h;

method my_callback(int i, float f, bool b) {
    ...
}

method m() {
    after h -> (x, y): my_callback(x, y, false);
}

method send_message() {
    // Assuming m() has been called once before, this 'send_now' will result in
    // `my_callback(1, 3.7, false)` being called.
    h.send_now(1, 3.7);
}
```

If the hook has only one message component, the syntax <tt>-> <em>msg</em></tt>
can be used instead, and if the hook has no message components, then the binding
syntax can be entirely omitted. Any message component parameter can be used for
any number of arguments, but cannot be used as anything *but* a direct argument.
For example, using the definitions of `h` and `my_callback` as above, the
following use of `after` is valid:
```
after h -> (x, y): my_callback(x, x, false)
```
Note that the first message component is used multiple times, and the second
is not used at all.

In contrast, the following use of `after` is invalid:
```
after h -> (x, y): my_callback(i, y + 1.5, false)
```
as the message component parameter `y` is used, but not as a direct argument.

#### Immediate After Statements
<pre>
after: <em>method</em>(<em>e1</em>, ... <em>eN</em>);
</pre>

In this form, the specified point in the future is when control is given back to
the simulation engine such that the ongoing simulation of the current processor
may progress, and would otherwise be ready to move onto the next cycle.
This happens after all entries to devices on the call stack have been completed.

Immediate after statements are most useful to avoid ordering bugs. It can be
used to delay a method call until all current lines of execution into the device
have been completed, and the device is guaranteed to be in a consistent state
where it is ready to handle the method call.

Semantically, the immediate after statement is very close to
`after 0 cycles: ...`, but has a number of advantages. In general, the immediate
after statement is designed to execute the callback as promptly as possible
while satisfying the semantics stated above, while `after 0 cycles: ...` is not.
In particular, in Simics, callbacks delayed via `after 0 cycles` are always
bound to the clock associated with the device instance, which is not always
that of the processor currently under simulation &mdash; in such cases the
simulated processor may progress indefinitely without the posted callback being
executed. The immediate after statement does not have this issue.
In addition, if an immediate after statement is executed while the
simulation is stopped (due to a device entry such as an attribute get/set
performed from a script/CLI) then the callback is registered as *work*,
thus guaranteeing that it is called before the simulation starts again.

Within a particular device instance, method calls suspended by immediate
after statements are executed in order of least recently suspended; in other
words, FIFO semantics. The order in which method calls suspended by immediate
after statements are executed across multiple device instances is not defined.

Within an immediate after statement, every argument provided to the called
method is evaluated at the time the `after` statement is executed, and stored so
that they may be used when the method call is to be performed. Unlike the other
forms of `after` statements, the input parameters of the method are never
required to be of serializable type, meaning pointers can be passed as arguments
to the callback. But **beware**: pointers to stack-allocated data (pointers to
or within `local` variables) must **never** be passed as arguments. The
stack with which the `after` statement is executed is *not* preserved, so any
pointers to stack-allocated data will point to invalid data by the time the
callback is called. The DML compiler has some checks in place to warn about the
most obvious cases where pointers to stack-allocated data are provided as
arguments, but it is unable to detect all cases. It is ultimately the modeller's
responsibility to ensure it doesn't happen.

To detail a scenario exemplifying the kind of issues that immediate after may
be leveraged to solve, consider the following device, which needs to communicate
with a *manager* device and receive permission in order to perform a particular
action. Its function is simple: once prompted, the device will raise a signal to
the manager in order to request permission, and waits for it to respond with an
acknowledgement. Once received, the device lowers the signal to the manager and
performs the action it just received permission for.
In order to implement the asynchronous logic needed for this, a simple FSM is
used.
```
param STATE_IDLE = 0;
param STATE_EXPECTING_ACK = 1;
saved int curr_state = STATE_IDLE;

port manager_link {
    connect manager {
        interface signal;
    }

    implement signal {
        method signal_raise() {
            on_acknowledgement();
        }
    }
}

method request_permission_for_action() {
    if (curr_state != STATE_IDLE) {
        log error: "Request already in progress";
        return;
    }
    manager_link.manager.signal.signal_raise();
    curr_state = STATE_EXPECTING_ACK;
}

method on_acknowledgement() {
    if (curr_state != STATE_EXPECTING_ACK) {
        log spec_viol: "Received ack when not expecting it";
        return;
    }
    manager_link.manager.signal.signal_lower();
    perform_permission_gated_action();
    curr_state = STATE_IDLE;
}
```
This device has a subtle bug: it can't handle if the manager responds to the
`signal_raise()` call synchronously.
The FSM transitions to the state capable of handling the acknowledgement
only after the `signal_raise()` call returns, so if the manager responds
synchronously &mdash; as part of the `signal_raise()` call &mdash; then
`on_acknowledgement` will be called while the device still considers itself to
be in its idle state.

This bug can be solved in numerous ways &mdash; the most obvious is to
transition the state before making the `signal_raise()` call &mdash; but
immediate after provides a solution which doesn't require carefully managing
the FSM's logic, by delaying the call to `on_acknowledgement` until the device
is done with all other logic.
```
implement signal {
    method signal_raise() {
        after: on_acknowledgement();
    }
}
```
This guarantees that the FSM is able to finish its current line of execution and
properly transition itself to its new state before it's asked to manage any
response of manager, even if the manager responds synchronously.


### Log Statements

<pre>
log <em>log-type</em>[, <em>level</em> [ then <em>subsequent-level</em> ] [, <em>groups</em>] ]: <em>format-string</em>, <em>e1</em>, ..., <em>eN</em>;
</pre>

Outputs a formatted string to the Simics logging facility. The string
following the colon is a normal C `printf` format string,
optionally followed by one or more arguments separated by commas. (The
format string should not contain the name of the device, or the type of
the message, e.g., "error:..."; these things are automatically prefixed.)
Either both of *`level`* and *`groups`* may be
omitted, or only the latter; i.e., if *`groups`* is
specified, then *`level`* must also be given explicitly.

A Simics user can configure the logging facility to show only specific
messages, by matching on the three main properties of each message:

* The *`log-type`* specifies the general category
  of the message. The value must be one of the identifiers
  `info`, `warning`, `error`, `critical`,
  `spec_viol`, or `unimpl`.

* The *`level`* specifies at what verbosity level the log
  messages are displayed. The value must be an integer from 1 to 4; if
  omitted, the default level is 1. The different levels have the following
  meaning:

  1. Important messages (displayed at the normal verbosity level)
  2. High level informative messages (like mode changes and important events)

  3. Medium level information (the lowest log level for SW development)
  4. Debugging level with low level model detail (Mainly used for model
     development)

  If the *`log-type`* is one of `warning`, `error` or `critical`, then *`level`*
  may only be 1.

* If *`subsequent-level`* is specified, then all logs after the first
  issued will be on the level *`subsequent-level`*. You are allowed
  to specify a *`subsequent-level`* of 5, meaning no logging after the
  initial log.

  If the *`log-type`* is one of `warning`, `error` or `critical`, then
  *`subsequent-level`* may only be either 1 or 5.

* The *`groups`* argument is an integer whose bit
  representation is used to select which log groups the message belongs
  to. If omitted, the default value is 0. The log groups are specific for
  the device, and must be declared using the `loggroup`
  device-level declaration. For example, a DML source file containing the
  declarations

  ```
  loggroup good;
  loggroup bad;
  loggroup ugly;
  ```

  could also contain a log statement such as

  ```
  log info, 2, (bad | ugly): "...";
  ```

  (note the `|` bitwise-or operator), which would be displayed if
  the user chooses to view messages from group `bad` or
  `ugly`, but not if only group `good` is shown.

  Groups allow the user to create arbitrary classifications of log
  messages, e.g., to indicate things that occur in different states, or in
  different parts of the device, etc. The two log groups
  `Register_Read` and `Register_Write` are predefined by
  DML, and are used by several of the built-in methods.

The *`format-string`* should be one or several string
literals concatenated by the '+' operator, all optionally surrounded
by round brackets.

See also *Simics Model Builder User's Guide*, section
"Logging", for further details.

### Assert Statements

<pre>
assert <em>expr</em>;
</pre>

Evaluates *`expr`*. If the result is `true`, the
statement has no effect; otherwise, a runtime-error is generated.
*`expr`* must have type `bool`.

### Error Statements

<pre>
error [<em>string</em>];
</pre>

Attempting to compile an `error` statement causes the compiler to
generate an error, using the specified string as error message. The
string may be omitted; in that case, a default error message is used.

The *`string`*, if present, should be one or several
string literals concatenated by the '+' operator, all optionally
surrounded by round brackets.

### Foreach Statements

<pre>
foreach <em>identifier</em> in (<em>expr</em>) <em>statement</em>
</pre>

The `foreach` statement repeats its body (the
*`statement`* part) once for each element given by *`expr`*.
The *`identifier`* is used to refer to the current element
within the body.

DML currently only supports `foreach` iteration on values of `sequence` types
&mdash; which are created through [Each-In expressions](#each-in-expressions).

The `continue` statement can be used within a `foreach` loop to continue to the
next element, and the `break` statement can be used to exit the loop.

<pre>
#foreach <em>identifier</em> in (<em>expr</em>) <em>statement</em>
</pre>

In this alternative form the *`expr`* is required
to be a DML compile-time constant,
and the loop is completely unrolled by the DML compiler.
This can be combined with tests on the value of
*`identifier`* within the body, which will be evaluated at
compile time.

DML currently only supports `#foreach` iteration on [compile-time list
constants](#list-expressions).

For example:

```
#foreach x in ([3,2,1]) {
    #if (x == 1) foo();
    #else #if (x == 2) bar();
    #else #if (x == 3) baz();
    #else error "out of range";
}
```

would be equivalent to

```
baz();
bar();
foo();
```

Only `#if` can be used to make such selections; `switch` or
`if` statements are *not* evaluated at compile time. (Also
note the use of `error` above to catch any compile-time
mistakes.)

The `break` statement can be used within a `#foreach` loop to exit it.

### Select Statements

<pre>
select <em>identifier</em> in (<em>expr</em>) where (<em>cond-expr</em>) <em>statement</em> else <em>default-statement</em>
</pre>

The `select` statement resembles a C `switch` statement and is very similar
to the `foreach` statement, but executes the *`statement`* exactly once for the
first matching element of those given by *`expr`*, i.e., for the first element
such that *`cond-expr`* is `true`; or if no element matches, it executes the
*`default-statement`*.

<pre>
#select <em>identifier</em> in (<em>expr</em>) where (<em>cond-expr</em>) <em>statement</em> #else <em>default-statement</em>
</pre>

In this alternative form the *`expr`* is required to be a DML
compile-time constant, and
*`cond-expr`* can only depend on compile-time constants, apart
from *`identifier`*. The selection will then be performed by
the DML compiler at compile-time, and code will only be generated for
the selected case.

DML currently only supports `#select` iteration on [compile-time list
constants](#list-expressions).

> [!NOTE]
> The `select` statement has been temporarily removed from DML 1.4 due
> to semantic issues, and only the `#select` form may currently be used.
> The `select` statement will be reintroduced in the near future.

### #if and #else Statements
<a id="if-else-statements"/>

<pre>
#if (<em>condition</em>) { <em>true_body</em> } #else { <em>false_body</em> }
</pre>

The `#if` statement resembles a C `if` statement. The difference
being that the `#if` statement must have a constant-valued
*condition* and the statement is evaluated at compile-time.
The *true\_body* of the `#if` is only processed
if the condition evaluates to `true`,
and will be dead-code eliminated otherwise.

Similarly, the `#else` statement can immediately follow the body of an
`#if` statement and the *false\_body* will only be processed
if the *condition* in the preceding `#if` evaluates to
`false`.

## Expressions

All ISO C operators are available in DML, except for certain limitations
on the comma-operator, the `sizeof` operator, and type casts; see
Section [Comparison to C/C++](#comparison-to-c). Operators have the same
precedences and semantics as in C

DML adds the following expressions:

### The Undefined Constant

```
undefined
```

The constant `undefined` is an abstract *compile-time
only* value, mostly used as a default for parameters that are
intended to optionally be overridden. The `undefined` expression may only
appear as a parameter value and as argument to
the <code>defined <em>expr</em></code> test (see below).

### References

<pre>
<em>identifier</em>
</pre>

To reference something in the DML object structure, members may be
selected using
`.` and `->` as in C. (However, most objects in the DML
object structure are proper substructures selected with the `.`
operator.) For example,

```
this.size # a parameter
dev.bank1 # a bank object
bank1.r0.hard_reset # a method
```

The DML object structure is a compile-time construction; references to
certain objects are not considered to be proper values, and result in
compile errors if they occur as standalone expressions.

Some DML objects are proper values, while others are not:

* `session`/`saved` variables are proper values

* Composite object references (to `bank`, `group`, `register`, etc.) are not
  proper values unless cast to a [template type](#templates-as-types).

* Inside an object array, the index variable (named `i` by
  default) may evaluate to an *unknown index* if accessed
  from a location where the index is not statically known. For
  instance, in `group g[i < 4] { #if (i == 0) { ... } }`,
  the `#if` statement is invoked once, statically, across all
  indices, meaning that the `i` reference is an unknown
  index, and will yield a compile error.

* A reference to a `param` member is a proper value
  only if the parameter value is a proper value: A parameter value
  can be a reference to an object, an object array, a list,
  the `undefined` expression, or a static index (discussed
  above), in which case the parameter is not allowed as a standalone
  expression.

* When the object structure contains an array of objects,
  e.g. `register r[4] { ... }`, then a reference to the array
  itself (i.e. `r` as opposed to `r[0]`), is not
  considered a proper value.

If a DML object is not a proper value, then a reference to the object
will give a compile error unless it appears in one of the following contexts:

* As the left operand of the `.` operator

* As the definition of a `param`

* As a list element in a compile-time list

* As the operand of the `defined` operator

* A `method` object may be called

* An object array may appear in an index
  expression <code><em>array</em>[<em>index</em>]</code>

* An unknown index may be used as an index to an object array; in the
  resulting object reference, the corresponding index variable of
  the object array will have an unknown value.

### Method References as Function Pointers
It is possible to retrieve a function pointer for a method by using the prefix
operator `&` with a reference to that method. The methods this is possible with
are subject to the same restrictions as with the [`export` object
statement](#export-declarations): it's not possible to retrieve a function
pointer to any inline method, shared method, method that throws, method with
more than one return argument, or method declared inside an object array.

For example, with the following method in DML:
```
method my_method(int x) { ... }
```

then the expression `&my_method` will be a function pointer of type:
```
void (*)(conf_object_t *, int);
```

The `conf_object_t *` parameter corresponds to the device instance, and is
omitted when the referenced method is [independent](#independent-methods).

Note that due to the precedence rules of `&`, if you want to immediately call a
method reference converted to a function pointer, then you will need to wrap
parentheses around the converted method reference. An example of where this may
be useful is in order to call a non-independent method from within an
independent method:
```
independent method callback(int i, void *aux) {
  local conf_object_t *obj = aux;
  (&my_method)(obj, i);
}
```

### The Discard Reference (`_`)
<a id="discard-reference"/>
```
_
```

The discard reference *`_`* is an expression without any run-time representation
that may be used as the target of an assignment in order to explicitly discard
the result of an evaluated expression or return value of a method call.

Example usage:
```
// Evaluate an expression and explicitly discard its result.
// Can be relevant to e.g. suppress Coverity's CHECKED_RETURN checker
_ = nonthrowing_single_return_method();

// Calls to methods that throw or have multiple return values require a target
// for each return value. `_` can be used to discard return values not of
// interest.
_ = throwing_method();
(_, x, _) = method_with_multiple_return_values();
```

The discard reference is related to the [discard
identifier](#discard-identifier), and have some use-cases in common.

### New Expressions

<pre>
new <em>type</em>

new <em>type</em>[<em>count</em>]
</pre>

Allocates a chunk of memory large enough for a value of the specified
type.  If the second form is used, memory for *count* values will
be allocated.  The result is a pointer to the allocated memory. (The
pointer is never null; if allocation should fail, the Simics
application will be terminated.)

When the memory is no longer needed, it should be deallocated using a
`delete` statement.

### Cast Expressions

<pre>
cast(<em>expr</em>, <em>type</em>)
</pre>

Type casts in DML must be written with the above explicit `cast`
operator, for syntactical reasons.

Semantically, <code>cast(<em>expr</em>, <em>type</em>)</code> is equivalent to
the C expression <code>(<em>type</em>) <em>expr</em></code>.

### Sizeoftype Expressions

<pre>
sizeoftype <em>type</em>
</pre>

The `sizeof` operator in DML can only be used on expressions,
not on types, for syntactical reasons. To take the size of a datatype,
the `sizeoftype` operator must be used, as in

```
int size = sizeoftype io_memory_interface_t;
```

Semantically, <code>sizeoftype <em>type</em></code> is equivalent to the C
expression <code>sizeof (<em>type</em>)</code>.

DML does not know the sizes of all types statically; DML usually regards a
`sizeoftype` expression as non-constant and delegates size calculations
to the C compiler. DML does evaluate the sizes of integer types, layout types,
and constant-sized arrays thereof, as constants.

### Defined Expressions

<pre>
defined <em>expr</em>
</pre>

This compile-time test evaluates to `false` if
*`expr`* has the value `undefined`, and to
`true` otherwise.

### Each-In Expressions

An expression `each`-`in` is available to traverse all objects
that implement a specific template. This can be used as a generic hook
mechanism for a specific template, e.g. to implement custom reset patterns.
For example, the following can be used to reset all registers in the bank
`regs`:
```
foreach obj in (each hard_reset in (regs)) {
    obj.hard_reset();
}
```

An `each`-`in` expression can currently only be used for
iteration in a `foreach` statement. The expression's type
is <code>sequence(<em>template-name</em>)</code>.

An `each`-`in` expression searches recursively in the
object hierarchy for objects implementing the template, but once it
finds such an object, it does not continue searching inside that
subobject. Recursive traversal can be achieved by letting the
template itself contain a method that descends into subobjects; the
implementation of `hard_reset` in `utility.dml`
demonstrates how this can be done.

The order in which objects are given by a specific `each`-`in` expression is not
defined, except for that it is deterministic. That is, for a particular choice
of template `X` and object `Y` in an `each X in (Y)` expression, for a
particular iteration of the device model, and for the particular DMLC build
used, the order in which objects are given by that expression is guaranteed to
be consistent.

### List Expressions

<pre>
[<em>e1</em>, ..., <em>eN</em>]
</pre>

A list is a *compile-time only* value, and is an ordered sequence
of zero or more expressions. Lists are in particular
used in combination with `foreach` and `select`
statements.

A list expression may only appear in the following contexts:

* As the list to iterate over in a `#foreach`
  or `#select` statement

* As the value in a `param` or `constant` declaration

* As a list element in another compile-time list

* In an index expression, <code><em>list</em>[<em>index</em>]</code>

* As the operand of the `defined` operator

### Length Expressions

<pre>
<em>list</em>.len

<em>sequence</em>.len

<em>object-array</em>.len

<em>value-array</em>.len
</pre>

Used to obtain the length of a *`list`*,
*`sequence`*, *`object-array`*,
or *`value-array`* expression.
This expression is constant for each form but *`sequence`*
expressions.

The *`value-array`* form can only be used with arrays of known
constant size: it can't be used with pointers, arrays of unknown size,
or variable-length arrays.

### Bit Slicing Expressions

<pre>
<em>expr</em>[<em>e1</em>:<em>e2</em>]

<em>expr</em>[<em>e1</em>:<em>e2</em>, <em>bitorder</em>]

<em>expr</em>[<em>e1</em>]

<em>expr</em>[<em>e1</em>, <em>bitorder</em>]
</pre>

If *`expr`* is of integer type, then the above
*bit-slicing* syntax can be used in DML to simplify extracting or
updating particular bit fields of the integer. Bit slice syntax can be
used both as an expression producing a value, or as the target of an
assignment (an L-value), e.g., on the left-hand side of an `=`
operator.

Both *`e1`* and *`e2`* must be integers. The
syntax <code><em>expr</em>[<em>e1</em>]</code> is a short-hand for
<code><em>expr</em>[<em>e1</em>:<em>e1</em>]</code> (but only evaluating
*`e1`* once).

The *`bitorder`* part is optional, and selects the bit
numbering scheme (the "endianness") used to interpret the values of
*`e1`* and *`e2`*. If present, it must be one
of the identifiers `be` or `le`, just as in the
`bitorder` device-level declaration.  If no
*`bitorder`* is given in the expression, the global bit
numbering (as defined by the `bitorder` declaration) is used.

The first bit index *`e1`* always indicates the *most
significant bit* of the [field](#field-declarations),
regardless of the bit numbering scheme. If the default
little-endian bit numbering is used, the least significant bit of the
integer has index zero, and the most significant bit of the integer has
index *n* - 1, where *n* is the width of the integer type.

If big-endian bit numbering is used, e.g., due to a `bitorder
be;` declaration in the file, or using a specific local bit
numbering as in <code><em>expr</em>[<em>e1</em>:<em>e2</em>, be]</code>, then
the bit corresponding to the little-endian bit number *n* - 1 has
index zero, and the least significant bit has the index *n* - 1,
where *n* is the bit width of *`expr`*.  Note that
big-endian numbering is illegal if *`expr`* isn't a simple
expression with a well-defined bit width.  This means that only local
variables, method parameters, device variables (registers, data etc),
and explicit cast expressions are allowed.  For little-endian
numbering, any expressions are allowed, since there is never any doubt
that bit 0 is the least significant bit.

If the bit-slicing expression results in a zero or negative sized
range of bits, the behavior is undefined.

### Stringify Expressions

<pre>
stringify(<em>expr</em>)
</pre>

Translates the value of *`expr`* (which must be a
compile-time constant) into a string constant. This is similar to the
use of `#` in the C preprocessor, but is performed on the level
of compile time values, not tokens. The result is often used with the
`+` string operator.

### String Concatenation Expressions

<pre>
<em>expr1</em> + <em>expr2</em>
</pre>

If both *`expr1`* and *`expr2`* are compile-time
string constants, the expression <code><em>expr1</em> + <em>expr2</em></code>
concatenates the two strings at compile time. This is often used in
combination with the `#` operator, or to break long lines for
source code formatting purposes.

### Compile-Time Conditional Expressions

<pre>
<em>condition</em> #? <em>expr1</em> #: <em>expr2</em>
</pre>

Similar to the C `conditional` expression, with the difference
that the *condition* must have a constant value and the
expression is evaluated at compile-time.
*expr1* is only processed if the
*condition* is `true` and *expr2* is only processed
if *condition* is `false`, so an expression like
`false #? 1/0 #: 0` is equivalent to `0`.
