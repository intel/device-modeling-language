<!--
  © 2021-2022 Intel Corporation
  SPDX-License-Identifier: MPL-2.0
-->

# DML 1.2

This chapter describes the DML language, version 1.2. It will
help to have read and understood the object model in the previous
chapter before reading this chapter.

## Overview

DML is not a general-purpose programming language, but a modeling
language, targeted at writing Simics device models. The algorithmic part
of the language is an extended subset of ISO C; however, the main power
of DML is derived from its simple object-oriented constructs for
defining and accessing the static data structures that a device model
requires, and the automatic creation of bindings to Simics.

Furthermore, DML provides syntax for *bit-slicing*, which much
simplifies the manipulation of bit fields in integers; `new` and
`delete` operators for allocating and deallocating memory; a
basic `try`/`throw` mechanism for error handling; built-in
`log` and `assert` statements; and a powerful
metaprogramming facility using *templates*, method inlining, and
compile-time evaluation for shaping the generated code.

Most of the built-in Simics-specific logic is implemented directly in
DML, in standard library modules that are automatically imported; the
`dmlc` compiler itself contains as little knowledge as possible
about the specifics of Simics.

## Lexical Structure

For the most part, the lexical structure of DML resembles that of C.
However, DML distinguishes between "object context" and "C context", so
that some C keywords such as `register`, `signed` and
`unsigned`, are allowed as identifiers in object context, while
on the other hand many words that look like keywords in the object
context, such as `bank`, `event` and `data`, are in
fact allowed as identifiers in all contexts.

Another major difference from C is that names do not generally need to
be defined before their first use. This is quite useful, but might
sometimes appear confusing to C programmers.

<dl><dt>

Character encoding
</dt><dd>

DML source files are written using UTF-8 encoding.  Non-ASCII
characters are only allowed in comments and in string literals.
String values are still handled as byte arrays, which means that a
string value written with a literal of three characters may actually
create an array of more than three bytes.
</dd><dt>

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
identifiers starting with an underscore (_).

The following words are reserved specially by DML: `after`,
`assert`, `call`, `cast`, `defined`,
`error`, `foreach`, `in`, `is`,
`local`, `log`, `parameter`, `select`,
`sizeoftype`, `typeof`, `undefined`, `vect`,
and `where`.

</dd><dt>

Identifiers
</dt><dd>

Identifiers in DML are defined as in C; an identifier may begin
with a letter or underscore, followed by any number of letters,
numbers, or underscores. Identifiers that begin with an underscore (\_)
are reserved by the DML language and standard library and should not
be used. Within C context, unqualified object-context
identifiers are always prefixed by a `$` character.
</dd><dt>

Constant Literals
</dt><dd>

DML has literals for strings, characters, integers, booleans, and floating-point numbers.  The integer literals can be written in decimal (`01234`), hexadecimal (`0x12af`), or binary (`0b110110`) form.

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
comments (`/``*`...`*``/`) and comments
that continue to the end of the line (`//`...).
</dd></dl>

## Module System

DML employs a very simple module system, where a *module* is any
source file that can be imported using the `import` directive.
Such files may not contain a `device` declaration, but otherwise
look like normal DML source files. The imported modules are merged into
the main program as if all the code was contained in a single file (with
some exceptions). This is similar to C preprocessor `#include`
directives, but in DML each imported file must be possible to parse in
isolation, and may contain declarations (such as `bitorder`) that
are only effective for that file.

## Source File Structure

A DML source file describes both the structure of the modeled device and
the actions to be taken when the device is accessed.

A DML source file defining a device starts with a *language version
declaration* and a *device declaration*. After that, any
number of *parameter declarations*, *methods*, *data
fields*, *object declarations*, or *global
declarations* can be written. A DML file intended to be
*imported* (by an `import` statement in another DML file)
has the same layout except for the device declaration.

### Language Version Declaration

Every DML source file should contain a version declaration, on the form
"<code>dml <em>m</em>.<em>n</em>;</code>", where *m* and *n* are
nonnegative integers denoting the major and minor version of DML,
respectively, that the source file is written in. The version
declaration allows the `dmlc` compiler to select the proper
versions of the DML parser and standard libraries to be used for the
file. (Note that each file has its own individual language version, even
if it is imported by a file using another version.) A file should not
import a file with a higher language version than its own.

The version declaration must be the first declaration in the file,
possibly preceded by comments. For example:

```
// My Device
dml 1.2;
...
```

This document describes DML 1.2. The compiler does not compile files
written in DML 1.0.

### Device Declaration

Every DML source file that contains a device declaration is a *DML
program*, and defines a device class with the specified name. Such a
file may *import* other files, but only the initial file may
contain a device declaration.

The device declaration must be the first proper declaration in the file,
only preceded by comments and the language version declaration. For
example:

```
/*
 *  My New Device
 */
dml 1.2;
device my_device;
...
```

### Source File Example

The following is an example of a small DML program defining a very
simple device. This lacks many details that would appear in a real
device.

```
dml 1.2;

device excalibur;

connect bus {
    interface pci;
}

bank config_registers {
    register cfg1 size 4 @ 0x0000 {
        field status {
            method read { ... }
            method write { ... }
        }
        field enable {
            method read { ... }
            method write { ... }
        }
    }
}

bank databank {
    register r1 size 4 @ 0x0000 {
        field f1 {
            method read { ... }
            method write { ... }
        }
    }
    register r2 size 4 @ 0x0004 {
        field f2 {
            method read { ... }
            method write { ... }
        }
    }
}
```

## Parameter Declarations

A parameter declaration has the general form "<code>parameter <em>name</em>
<em>specification</em>;</code>", where *`specification`* is
either "<code>= <em>expr</em></code>" or "<code>default <em>expr</em></code>".
For example:

```
parameter offset = 8;
parameter byte_order default "little-endian";
```

A default value is overridden by an assignment (`=`). There can
be at most one assignment, and at most one default value, for each
parameter. Typically, a default value for a parameter is specified in a
template, and the programmer may then choose to override it where the
template is used. See Section [x](#template-declarations) for
more information about templates.

The *`specification`* part is in fact optional; if omitted,
it means that the parameter is declared to exist (and *must* be
given a value, or the program will not compile). This is sometimes
useful in templates, as in:

```
template constant {
    parameter value;
    method get -> (v) {
        v = $value;
    }
}
```

so that wherever the template `constant` is used, the programmer
is also forced to define the parameter `value`. E.g.:

```
register r0 size 2 @ 0x0000 is (constant) {
    parameter value = 0xffff;
}
```

Note that simply leaving out the parameter declaration from the template
definition can have unwanted effects if the programmer forgets to
specify its value where the template is used. At best, it will only
cause a more obscure error message, such as "unknown identifier"; at
worst, the scoping rules will select an unrelated definition of the same
parameter name.

You may see the following special form in some standard library files:

<pre>
parameter <em>name</em> auto;
</pre>

for example,

```
parameter parent auto;
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
respectively. The C keywords `short`, `signed`, `unsigned`
and `long` are reserved words in DML and not allowed in type
declarations.  The types `size_t` and `uintptr_t` are
defined as in C.
</dd><dt>

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
and little-endian integers, respectively. So for example;
`uint8_be_t`, `uint8_le_t`,
`int16_be_t`, `uint24_le_t` are all endian integer
types.
These types can be transparently
used interchangeably with regular integer types, values of one
type will be coerced to the other as needed. Note that operations
on integers will always produce regular integer types, even
if all operands are of endian integer type.
Assigning a value that would not fit into the type is undefined,
thus it is an error to assume that values will be truncated.

</dd><dt>

Floating-point numbers
</dt><dd>

There is only one floating-point type, called `double`.
Note that DMLC currently permits `float` as a type, but this
is not officially supported and we may change some details in the
semantics later. We generally recommend the use of `double`
instead.
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

The allowed types of the layout members are integers, other layout
types, bitfields (see below), and arrays of these.

The byte order declaration is mandatory, and is
either `"big-endian"` or `"little-endian"`.

Access of layout members do not always provide a value of the
type used for the member in the declaration. Bitfields and
integer members (and arrays of similar) are translated
to endian integers of similar size, with endianness matching
the layout (or arrays of such). Layout and endian integer members are
accessed normally.

Pointer arithmetic on layout members gives undefined behavior.
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

## Methods

Methods are similar to C functions, but also have an implicit
(invisible) parameter which allows them to refer to the current device
instance, i.e., the Simics configuration object representing the device.
Methods also support exception handling in DML, using `try` and
`throw`. The body of the method is a compound statement in an
extended subset of C; see also Section [x](#comparison-to-c)
It is an error to have more than one method declaration using the same
name within the same scope.

### Input and Output Parameters

In addition to the input parameters, a DML method can have any number of
output parameters, in contrast to C functions which have at most one
return value. DML methods do not use the keyword `void` - an
empty pair of parentheses always means "zero parameters", and can even
be omitted. Apart from this, the parameter declarations of a method are
ordinary C-style declarations.

For example,

```
method m1 {...}
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
method m3(int a, int b) -> (int c) {
  ...;
  c = ...;
}
```

defines a method with two input parameters and a single output
parameter. Output parameters must be explicitly assigned before the
method returns.

```
method m4() -> (int c, int d) {
  ...;
  c = ...;
  d = ...;
  return;
}
```

has no input parameters, but two output parameters. Ending the method
body with a `return` statement is optional.

If it is statically known that a method can not throw an exception,
then it may be declared as `nothrow`:

```
method m5(int x) -> (int y) nothrow {
    y = x * x;
}
```

This allows the method to be called as an expression, without a `call`
statement:

```
log info: "m5(4) is %d", $m5(4);
```

If a method is declared `nothrow`, then any throwing statement
inside the method must be enclosed in a `try` block. A `throw`
statement counts as throwing, as does a `call` or `inline`
statement, unless the method it invokes is declared `nothrow`.

<div class="note">

**Note:** In DML 1.4, `nothrow` is the default, and methods that
*can* throw an exception must be explicitly declare that with
a `throws` annotation.

</div>

### Default Methods

A method can be declared as the *default* implementation with the
`default` keyword:

```
method m() default {...}
```

A default method can be overridden *once* by defining a method
with the same name and signature in the same object but without
the `default` keyword. This is especially useful in templates
(see section [x](#template-declarations)) to provide some
default behavior while allowing the user to customize that behavior by
overriding a default method where the template is instantiated.

### Calling Methods

In DML, a method call is performed with one of the `call` and
`inline` keywords. For instance,

```
call $access(...) -> (a, b)
```

will call the method 'access' in the same object, assigning the values
of the output parameters to variables `a` and `b`. (Note
the '`$`' character which is necessary for referring to the
method.) The call might be inline expanded depending on the C compiler
used to compile the generated code, but there is no guarantee.

On the other hand,

```
inline $access(...) -> (a, b)
```

has the same semantics as the `call`, but will always inline
expand the called method.

It is possible to invoke the default method in the overriding method using
the following special method call syntax (note that there is no `$`
before `default`):

```
call default(...) -> (...);
```

, or

```
inline default(...) -> (...);
```

to expand the overridden method inline.

### Methods as Macros

Methods can also be used as macros, by omitting the types on
one or more of the input parameters. A method defined this way can only
be called through an <code>inline</code> statement; see Section
[x](#inline-statements).

For example,

```
method twice(x) -> (y) { y = x + x; }
```

could be used to double any numeric value without forcing the result to
be of any particular type, as in

```
int32_t x;
int64_t y;
inline twice(x) -> (x);
inline twice(y) -> (y);
```

This is sometimes referred to as a *polymorphic* method.

This form of macros is particularly useful when writing
*templates* (see Section [x](#template-declarations)).
Note that the name scoping rules and the semantics of calls are the same
as for normal methods; in other words, it is a form of "hygienic
macros".

### External Methods

A method can be declared *external*, which means that a C
function corresponding to the method is guaranteed to be generated.
This is done by prefixing the name of the
method with the keyword `extern`; e.g.:

```
method extern my_method(int x) { ... }
```

An external method must have a proper signature, i.e., the types of all
its input and output parameters must be specified.

External methods are rarely used, since most of the needs for making DML
methods accessible from outside the device model itself are covered by
the `implement`, `event`, and
`attribute` type objects.  Note that the generated
functions will always have static linkage.

## Data Fields

A *data* declaration creates a named storage location for an
arbitrary run-time value. The name belongs to the same namespace as
objects and methods. The general form is:

<pre>
data <em>declaration</em> = <em>initializer</em>;
</pre>

where *`= initializer`* is optional
and *`declaration`* is similar to a C variable
declaration; for example,

```
data int id = 1;
data bool active;
data double table[4] = {0.1, 0.2, 0.4, 0.8};
data conf_object_t *obj;
```

In the absence of explicit initializer expression, a default
"all zero" initializer will be applied to the declared object.

Note that the number of elements in the initializer must match with
the number of elements or fields of the type of the *data*
object. This also implies that each sub-element, if itself being a
compound data structure, must also be enclosed in braces. C99-style
designated initializers are not supported.

## Object Declarations

The general form of an object declaration is "<code><em>type</em>
<em>name</em> <em>extras</em> is (<em>template</em> ...) <em>desc</em> {
... }</code>" or "<code><em>type</em> <em>name</em> <em>extras</em> is
(<em>template</em> ...) <em>desc</em>;</code>", where *`type`*
is an object type such as `bank`, *`name`* is an
identifier naming the object, and *`extras`* is optional
special notation which depends on the object type. The <code>is
(<em>template</em> ...)</code> part is optional and will make the object
use the named templates. The *`desc`* is an optional
string constant giving a very short summary of the object. It can consist
of several string literals concatenated by the '+' operator. Ending the
declaration with a semicolon is equivalent to ending with an empty
pair of braces. The *body* (the section within the braces) may
contain *parameter declarations*, *methods*, *data
fields*, and *object declarations*.

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
    parameter desc = <em>desc</em>;
    ...
}
</pre>

In the following sections, we will leave out *`desc`* from the object
declarations, since it is always optional. Another parameter, `documentation`
(for which there is no short-hand), may also be defined for each object, and for
some object types it is used to give a more detailed description. See Section
[x](libraries.html#object-parameters) for details.)

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
`group`, `attribute`, `connect`,
`event`, and `port`) may be used
in *arrays*. The general form of an object array declaration is

<pre>
<em>type</em> <em>name</em> [<em>variable</em> in 0..<em>max</em>]... <em>extras</em> { ... }
</pre>

Here, each *variable* defines the name of one index in the array and
<code>0..<em>max</em></code> defines the size of the corresponding dimension of the
array. Each *variable* defines a parameter in the object scope, and thus must
be unique.
*max* must be a compile time constant. For instance,

```
register regs[i in 0..15] size 2 {
    parameter offset = 0x0100 + 2 * $i;
    ...
}
```

or written more compactly

```
register regs[i in 0..15] size 2 @ 0x0100 + 2 * $i;
```

defines an array named `regs` of 16 registers (with index 0 to 15) of 2 bytes
each, whose offsets start at 0x0100. There is also a special syntax
"<code><em>type</em> <em>name</em> [<em>size</em>] ...</code>", this is
equivalent to declaring "<code><em>type</em> <em>name</em> [i in
0..<em>size</em>-1]...</code>". See Section [x](libraries.html#array-parameters)
for details about arrays and index parameters.

The following sections give further details on declarations for object
types that have special conventions.

### Bank Declarations

The general form of a `bank` declaration is

<pre>
bank <em>name</em> { ... }
</pre>

where *`name`* may be omitted. The elements (e.g., registers)
of a bank that has no name belong to the namespace of the parent object,
i.e., the device. There is at most one such anonymous bank object per
device and it is not possible to define an anonymous bank array
object; multiple "`bank { ... }`" declarations are concatenated.

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
  register <em>name</em> ... { parameter size = <em>n</em>; ... }
  </pre>

* A declaration

  <pre>
  register <em>name</em> ... @ <em>d</em> ... { ... }
  </pre>

  is equivalent to

  <pre>
  register <em>name</em>  ... { parameter offset = <em>d</em>; ... }
  </pre>

* A declaration

  <pre>
  register <em>name</em> ... is (<em>t1</em>,...,<em>tN</em>) { ... }
  </pre>

  is equivalent to

  <pre>
  register <em>name</em>  ... { is <em>t1</em>; ... is <em>tN</em>; ... }
  </pre>

Templates are further described in Section [x](#template-declarations).

### Field Declarations

The general form of a `field` declaration is

<pre>
field <em>name</em> [<em>highbit</em>:<em>lowbit</em>] is (<em>templates</em>) { ... }
</pre>

or simply

<pre>
field <em>name</em> [<em>bit</em>] ... { ... }
</pre>

specifying a range of bits of the containing register, where the syntax
<code>[<em>bit</em>]</code> is short for <code>[<em>bit</em>:<em>bit</em>]</code>.
Both the "`[...]`" and the <code>is (<em>templates</em>)</code>
sections are optional; in fact, the "`[...]`" syntax is merely a
much more convenient way of defining the (required) field parameters
`lsb` and `msb` (cf. Section [x](libraries.html#field-parameters)).

* A declaration

  <pre>
  field <em>name</em> [<em>high</em>:<em>low</em>] is (<em>t1</em>,...,<em>tN</em>) { ... }
  </pre>

  is equivalent to

  <pre>
  field <em>name</em> [<em>high</em>:<em>low</em>] { is <em>t1</em>; ... is <em>tN</em>; ... }
  </pre>

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
    field H [31:16];
    field L [15:0];
}
```

If instead big-endian bit numbering is selected in the file, the most
significant bit has index zero, and the least significant bit has the
highest index. In that case, the register above may be written as

```
register HL size 4 ... {
    field H [0:15];
    field L [16:31];
}
```

This is useful when modeling a system where the documentation uses
big-endian bit numbering, so it can be compared directly to the model.

If a register contains a set of fields with similar functionality,
then these may be represented as a *field array*. In this case,
the array size must be specified using the verbose <code><em>i</em> in
0..<em>max</em></code> syntax, and the bit range specification is
preceded by the `@` character, like:

```
register R size 4 ... {
    field A[i in 0..2] @ [i*3 + 2 : i * 3];
}
```

## Conditional Objects

It is also possible to conditionally include or exclude one or more
object declarations, depending on the value of a boolean
expression. This is especially useful when reusing source files
between several similar models that differ in some of the details.

The syntax is very similar to the `if` statements used in
methods.

```
if ($enable_target) {
    connect target (
        interface signal;
    }
}
```

One difference is that the braces are required.  It is also possible
to add else branches, or else-if branches.

```
if ($modeltype == "Mark I") {
    ...
} else if ($modeltype == "Mark II" {
    ...
} else {
    ...
}
```

The general syntax is

<pre>
if ( <em>conditional</em> ) { <em>object declarations</em> ... }
else if ( <em>conditional</em> ) { <em>object declarations</em> ... }
...
else { <em>object declarations</em> ... }
</pre>

The *conditional* is an expression with a constant boolean
value.  It may use parameters declared at the same level in the object
hierarchy, or in parent levels.

The *object declarations* are any number of declarations of
objects or methods, but not parameters.  When the conditional is true
(or if it's the else branch of a false conditional), the object
declarations are treated as if they had appeared without any
surrounding *if*.  So the two following declarations are
equivalent:

```
if (true) {
    register R size 4;
} else {
    register R size 2;
}
```

is equivalent to

```
register R size 4;
```

## Global Declarations

The following sections describe the global declarations in DML. These
can only occur on the top level of a DML program, i.e., not within an
object or method. Unless otherwise noted, their scope is the entire
program.

### Import Declarations

<pre>
import <em>filename</em>;
</pre>

Imports the contents of the named file. *filename* must be a string
literal, such as `"utility.dml"`. The `-I` option to the
`dmlc` compiler can be used to specify directories to be searched
for import files.

If *filename* starts with `./`, the compiler disregards
the `-I` flag, and the path is instead interpreted relative to
the directory of the importing file.

Note that imported files are parsed as separate units, and use their own
language version and bit order declarations.

A DML 1.2 file is allowed to import a DML 1.4 file, but the exact
semantics are not well-specified: The standard libraries of DML 1.4
and DML 1.2 are quite different, and the compiler contains workarounds
to bridge some of the differences; these workarounds may evolve in
incompatible ways.

### Template Declarations

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

or

<pre>
is (<em>name1</em>, <em>name2</em>);
</pre>

It is also possible to use templates when declaring an object or a
template, as in

<pre>
field F is <em>name</em>;
</pre>

or

<pre>
template T is (<em>name1</em>,<em>name2</em>) { ... }
</pre>

These can be used in any context where an object declaration may be
written, and has the effect of expanding the body of the template at the
point of the `is`. Note that the expansion is purely textual, so
e.g., two templates which define methods with the same name cannot both
be used in the same context.

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

Defines a named constant which can be used in C context.
*`expr`* must be a constant-valued expression.

### Loggroup Declarations

<pre>
loggroup <em>name</em>;
</pre>

Defines a log group, for use in `log` statements. More generally,
the identifier *`name`* is bound to an unsigned integer
value that is a power of 2, and can be used anywhere in C context; this
is similar to a `constant` declaration, but the value is
allocated automatically so that all log groups are represented by
distinct powers of 2 and can be combined with bitwise *or* (see
Section [x](#log-statements)).

### Typedef Declarations

<pre>
typedef <em>declaration</em>;
extern typedef <em>declaration</em>;
</pre>

Defines a name for a data type.

When the `extern` form is used, the type is assumed to exist in
the C environment.  No definition of the type is added to the
generated C code, and the generated C code may assume that the data
type supports the operations of the declared type.

### Struct Declarations

<pre>
struct <em>name</em> { <em>declarations</em> }
</pre>

This is the same as writing <code>typedef struct { <em>declarations</em> }
<em>name</em>;</code>

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

## Comparison to C/C++
<a id="comparison-to-c"/>

The algorithmic language used to express method bodies in DML is an
extended subset of ISO C, with some C++ extensions such as `new`
and `delete`. The DML-specific statements and expressions are
described in Sections [x](#statements) and [x](#expressions).

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

Most forms of C statements and expressions can be used in DML, with the
exceptions listed below. Some of these limitations may be removed in a
later version of DML.

* Local variable declarations must use the keyword `local` or
  `static`, as in

  ```
  method m() {
      static int call_count = 0;
      local int n = 0;
      local float f;
      ...
  }
  ```

  (only one variable can be introduced per declaration). Static
  variables have a similar meaning as in C, they retain value over function calls.
  But note that a `static` variable in DML is per device object, it is not
  globally shared between device instances.
  For symmetry with C, the keyword
  `auto` may be used as a synonym for `local`.

* Plain C functions (i.e., not DML methods) can be called using normal
  function call syntax, as in `f(x)`.

  C functions must either be defined in the `footer` or `header`
  sections of the DML file, or in one or more C source code files that are
  compiled or linked to separately.

  See sections [x](#header-declarations) and [x](#footer-declarations) for more detail on `header` and `footer`.

  In either case, the functions must also be declared as `extern` in the
  DML source code. For example:

  **foo.c**

  ```
  int foo(int i)
  {
      return ~i + 1;
  }
  ```

  **bar.dml**

  ```
  // tell DML that these functions are available
  extern int foo(int);
  extern int bar(int);

  header %{
      // tell generated C that these functions are available
      int foo(int);  // defined in separate C file
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

* `return` statements do not take a return value as argument;
  output parameters of methods must be assigned explicitly.

* Type casts must be written as <code>cast(<em>expr</em>,
  <em>type</em>)</code>.

* Comparison operators and logical operators produce results of type
  `bool`, not integers.

* Conditions in `if`, `for`, `while`, etc. must
  be proper booleans; e.g., `if (i == 0)` is allowed, and `if
  (b)` is allowed if `b` is a boolean variable, but `if
  (i)` is not, if `i` is an integer.

* The `sizeof` operator can only be used on expressions. To
  take the size of a datatype, the `sizeoftype` operator must be
  used.

* Declarations are not allowed in the first clause of `for`-
  statements.

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

## Statements

All ISO C statements are available in DML, and have the same semantics
as in C. Like ordinary C expressions, all DML expressions can also be
used in expression-statements.

DML adds the following statements:

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
`try`-statement. Exceptions are propagated over method call
boundaries. This is similar to `throw` in C++, but in DML it is
not possible to specify a value to be thrown. Furthermore, in DML,
`throw` is a statement, not an expression.

### Call Statements

<pre>
call <em>method</em>(<em>e1</em>, ... <em>eN</em>) -&gt; (<em>d1</em>, ... <em>dM</em>);
</pre>

Calls a DML method with input arguments *`e1`*, ...
*`eN`* and output destinations *`d1`*, ...
*`dM`*. The destinations are usually variables, but they can
be arbitrary L-values (even bit slices) as long as their types match the
method signature.

If the method has no output parameters, the `-> ()` part may be
omitted, as in

```
call p(...);
```

which is equivalent to `call p(...) -> ();`.

If the method has no input parameters, the empty pair of parentheses may
also be omitted, as in

```
call q -> (...);
```

which is equivalent to `call q() -> (...);`.

A method with neither input nor output parameters may thus be called
simply as

```
call me;
```

### Inline Statements

<pre>
inline <em>method</em>(<em>e1</em>, ... <em>eN</em>) -&gt; (<em>d1</em>, ... <em>dM</em>);
</pre>

This is equivalent to <code>call <em>method</em>(<em>e1</em>, ... <em>eN</em>)
-&gt; (<em>d1</em>, ... <em>dM</em>);</code> but the code for the called method
is expanded at the place of the `inline` call, and may be partly
specialized to the values of any input arguments that are constant at
DML compile time.

Furthermore, methods that are only intended for inlining may be declared
as a form of polymorphic hygienic macros; see Section [x](#methods-as-macros).

### After Statements

<pre>
after (<em>time</em>) call <em>method</em>(<em>e1</em>, ... <em>eN</em>);
</pre>

The `after` construct sets up an asynchronous event which will
perform the specified method call with the provided arguments at the
given time into the future (in simulated time, measured in seconds)
relative to the time when the `after` statement is executed.
For example:

```
after (0.1) call $my_callback(1, false);
```

This is equivalent to creating a named `event` object with
an event-method that performs the specified `call`, and posting
that event at the given time, with associated data corresponding to the
provided arguments; see Section [x](object-model.html#events).

Each argument to the called method is evaluated at the time the
`after` statement is executed and the event is posted.

If the method has no input parameters, the empty pair of parentheses may
be omitted, as in

```
after (0.1) call $q;
```

To allow the posted event to be checkpointed, `after` statements
may only be performed with methods that have no output parameters, and
where each input parameter is of *serializable type*. Serializable
types are currently restricted to primitive data types, or structs or
arrays containing only data types that are serializable.
This means that `after` statements cannot be used with methods
that e.g. have pointer parameters.

### Log Statements

<pre>
log <em>log-type</em>, <em>level</em>, <em>groups</em>: <em>format-string</em>, <em>e1</em>, ..., <em>eN</em>;
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
  `info`, `error`, `critical`,
  `spec_viol`, or `unimpl`.
  Log types can also be written using the alternative syntaxes
  `"info"`, `"error"`, `"critical"`,
  `"spec_violation"`, and `"unimplemented"`, respectively.
  This alternative syntax is only reserved for backward compatibility
  and may be removed in future language versions.

* The *`level`* specifies at what verbosity level the log
  messages are displayed. The value must be an integer from 1 to 4; if
  omitted, the default level is 1. The different levels have the following
  meaning:

  1. Important messages (displayed at the normal verbosity level)
  2. High level informative messages (like mode changes and important events)

  3. Medium level information (the lowest log level for SW development)
  4. Debugging level with low level model detail (Mainly used for model
     development)

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
error <em>string</em>;
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
*`statement`* part) once for each element in the
*list*  given by *`expr`*.
The *`identifier`* is used to refer to the current element
within the body. It is *not* used with a `$` prefix.

If *`expr`* is a list, it is always a DML compile-time
constant, and in that case the loop is completely unrolled by the DML
compiler. This can be combined with tests on the value of
*`identifier`* within the body, which will be evaluated at
compile time.

For example:

```
foreach x in ([3,2,1]) {
    if (x == 1) foo();
    else if (x == 2) bar();
    else if (x == 3) baz();
    else error "out of range";
}
```

would be equivalent to

```
baz();
bar();
foo();
```

Only `if` can be used to make such selections; `switch`
statements are *not* evaluated at compile time. (Also
note the use of `error` above to catch any compile-time
mistakes.)

### Select Statements

<pre>
select <em>identifier</em> in (<em>expr</em>) where (<em>cond-expr</em>) <em>statement</em> else <em>default-statement</em>
</pre>

The `select` statement
resembles a C `switch` statement and it
is very similar to the `foreach`
statement, but executes the *`statement`* exactly once for
the first matching element in the *list*  given by *`expr`*, i.e., for the first element such that
*`cond-expr`* is `true`; or if no element matches, it
executes the *`default-statement`*.

If *`expr`* is a list, *and* the
*`cond-expr`* only depends on compile-time constants, apart
from *`identifier`*, then the choice will be performed by
the DML compiler, and code will only be generated for the selected case.

## Expressions

All ISO C operators are available in DML, except for certain limitations
on the comma-operator, the `sizeof` operator, and type casts; see
Section [x](#comparison-to-c). Operators have the same
precedences and semantics as in C

DML adds the following expressions:

### The Undefined Constant

```
undefined
```

The constant `undefined` is an abstract *compile-time
only* value, mostly used as a default for parameters that are
intended to optionally be overridden. The `undefined` expression may only
appear as a parameter value, as argument to
the <code>defined <em>expr</em></code> test (see below), and as the value for an
untyped parameter when invoking a method with
`inline`.

### References

<pre>
$<em>identifier</em>
</pre>

To reference something in the DML object structure, the reference (an
object-context identifier) must be prefixed by a `$` character;
see also Section [x](#lexical-structure). Following the
identifier, subobjects may be selected using
`.` and `->` as in C. (However, most objects in the DML
object structure are proper substructures selected with the `.`
operator.) For example,

```
$this.size # a parameter
$dev.bank1 # a bank object
$bank1.r0.hard_reset # a method
```

The DML object structure is a compile-time construction; references to
certain objects are not considered to be proper values, and result in
compile errors if they occur as standalone expressions.

Some DML objects are proper values, while others are not:

* `data` objects are proper values

* `register`, `field` and `attribute` objects
  are proper values when they have allocated storage.

* Objects of types `method`, `interface`, `implement`
  and `device` are technically considered run-time values, but
  the exact meaning is undefined so their use is discouraged.

* A `bank`, `group`, `connect`, `port`
  or `event` object is not a value.

* Inside an object array, the index variable (named `$i` by
  default) may evaluate to an *unknown index* if accessed
  from a location where the index is not statically known. For
  instance, in `group g[i in 0..3] { if ($i == 0) { ... } }`,
  the `if` statement is invoked once, statically, across all
  indices, meaning that the `$i` reference is an unknown
  index, and will yield a compile error.

* A reference to a `parameter` object is a proper value
  only if the parameter value is a proper value: A parameter value
  can be a reference to an object, an object array, a list,
  the `undefined` expression, or a static index (discussed
  above), in which case the parameter is not allowed as a standalone
  expression.

* When the object structure contains an array of objects,
  e.g. `register r[4] { ... }`, then a reference to the array
  itself (i.e. `$r` as opposed to `$r[0]`), is not
  considered a proper value.

If a DML object is not a proper value, then a reference to the object
will give a compile error unless it appears in one of the following contexts:

* As the left operand of the `.` operator

* As the value of a `parameter` object

* As a list element in a compile-time list

* As the operand of the `defined` operator

* A `method` object may be invoked in a `call`
  or `inline` statement

* An object array may appear in an index
  expression <code><em>array</em>[<em>index</em>]</code>

* A register or field may appear as a `typeof` operand, even if the
  object does not have allocated storage.

* An unknown index may be used as an index to an object array; in the
  resulting object reference, the corresponding index variable of
  the object array will have an unknown value.

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

### Defined Expressions

<pre>
defined <em>expr</em>
</pre>

This compile-time test evaluates to `false` if
*`expr`* has the value `undefined`, and to
`true` otherwise.

### List Expressions

<pre>
[<em>e1</em>, ..., <em>eN</em>]
</pre>

A list is a *compile-time only* value, and is an ordered sequence
of zero or more compile-time constant values. Lists are in particular
used in combination with `foreach` and `select`
statements, and are sometimes provided by built-in parameters,
such as the `fields` parameter of `register`
objects.

A list expression may only appear in the following contexts:

* As the list to iterate over in a `foreach`
  or `select` statement

* As the value in a `parameter` or `constant` declaration

* As a list element in another compile-time list

* In an index expression, <code><em>list</em>[<em>index</em>]</code>

* As the operand of the `defined` operator

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
significant bit* of the field, regardless of the bit numbering
scheme; cf. Section [x](#field-declarations). If the default
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
# <em>expr</em>
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
