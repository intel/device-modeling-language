<!--
  © 2021 Intel Corporation
  SPDX-License-Identifier: MPL-2.0
-->

# Libraries and Built-ins

## Standard Parameters

This section describes the built-in parameters of the different object
types, together with the expected data types of their values.

### Object Parameters

The following are the standard parameters defined in all DML objects:

<dl><dt>

this [reference]
</dt><dd>

Always refers to the current object, i.e., the nearest enclosing
object definition.
</dd><dt>

parent [reference | undefined]
</dt><dd>

Always refers to the parent (containing) object. This has the value
`undefined` for the `device` object.
</dd><dt>

desc [string | undefined]
</dt><dd>

A short summary of the object, for documentation and debugging purposes. This is
`undefined` by default, and is expected to be supplied by the programmer. It
should preferably be only a few words. `desc` may be used by all object types.
(The short-hand syntax for `desc` is described in Section
[x](language.html#object-declarations).) See also `documentation`. </dd><dt>

documentation [string | undefined]
</dt><dd>

A longer documentation string describing the object. The default
value is `undefined`.
`documentation` is intended for those object types which may have
a longer documentation.
If undefined, the `desc` parameter is used instead.
If defined, it will override the `desc` parameter for object types
such as connect, attribute, and register.

If you have the *Documentation and Packaging* package and intend to
generate Simics reference documentation for the device then the
`documentation` string must follow the Simics documentation XML format,
otherwise you will get a syntax error during the documentation build. See the
*Writing Documentation* application note.
</dd><dt>

limitations [string | undefined]
</dt><dd>

A description of the limitations in the model, or the value
`undefined`. This can be used for generating documentation of the model.

If you have the *Documentation and Packaging* package and intend to
generate Simics reference documentation for the device then the
`limitations` string must follow the Simics documentation XML format,
otherwise you will get a syntax error during the documentation build. See the
*Writing Documentation* application note.
</dd><dt>

name [string | undefined]
</dt><dd>

The name of an object is the identifier used when declaring the
object. For the `device` object it is the identifier
used in the `device` statement. In an
anonymous `bank` object it is `undefined`, and
in an implicitly created `field` object it is the name
of the parent `register` object. See
also `qname` below.
</dd><dt>

qname [string]
</dt><dd>

The fully-qualified name of the object, such as
`"bank1.r0"`. In the `device` object, and in all
objects declared on top level, this has the same value as
the `name` parameter. In an anonymous `bank`
object it is `""`, and in an implicitly
created `field` object it is the fully-qualified name
of the parent `register` object.
</dd><dt>

objtype [string]
</dt><dd>

The object type name.
</dd><dt>

dev [reference]
</dt><dd>

Normally refers to the device object, i.e., the top-level object
definition (regardless of the name used for the device); inherited from
parent object.
</dd><dt>

bank [reference | undefined]
</dt><dd>

Normally refers to the enclosing bank object; inherited from parent
object. Has the value `undefined` for objects not within a
bank.
</dd><dt>

index [integer | undefined]
</dt><dd>

For objects that are not array elements, this has the default value
`undefined`. For further details, see Array Parameters,
below.
</dd><dt>

indexvar [string | undefined]
</dt><dd>

For objects that are not array elements, this has the default value
`undefined`. For further details, see Array Parameters,
below.
</dd></dl>

### Array Parameters

All objects that can form an array have a few parameters that can be
used to index the array.  These parameters are a little special
compared to other parameter in the sense that they are
*variable*, and depends on the index argument. Consider a
register declared in this way:

```
register r[4][j in 0..3];
```

Each individual register in this register array will have different
values for the `index` parameter, so `$r[1][3].index` is
`[1, 3]`, and `$r[x][y].index` is `[x, y]`.
The declaration above also specifies that the `j`
parameter should work as a synonym for the second element
of the `index` parameter. In addition it defines (implicitly)
that the `i` parameter should work as a synonym for the first element.

This is useful when nesting arrays or using multi-dimensional arrays:

```
group g[i in 0..3] {
    register r[j in 0..7] {
        // $i is the index in g, and $j is the index in r
    }
}
register r[i in 0..3][j in 0..7] {
    // $i is the first index in r, and $j is the second index in r
}
```

These parameters may be used when assigning values to other parameter.
The most common case is when assigning the <code>offset</code>
parameter for a register, as in

```
register r[i in 0..3] @ 0x1000 + $i * 4;
```

<dl><dt>

index [integer]
</dt><dd>

This parameter specifies the indices of the object in the
array. If the object is in a multi-dimensional array, this parameter
is a list, if the object is in a single-dimensional array it is an integer,
otherwise it is the constant `undefined`.
The first element of an array always has index 0 (or [0][0]...).
</dd><dt>

*i* [integer]
</dt><dd>

Each array has an *individual index parameter*, to make it
possible to refer to both inner and outer indexes when arrays are nested
(cf. the `index` parameter, above). The parameter name can be
specified in the array declaration, as in "`register regs[j in 0..10]
{...}`". The default name is `i`, if the array is defined
without an explicit index parameter, as in "`register regs[10]
{...}`" (see also `indexvar`, below).
</dd><dt>

indexvar [string]
</dt><dd>

This parameter specifies the name of the individual index parameter
of the array. If the array is multi-dimensional, the value
of this parameter is the constant `undefined`
</dd></dl>

### Device Parameters

<dl><dt>

classname [string]
</dt><dd>

The name of the Simics configuration object class defined by the
device model. Defaults to the name of the device object.
</dd><dt>

banks [list of references]
</dt><dd>

A list of references to all the `bank` objects of the
device object.
</dd><dt>

register\_size [integer | undefined]
</dt><dd>

The default size (width) in bytes used for `register`
objects; inherited by `bank` objects. The default value is
`undefined`.
</dd><dt>

byte\_order [string]
</dt><dd>

The default byte order used when accessing registers wider than a
single byte; inherited by `bank` objects. Allowed values
are `"little-endian"` and `"big-endian"`. The default
value is `"little-endian"`.
</dd><dt>

log\_group [integer | undefined]
</dt><dd>

An additional log group used by registers when logging register
accesses.  This may be overridden in each bank and each individual
register or register group.
</dd><dt>

obj [reference]
</dt><dd>

A special built-in parameter that can be used to provide a Simics
API `conf_object_t*` reference to the device object at the C
level.
</dd><dt>

logobj [reference]
</dt><dd>

Deprecated alias of the `obj` parameter.
</dd><dt>

dml\_1\_2 [true]
</dt><dd>

This parameter is always true in DML 1.2, but will be false in
future versions of the language. The parameter can be useful to
create conditional common code when migrating a larger system to a
new language version.
</dd></dl>

### Attribute Parameters

<dl><dt>

allocate\_type [string | undefined]
</dt><dd>

Either `undefined` or the desired type name of the
attribute value as a string, for example, `"uint32"`. In the latter
case, storage will be automatically created for the attribute. For
simple types of `bool`, `int{8,16,32,64}`,
`uint{8,16,32,64}`, `double` and
`string`, the corresponding get/set methods will also
be generated automatically. The default value of this parameter
is `undefined`.
</dd><dt>

type [string | undefined]
</dt><dd>

A Simics configuration-object attribute type description string,
such as `"i"` or `"[s*]"`, specifying the type of the
attribute. (See the documentation of
`SIM_register_typed_attribute` in the *Model Builder
Reference Manual* for details.)  This is calculated
automatically from the
`allocate_type` parameter, if that is not
`undefined` and falls into one of the simple types that DML
handles automatically.
</dd><dt>

configuration [`"required"` | `"optional"`
| `"pseudo"` | `"none"`]
</dt><dd>

Specifies how Simics treats the attribute. The default value is
`"optional"`. A *required* attribute must be initialized
to a value when the object is created, while an *optional*
attribute can be left undefined. In both cases, the value is saved when
a checkpoint is created. For a *pseudo*-attribute, the
value is *not* saved when a checkpoint is created (and it is not
required to be initialized upon object creation). Setting the value to
`"none"` suppresses creation of the attribute; this can sometimes
be useful for implicit attributes that inherit the
`configuration` parameter from their parent, as e.g., in
`register` objects (cf. Section
[x](object-model.html#register-attributes)).
</dd><dt>

persistent [bool]
</dt><dd>

If this parameter is `true`, the attribute will be treated
as persistent, which means that its value will be saved
when using the `save-persistent-state` command.  The default
value is `false`.
</dd><dt>

internal [bool]
</dt><dd>

If this parameter is `true`, the attribute will be treated
as internal, meaning that it will be excluded from documentation.
The default value is `true` if the `documentation` parameter
is defined, and `false` otherwise.
</dd><dt>

attr\_type [string | undefined]
</dt><dd>

Reserved for internal use. Always has the same value as the
`type` parameter.
</dd></dl>

### Bank Parameters

<dl><dt>

mapped\_registers [list of references]
</dt><dd>

A list of references to all register objects that are mapped into
the address space of the bank, in an undefined order.
</dd><dt>

unmapped\_registers [list of references]
</dt><dd>

A list of references to all register objects that are *not*
mapped into the address space of the bank, in an undefined order.
</dd><dt>

numbered\_registers [list of references]
</dt><dd>

A list of references to all register objects that are assigned
a register number, in an undefined order.
</dd><dt>

mappable [boolean]
</dt><dd>

Controls whether a bank is visible as an interface port for the
`io_memory` interface, which makes it mappable in a memory
space.  This defaults to true, unless the bank is anonymous.
</dd><dt>

function [integer | undefined]
</dt><dd>

The *function number* of the bank, which can be
used for mapping the bank into a Simics memory space. Defaults to
`undefined`.  Instead of using this parameter, it is usually
more convenient to map the bank by name instead. Bank arrays
normally do not use this parameter and are mapped
into Simics memory spaces by names and indices.
</dd><dt>

overlapping [bool]
</dt><dd>

Specifies whether this bank allows accesses that cover more than
one register. (This translates to one or more, possibly partial,
accesses to adjacent registers.) Defaults to `false`. Setting
this to true will make the generated C code larger. This parameter
must have the same value among all elements in a bank array
object, i.e., it must not depend on the index of the bank.
</dd><dt>

partial [bool]
</dt><dd>

Specifies whether this bank allows accesses that cover only parts
of a register. A partial read will read the touched register fields
(or the whole register if there are no fields) and extract the bits
covered by the read. A partial write will call the `get`
method on the touched register fields (or the whole register when
there are no fields) and replace the written bits with the written
value and then call the `write` method on the fields (or the
register) with the merged value. Defaults to `false`. Setting
this to true will make the generated C code larger. This parameter
must have the same value among all elements in a bank array
object, i.e., it must not depend on the index of the bank.
</dd><dt>

signed [bool]
</dt><dd>

The default for whether register contents should be treated as
signed integers; inherited by `register` objects. The
default value is `false`.
</dd><dt>

allocate [bool]
</dt><dd>

The default for whether storage for register contents should be
allocated in the generated C code; inherited by `register`
objects. The default value is `true`.
</dd><dt>

register\_size [integer | undefined]
</dt><dd>

Inherited from the `device` object; provides the
default value for the `size` parameter of `register`
objects.
</dd><dt>

byte\_order [string]
</dt><dd>

Specifies the byte order used when accessing registers wider than a
single byte; inherited from `device` objects. Allowed values
are `"little-endian"` and `"big-endian"`.
This parameter must have the same value among all elements in a bank
array object, i.e., it must not depend on the index of the bank.
</dd><dt>

log\_group [integer | undefined]
</dt><dd>

An additional log group used by registers when logging register
accesses.  This may be overridden in each individual register or
register group.  The default value is inherited from the
`device` object.
</dd><dt>

miss\_bank [reference | undefined]
</dt><dd>

Can be set to refer to another bank object, to which accesses are
forwarded if they cannot be performed in the current bank. Defaults to
`undefined`.
</dd><dt>

miss\_bank\_offset [integer]
</dt><dd>

The offset to be added to the address when an access is forwarded
through the `miss_bank` parameter. The default value is 0.
</dd><dt>

miss\_pattern [integer | undefined]
</dt><dd>

If set to an integer value in the range of 0-255, any bytes in a
memory access through the `io_memory` interface that do
not map to a register will assume this value for read access; writes
to those bytes are ignored. This parameter is valid only when
the `overlapping` parameter is set to `true`. The
default value is `undefined`. This parameter must have the
same value among all elements in a bank array
object, i.e., it must not depend on the index of the bank.
</dd></dl>

### Group Parameters

Groups have no specific parameters.

### Register Parameters

<dl><dt>

size integer
</dt><dd>

The size (width) of the register, in bytes. This parameter
can also be specified using the "<code>size <em>n</em></code>" short-hand
syntax for register objects. The default value is provided by the
`register_size` parameter of the enclosing `bank`
object.
</dd><dt>

bitsize [integer]
</dt><dd>

The size (width) of the register, in bits. This is equivalent to the
value of the `size` parameter multiplied by 8, and cannot be
overridden.
</dd><dt>

offset [integer | undefined]
</dt><dd>

The address offset of the register, in bytes relative to the start
address of the bank that contains it. This parameter can also be
specified using the "<code>@ <em>n</em></code>" short-hand syntax for register
objects. There is no default value. If the offset is set to `undefined`,
the register is not mapped to an address. This parameter must have the
same value among all elements in a bank array
object, i.e., it must not depend on the index of the bank.
</dd><dt>

regnum [integer | undefined]
</dt><dd>

The *register number* of the register.  It is a
number that must be unique within the register bank, and is used by
the bank implementation of the `int_register` interface.
If the value is `undefined`, the register is not available
through the `int_register` interface. This parameter must
have the same value among all elements in a bank array
object, i.e., it must not depend on the index of the bank.
</dd><dt>

signed [bool]
</dt><dd>

Specifies whether the register contents should be treated as a
signed integer; inherited from `bank` objects.
</dd><dt>

allocate [bool]
</dt><dd>

Specifies whether storage for the register contents should be allocated in
the generated C code; inherited from `bank` objects. If set
to `false`, the configuration parameter decides whether `set`
and `get` methods must be implemented or not.
</dd><dt>

fields [list of references]
</dt><dd>

A list of references to all the `field` objects of a register
object. There is always at least one field per register.
</dd><dt>

hard\_reset\_value [integer]
</dt><dd>

The value used by the default implementation of the
`hard_reset` method to initialize the internal state of a
register upon hard reset. Defaults to 0.
</dd><dt>

soft\_reset\_value [integer]
</dt><dd>

The value used by the default implementation of the
`soft_reset` method to initialize the internal state of a
register upon soft reset. Defaults to the value of the
`hard_reset_value` parameter.
</dd><dt>

logging [bool]
</dt><dd>

Specifies whether accesses to the register should be logged. The
default is `true`. See also `read_logging` and
`write_logging`.
</dd><dt>

log\_group [integer | undefined]
</dt><dd>

An additional log group used when logging register accesses.  The
default value is inherited from the `device` object.
</dd><dt>

read\_logging [bool]
</dt><dd>

Specifies whether read accesses to the register should be logged.
Defaults to the value of the `logging` parameter.
</dd><dt>

write\_logging [bool]
</dt><dd>

Specifies whether write accesses to the register should be logged.
Defaults to the value of the `logging` parameter.
</dd><dt>

configuration [`"required"` | `"optional"`
| `"pseudo"` | `"none"`]
</dt><dd>

Specifies how Simics treats the automatically created attribute
corresponding to the register. See Section [x](#attribute-parameters)
for details.
</dd><dt>

persistent [bool]
</dt><dd>

Specifies whether the register attribute should be persistent. See
Section [x](#attribute-parameters) for details.
</dd><dt>

internal [bool]
</dt><dd>

Specifies whether the register attribute should be internal. See
Section [x](#attribute-parameters) for details.
</dd><dt>

attr\_type [string | undefined]
</dt><dd>

Reserved for internal use. Always has the value `"i"`.
</dd></dl>

### Field Parameters

<dl><dt>

reg [reference]
</dt><dd>

Always refers to the containing register object.
</dd><dt>

explicit [bool]
</dt><dd>

The value of this parameter is `true` for fields explicitly
declared in the DML source code, and is `false` for implicitly
created fields.
</dd><dt>

lsb [integer]
</dt><dd>

The bit number (*in little-endian bit order*) of the least
significant bit of the field, in the containing register; a required
parameter. *This is not affected by any `bitorder`
declaration.* The preferred way of defining this parameter is to use
the "<code>[<em>highbit</em>:<em>lowbit</em>]</code>" short-hand syntax for
field ranges, whose interpretation *is* dependent on the
`bitorder` declaration of the file. Care must be taken when
referring to this parameter in a big-endian bit numbering system - if
possible, put such code in a separate file that uses little-endian bit
order interpretation.
</dd><dt>

msb [integer]
</dt><dd>

The bit number (*in little-endian bit order*) of the most
significant bit of the field, in the containing register; a required
parameter. See `lsb` for details.
</dd><dt>

bitsize [integer]
</dt><dd>

The size (width) of the field, in bits. This is automatically set
from the `lsb` and `msb` parameters and cannot be
overridden.
</dd><dt>

signed [bool]
</dt><dd>

Specifies whether the field contents should be treated as a signed
integer; inherited from the `register` object.
</dd><dt>

allocate [bool]
</dt><dd>

Specifies whether storage for the field contents should be allocated
in the generated C code; inherited from the `register`
object.
</dd><dt>

hard\_reset\_value [integer]
</dt><dd>

The value used by the default implementation of the
`hard_reset` method to initialize the internal state of the
field upon hard reset. Defaults to `undefined`. If the value is
`undefined`, the corresponding bits of the value of the
`hard_reset_value` parameter of the containing register are
used instead.
</dd><dt>

soft\_reset\_value [integer]
</dt><dd>

The value used to initialize the internal state of the field upon
soft reset. Defaults to the value of the `hard_reset_value`
parameter. If the value is `undefined`, the corresponding bits of
the value of the `soft_reset_value` parameter of the containing
register are used instead.
</dd></dl>

### Connect Parameters

<dl><dt>

interfaces [list of references]
</dt><dd>

A list of references to all the `interface` objects of a
connect object.
</dd><dt>

configuration [`"required"` | `"optional"`
| `"pseudo"`]
</dt><dd>

Specifies how Simics treats the automatically created attribute
corresponding to the connect object. Note that for array objects,
setting this parameter to `"optional"` has the effect that each
instance of the connect can be individually set to a nil value. See
Section [x](#attribute-parameters) for details.
</dd><dt>

persistent [bool]
</dt><dd>

Specifies whether the attribute should be persistent. See
Section [x](#attribute-parameters) for details.
</dd><dt>

internal [bool]
</dt><dd>

Specifies whether the attribute should be internal. See
Section [x](#attribute-parameters) for details.
</dd><dt>

attr\_type [string | undefined]
</dt><dd>

Reserved for internal use.
</dd></dl>

### Interface Parameters

<dl><dt>

required [bool]
</dt><dd>

Specifies whether a connected object must implement the interface.
If the value is `true` and the object does not implement the
interface, a runtime error is generated. The default value is
`true`.
</dd><dt>

c\_type [string]
</dt><dd>

Specifies the C type of the interface (typically a
`typedef`:ed name for a struct containing function pointers).
Defaults to the name of the interface object with the string
`"_interface_t"` appended.
</dd></dl>

### Event Parameters

<dl><dt>

timebase [`"steps"` | `"cycles"` | `"seconds"`
| `"stacked"`]
</dt><dd>

Specifies the unit for the time value passed to the `post`
method. Defaults to `"seconds"`.

The special unit `"stacked"` is deprecated and should not be
used.  It makes sure that the event callback is always called after
the current instruction.  Posting on 0 cycles or 0.0 seconds, or
calling `SIM_run_unrestricted` instead should be done
instead.
</dd><dt>

evclass [pointer]
</dt><dd>

This is a pointer to the `event_class_t` object used in the
calls to the Simics API. Most user code doesn't need to access this,
but it can be used when calling the Simics Event API functions
directly.
</dd></dl>

### Implement Parameters

<dl><dt>

c\_type [string]
</dt><dd>

Specifies the C type of the implemented interface (typically a
`typedef`:ed name for a struct containing function pointers).
Defaults to the name of the implement object with the string
`"_interface_t"` appended.
</dd></dl>

## Standard Methods

This section describes the built-in methods of the different object
types. Most methods can be redefined by the user, unless otherwise
stated.

### Device Methods

<dl><dt>

init()
</dt><dd>

Called when the device object is loaded, but before its
configuration-object attributes have been initialized.
</dd><dt>

post\_init()
</dt><dd>

Called when the device object is loaded, *after* its
configuration-object attributes have been initialized.
</dd><dt>

destroy()
</dt><dd>

Called when the device object is being deleted.
</dd><dt>

hard\_reset()
</dt><dd>

Called by Simics when a hard reset is performed on the device.
Invokes the `hard_reset` method on each `bank` of
the device. It is also called once, after the `init`
method, when the device object is created.
</dd><dt>

soft\_reset()
</dt><dd>

Called by Simics when a soft reset is performed on the device.
Invokes the `soft_reset` method on each `bank` of
the device.
</dd></dl>

### Attribute Methods

<dl><dt>

get() -> (attr\_value\_t value)
</dt><dd>

Returns the value of the attribute.  This method should be
redefined for all attributes that do not set the
`allocate_type` parameter.
</dd><dt>

set(attr\_value\_t value)
</dt><dd>

Sets the value of the attribute.  This method should be redefined
for all attributes that do not set the `allocate_type`
parameter.  If the provided value is not allowed, use a `throw`
statement to signal the error.
</dd><dt>

before\_set()
</dt><dd>

Called by `set_attribute` just before the `set`
method is called. The default implementation does nothing.
</dd><dt>

after\_set()
</dt><dd>

Called by `set_attribute` just after the `set` method
has been called. The default implementation does nothing.
</dd><dt>

get\_attribute -> (attr\_value\_t value)
</dt><dd>

Not intended to be used directly. Called by Simics for reading the
attribute value. Calls the `get` method to read the value, and
handles any exceptions that may occur.
</dd><dt>

set\_attribute(attr\_value\_t value) -> (set\_error\_t err)
</dt><dd>

Not intended to be used directly. Called by Simics for setting the
attribute value. It first calls the `before_set` method,
followed by the `set` method to set the value, and afterwards
calls the `after_set` method if `set`
succeeded. Handles any exceptions that may occur.
</dd></dl>

### Bank Methods

<dl><dt>

access(generic\_transaction\_t \*memop, uint64 offset, uint64 size)
</dt><dd>

Called when the bank is accessed via the `io_memory`
interface. Depending on the `memop` contents, the
`read_access` or `write_access` method is called. For a
write access, the method `get_write_value` is called to extract
the value from the memop. If the access succeeded, the corresponding
`finalize_read_access` or `finalize_write_access` method is
called, updating the `memop` before the `access` method
returns; otherwise, the `miss_read_access` or
`miss_write_access` method, respectively, is called. If this also
fails to handle the access, the generic `miss_access` method is
called, and gets full responsibility for updating the `memop`.

To signal a complete failure to handle the access, the `access`
method (or any method that it calls, such as `miss_access`)
should raise an exception instead of returning. This will be caught and
handled by the `io-memory.dml` library.
</dd><dt>

read\_access(generic\_transaction\_t \*memop, uint64 offset, uint64 size)
-> (bool success, uint64 value)
</dt><dd>

Performs a read access by calling the corresponding
`read_access` method of the addressed register (or registers,
for overlapping accesses). Does not update the `memop`. If the
access is valid, the `success` output parameter is set
to `true`, otherwise to `false`. Affected by
the `miss_pattern` parameter.
</dd><dt>

read(generic\_transaction\_t \*memop, uint64 offset, uint64 size) -> (uint64 value)
</dt><dd>

Utility method; equivalent to calling `read_access`, but does
not return a success flag. E.g., can be used to read directly from a bank
when it is known that the access will always succeed.
</dd><dt>

write\_access(generic\_transaction\_t \*memop, uint64 offset, uint64 size,
uint64 value) -> (bool success)
</dt><dd>

Performs a write access by calling the corresponding
`write_access` method of the addressed register (or registers,
for overlapping accesses). Does not update the `memop`. If the
access is valid, the `success` output parameter is set to
`true`, otherwise to `false`. Affected by
the `miss_pattern` parameter.
</dd><dt>

write(generic\_transaction\_t \*memop, uint64 offset, uint64 size, uint64 value)
</dt><dd>

Utility method; equivalent to calling `write_access`, but
does not return a success flag. E.g., can be used to write directly to a
bank when it is known that the access will always succeed.
</dd><dt>

miss\_read\_access(uint64 offset, uint64 size) -> (bool success,
uint64 value)
</dt><dd>

Called from `access` upon a missed read access. By default,
this function only sets the `success` output parameter to
`false` and returns. Provides a simple hook for handling read
misses.
</dd><dt>

miss\_write\_access(uint64 offset, uint64 size, uint64 value)
-> (bool success)
</dt><dd>

Called from `access` upon a missed write access. By default,
this function only sets the `success` output parameter to
`false` and returns. Provides a simple hook for handling write
misses.
</dd><dt>

miss\_access(generic\_transaction\_t \*memop, uint64 offset, uint64 size)
</dt><dd>

Called from `access` when the access could not be handled.
This method takes over the responsibility for either updating the
`memop` and returning, or raising an exception; see
`access` for further details.

By default, an info message will be logged. If the parameter
`miss_bank` of the bank is not `undefined`, then the
access is redirected to the bank referred to by `miss_bank`,
adding the value of the parameter `miss_bank_offset` (default 0)
to the offset for the access. Otherwise, a specification violation
message is logged and an exception is raised.
</dd><dt>

finalize\_read\_access(generic\_transaction\_t \*memop, uint64 val)
</dt><dd>

Called by `access` when a read access has succeeded. By
default, this method calls `set_read_value` to update the
`memop`. This method may also be useful to call when overriding
`miss_access`.
</dd><dt>

finalize\_write\_access(generic\_transaction\_t \*memop, uint64 val)
</dt><dd>

Called by `access` when a write access has succeeded. By
default it has no effect, since the `memop` normally does not
need updating after a write; cf. `finalize_read_access`.
</dd><dt>

get\_write\_value(generic\_transaction\_t \*memop) -> (uint64 writeval)
</dt><dd>

Extracts the value to be written from the `memop`. How this
is done depends on the <code>byte_order</code> parameter.
</dd><dt>

set\_read\_value(generic\_transaction\_t \*memop, uint64 value)
</dt><dd>

Stores the read value in the `memop`. How this is done
depends on the <code>byte_order</code> parameter.
</dd><dt>

hard\_reset()
</dt><dd>

Called automatically from the `hard_reset` method of the
device. Invokes the `hard_reset` method on each
`register` of the bank, in an undefined order.
</dd><dt>

soft\_reset()
</dt><dd>

Called automatically from the `soft_reset` method of the
device. Invokes the `soft_reset` method on each
`register` of the bank, in an undefined order.
</dd></dl>

### Group Methods

Groups have no specific methods.

### Register Methods

<dl><dt>

get() -> (value)
</dt><dd>

Returns the value of the register, without any other effects. If the
register contains fields, this is done by calling the `get`
method on each `field` of the register and composing the
results into a single value.
</dd><dt>

set(value)
</dt><dd>

Sets the value of the register, without any other effects. If the
register contains fields, this is done by calling the `set`
method on each `field` of the register, for the
corresponding bits of the value.
</dd><dt>

read\_access(generic\_transaction\_t \*memop, msb1, lsb) -> (value)
</dt><dd>

Performs a read access by invoking the `read` method of
the register, or if the register contains fields, by invoking the
`read_access` method on each of its fields and composing the
results into a single value. For a partial access, only affected
fields are read. The input parameters, `msb1` and `lsb`,
specify the most significant bit and the least significant bit
(in *little-endian* bit order) of the access if partial access
is allowed (e.g., the `partial` parameter in the enclosing bank
is set to `true`) but otherwise have the special compile-time
value `undefined`. The fields are read in order, starting with
the field containing the least significant bit of the register and
ending with the field containing the most significant bit of the
register. Before any of this is done, however, the
`before_read` method is called.  And finally, the
`after_read` method of the register is called.

This method is called from the `access` method of the
bank.
</dd><dt>

write\_access(generic\_transaction\_t \*memop, msb1, lsb, value)
</dt><dd>

Performs a write access by invoking the `write` method of
the register, or if the register contains fields, by invoking the
`write_access` method on each of its fields for the
corresponding bits of the value. For a partial access, only affected
fields are written. The input parameters, `msb1`
and `lsb`, specify the most significant bit and the least
significant bit (in *little-endian* bit order) of the access if
partial access is allowed (e.g., the `partial` parameter in the
enclosing bank is set to `true`) but otherwise have the special
compile-time value `undefined`. The fields are written in
order, starting with the field containing the least significant bit of
the register and ending with the field containing the most significant
bit of the register. Before any of this is done, however, the
`before_write` method is called.  And finally, the
`after_write` method of the register is called.

This method is called from the `access` method of the
bank.
</dd><dt>

read() -> (value)
</dt><dd>

Called by `read_access` for reading the actual value of the
register. This is not used if the register contains fields.
</dd><dt>

write(value)
</dt><dd>

Called by `write_access` for performing the actual write to
the register. This is not used if the register contains fields.
</dd><dt>

after\_read(generic\_transaction\_t \*memop)
</dt><dd>

Called at the very end of the `read_access` method. The
default implementation does nothing.
</dd><dt>

before\_read(generic\_transaction\_t \*memop)
</dt><dd>

Called at the start of the `read_access` method. The
default implementation does nothing.
</dd><dt>

after\_write(generic\_transaction\_t \*memop)
</dt><dd>

Called at the very end of the `write_access` method. The
default implementation does nothing.
</dd><dt>

before\_write(generic\_transaction\_t \*memop, msb1, lsb, value)
</dt><dd>

Called at the start of the `write_access` method. The
input parameters have the same meaning as they are in
the `write_access` method. The default implementation does
nothing.
</dd><dt>

hard\_reset()
</dt><dd>

Called automatically from the `hard_reset` method of the
bank. The default implementation works differently depending on
whether the register contains field or not.  If the register contains
fields, it invokes the `hard_reset` method on each of its
fields, otherwise it sets the register to the
`hard_reset_value` parameter. Finally, the
`after_hard_reset` method of the register is called.
</dd><dt>

soft\_reset()
</dt><dd>

Called automatically from the `soft_reset` method of the
bank. The default implementation works differently depending on
whether the register contains field or not.  If the register contains
fields, it invokes the `soft_reset` method on each of its
fields, otherwise it sets the register to the
`soft_reset_value` parameter. Finally, the
`after_soft_reset` method of the register is called.
</dd><dt>

after\_hard\_reset()
</dt><dd>

Called at the very end of the `hard_reset` method. The
default implementation does nothing.
</dd><dt>

after\_soft\_reset()
</dt><dd>

Called at the very end of the `soft_reset` method. The
default implementation does nothing.
</dd><dt>

get\_attribute -> (attr\_value\_t value)
</dt><dd>

Not intended to be used directly. Called by Simics for reading the
value of the register as an attribute. Calls the `get` method to
read the value, and handles any exceptions that may occur.
</dd><dt>

set\_attribute(attr\_value\_t value) -> (set\_error\_t err)
</dt><dd>

Not intended to be used directly. Called by Simics for setting the
attribute value. It first calls the `before_set` method,
followed by the `set` method to set the value, and afterwards
calls the `after_set` method if `set`
succeeded. Handles any exceptions that may occur.
</dd><dt>

before\_set()
</dt><dd>

Called by `set_attribute` just before the `set`
method is called. The default implementation does nothing.
</dd><dt>

after\_set()
</dt><dd>

Called by `set_attribute` just after the `set` method
has been called, if it succeeded. The default implementation does
nothing.
</dd></dl>

### Field Methods

<dl><dt>

get() -> (value)
</dt><dd>

Returns the value of the field, without any other effects.
</dd><dt>

set(value)
</dt><dd>

Sets the value of the field, without any other effects.
</dd><dt>

read() -> (value)
</dt><dd>

Performs an actual read from the field. Called from the
`read_access` method.
</dd><dt>

write(value)
</dt><dd>

Performs an actual write to the field. Called from the
`write_access` method.  The `write` functions
for the fields in a register are called in a defined order.
The field containing the least significant bits of the register are
called first, and the field containing the most significant bits are
called last. See the documentation for `write_access` on a
register for more information.
</dd><dt>

read\_access() -> (value)
</dt><dd>

Usually not used directly. Performs a read access on the field by
calling the `read` method. Called from the `read_access`
method of the containing register.
</dd><dt>

write\_access(value)
</dt><dd>

Usually not used directly. Performs a write access to the field by
calling the `write` method. Called from the `write_access`
method of the containing register.
</dd><dt>

hard\_reset()
</dt><dd>

Performs a hard reset on the field. Called from the
`hard_reset` method of the containing register.
</dd><dt>

soft\_reset()
</dt><dd>

Performs a soft reset on the field. Called from the
`soft_reset` method of the containing register.
</dd></dl>

### Connect Methods

<dl><dt>

before\_set()
</dt><dd>

Called at the start of the `set_attribute` method. The
default implementation does nothing.
</dd><dt>

after\_set()
</dt><dd>

Called at the very end of the `set_attribute` method. The
default implementation does nothing.
</dd><dt>

validate\_port(conf\_object\_t \*obj, const char \*port) -> (bool valid)
</dt><dd>

Called before updating the value. If the return value is false,
the attribute assignment will fail.
</dd><dt>

validate(conf\_object\_t \*obj) -> (bool valid)
</dt><dd>

Called before updating the value. If the return value is false,
the attribute assignment will fail.

<div class="note">

**Note:** New code should
use `validate_port` instead, since `validate` does
not get a *port* argument.

</div>

</dd><dt>

get\_attribute -> (attr\_value\_t value)
</dt><dd>

Not intended to be used directly. Called by Simics for reading the
value of the connect as an attribute.
</dd><dt>

set\_attribute(attr\_value\_t value) -> (set\_error\_t err)
</dt><dd>

Not intended to be used directly. Called by Simics for setting the
attribute value. It first calls the `before_set` method,
followed by the `set` method to set the value, and afterwards
calls the `after_set` method if `set`
succeeded. Handles any exceptions that may occur.
</dd></dl>

### Interface Methods

Interfaces have no documented standard methods.

### Event Methods

<dl><dt>

event(void \*data)
</dt><dd>

The method called when the event is triggered. The default event
method logs a short info message noting that the event occurred.
</dd><dt>

get\_event\_info(void \*data) -> (attr\_value\_t info)
</dt><dd>

This method is called once for each pending event instance when
saving a checkpoint.  It should create an attribute value that can be
used to restore the event.  The `data` parameter is the
user data provided in the `post` call.  The default
implementation always returns a nil value.
</dd><dt>

set\_event\_info(attr\_value\_t info) -> (void \*data)
</dt><dd>

This method is used to restore event information when loading a
checkpoint.  It should use the attribute value to create a user data
pointer, as if it had been provided in a `post`. The default
implementation only checks that the checkpointed information is
nil.
</dd><dt>

describe\_event(void \*data) -> (char \*description)
</dt><dd>

This method is used to provide a descriptive string to be shown in
listings of the event queue. It must return an allocated string that
will be deallocated by Simics after use.
</dd><dt>

destroy(void \*data)
</dt><dd>

This method is called when an event is removed from the event
queue
</dd><dt>

post(time, void \*data)
</dt><dd>

Posts the event on the associated queue of the device, with the
specified time (relative to the current time, in the unit specified by
the `timebase` parameter), and additional data.
</dd><dt>

post\_on\_queue(conf\_object\_t \*queue, when, void \*data)
</dt><dd>

Like `post`, but posts the event on the specified queue.
This is very rarely needed, since the events should usually always be
posted to the queue that the device belongs to, as configured by the
`queue` attribute.
</dd><dt>

remove(void \*data)
</dt><dd>

Removes all events of this type with matching data pointer from
the queue.
</dd><dt>

posted(void \*data) -> (bool flag)
</dt><dd>

Returns `true` if the event is in the queue, and
`false` otherwise.
</dd><dt>

next(void \*data) -> (time)
</dt><dd>

Returns the time to the next occurrence of the event in the queue
(relative to the current time, in the unit specified by the
`timebase` parameter). If there is no such event in the queue, a
negative value is returned.
</dd></dl>

### Implement Methods

Implements have no documented standard methods.
