<!--
  © 2021 Intel Corporation
  SPDX-License-Identifier: MPL-2.0
-->

# The Object Model

DML is structured around an *object model*, where each
DML program describes a single *device class* (being a
subclass of the built-in generic device class), which can contain a
number of *member objects*. Each member object can in
its turn have a number of members of its own, and so on in a nested
fashion.

Every object (including the device itself) can also have
*methods*, which implement the functionality of the
object, and *parameters*, which are constant-valued
members that exist only during compilation.

Each object is an instance of a *class*, often simply called the
"object type"; the built-in object types are described in Section [x](#object-types). There is no way of adding user-defined classes
in DML; however, each object is in general locally modified by
adding (or overriding) members, methods and parameters - this can be
viewed as creating a local one-shot subclass for each instance.

Many parts of the DML object model are automatically mapped onto the
Simics *configuration object* model; most
importantly, the device class itself, its attributes and interface
methods. (See *Simics Model Builder User's Guide* for details.)

## Device Structure

A device is made up of a number of member objects and methods, where any
object may contain further objects and methods of its own. Although some
object types, such as `events`, are generic and can appear
almost anywhere, many object types only make sense in a particular
context and are not allowed elsewhere:

* Objects of type <tt>attribute</tt>,
<tt>bank</tt>, <tt>implement</tt>, or
<tt>connect</tt>, may only appear as
part of a <tt>device</tt>.

* Objects of type <tt>register</tt> may only appear as
part of a <tt>bank</tt>, or indirectly as a
part of a <tt>group</tt> that is (directly or indirectly)
a part of a `bank`.

* Objects of type <tt>field</tt> may only appear
as part of a `register`.

* Objects of type <tt>interface</tt> may only appear
as part of a <tt>connect</tt>.


## Object Types

The following is an overview of the available *object
types* (the built-in classes):

<dl><dt>

device
</dt><dd>

Each DML program defines a subclass of the `device`
class, which can be instantiated as a configuration object in
Simics.
</dd><dt>

attribute
</dt><dd>

An arbitrary Simics configuration-object attribute of the
`device`. Many attributes are created automatically from
the declaration of device members such as registers and connects, but it
is possible to manually add other attributes as well. This can be used
to provide synthetic information about the device for inspection, or as
a simple programming interface. Attributes are typically saved
automatically when a Simics checkpoint is created.
</dd><dt>

bank
</dt><dd>

A register bank. Banks make sets of registers accessible by
placing them in an *address space*. Register banks
can be individually mapped into Simics memory spaces. The
`bank` class is a subclass of <tt>group</tt>.
</dd><dt>

register
</dt><dd>

A `register` holds an integer value, and is generally
used to model a hardware register. The width of a register is a nonzero
number of bytes (a register may not be wider than 8 bytes). Registers
may only occur within a `bank` (directly or indirectly).
Registers divide the address space of a bank into discrete elements with
non-overlapping addresses. Registers are used for communication via
memory-mapped I/O, and also for storing device state in natural way.
Sometimes it is useful to have registers that belong to a bank but are
not mapped into its address space.
</dd><dt>

field
</dt><dd>

A `register` can be further divided into
*fields* on the bit level. Each such field can be
accessed separately, both for reading and for writing. The fields of a
register may not overlap.
</dd><dt>

group
</dt><dd>

Groups are generic container objects, and are mainly used to define
logical groups of registers. See also `bank`.
</dd><dt>

connect
</dt><dd>

A `connect` object holds a reference to a Simics
configuration object. (Typically, the connected object is expected to
implement some particular Simics-interface.) An attribute with the same
name is added to the `device`; thus, a
`connect` is similar to a simple `attribute`
object. Usually, initialization is done from outside the device, e.g., in
a Simics configuration file.
</dd><dt>

interface
</dt><dd>

An `interface` object may only occur within a
`connect`. It is used to declare a Simics-interface assumed
to be implemented by the connected object. In many cases, the name of
the interface is sufficient, and the body of the object can be left
empty.
</dd><dt>

implement
</dt><dd>

An `implement` object is used to define a
Simics-interface that the `device` implements. The methods
defined within the `implement` must correspond to the
functions of the interface.
</dd><dt>

port
</dt><dd>

A `port` object defines a named interface port that
can contain `implement` objects. Each port defines a
separate name space for interface implementations, which means that a
device may implement the same interface several times on different
ports.

Note that `bank` objects also implicitly defines a
port.
</dd><dt>

event
</dt><dd>

An `event` object is an encapsulation of a Simics event
that can be posted on a time or step queue. Almost all objects can have
event members, except events themselves, and objects of type
`field` and `implement`.
</dd></dl>

## Parameters

Parameters are *compile-time, constant-valued* object members.
They exist only during translation from DML to C; no storage is
allocated for parameters, and they cannot be updated - only overridden.

No type declarations are necessary for parameters; all DML compile-time
computations are dynamically type-checked. The possible types of values
of a parameter are listed below.

Parameters are used to describe static properties of the objects, such
as names, sizes, and offsets. There are a number of standard parameters
that are always defined for every object, and for each object type there
is a set of additional pre-defined parameters. Furthermore, the
programmer can add any number of new parameters, or override the
pre-defined ones.

### Parameter Types

A parameter can be assigned a value of one of the following types:

<dl><dt>

integer
</dt><dd>

An arbitrarily-sized integer value.
</dd><dt>

float
</dt><dd>

A floating-point value.
</dd><dt>

string
</dt><dd>

A sequence of characters. String literals are written within double quotes.
Strings can be split over several lines using the `+` operator; see Section
[x](language.html#string-concatenation-expressions).

</dd><dt>

bool
</dt><dd>

One of the values <tt>true</tt> and
<tt>false</tt>. These can only be used in tests and boolean
expressions; they are not integer values.
</dd><dt>

list
</dt><dd>

A list of arbitrary values. Lists are written as `[x1, ...,
xN]`.
</dd><dt>

reference
</dt><dd>

A reference to a DML object or method.
</dd><dt>

undefined
</dt><dd>

The value `undefined` is a unique value which can only be
used in the compile-time test <tt>defined <i>x</i></tt>. The result of
the test is `false` if *`x`* has the value
`undefined`, and `true` otherwise. The `undefined`
value is used mainly as a default for parameters that are intended to be
overridden.
</dd></dl>

## Methods

Methods are object members providing implementation of the functionality
of the object. Although similar to C functions, DML methods can have
both a number of input parameters and zero, one or more output
parameters. DML methods also support a basic exception handling
mechanism using `throw` and `try`.

## Devices

A *device* defined by a DML program corresponds
directly to a Simics *configuration object*, i.e., something
that can be included in a Simics configuration. Typically, the device
object will make its functionality available by implementing one or more
Simics API *interfaces* (sets of methods).

A configuration object can also contain *state* (data), and can
have any number of *attributes*, which are a specialized form
of interfaces with a pair of `get`/`set` methods
used for inspecting and manipulating some part of the state. (See
*Simics Model Builder User's Guide* for further details about
configuration objects, interfaces and attributes.)

### Attributes

The Simics configuration object attributes that are automatically added
to the device object for objects such as connects and registers are
often sufficient, but is also possible to define arbitrary additional
attributes of the device, represented by `attribute`
objects.

## Register Banks

A *register bank* (or simply
*bank*) is an abstraction that is used to group
*registers* in DML, and can also be used as
an *interface port*, typically for memory mapped
communication via the *io\_memory* interface.

For a register bank to be mapped into a Simics memory space, it must
have a name, or the <tt>function</tt> parameter must be set to a nonnegative
integer that is unique within the device.

A bank normally contains a number of <tt>register</tt>
objects, but that is not a requirement. It is possible to define an
alternative implementation of the `access` method of a particular
bank, that does not depend on registers.

Banks also implement the `int_register` interface,
allowing for indexed access to registers.  See the definition of the
<tt>regnum</tt> parameter in registers.

It is possible to define bank arrays to model a row of
similar but *externally* differently mapped banks. Elements (i.e.,
*bank*s) in bank arrays are typically mapped into Simics memory spaces
by their (*port*) names and indices.

### Groups

A *group* is a part of a bank that contains a number
of registers and other groups. It is primarily used to replicate sets
of registers within one bank. If a bank has a sequence of blocks, each
containing the same registers, it can be written as a group
array. This allows for two-dimensional register layouts, as in the
following example where there are 8&#215;6 instances of register
`r3`.

```
bank regs {
    parameter register_size = 4;
    group blocks[i in 0..7] {
        register r1 @ $i * 32 + 0;
        register r2 @ $i * 32 + 4;
        register r3[j in 0..5] @ $i * 32 + 8 + $j * 4;
    }
}
```

Another typical use of `group` is in combination with a
template for the group that contains common registers and
more that are shared between several groups, as in the following
example.

```
template weird {
    parameter group_offset;
    register a size 4 @ $group_offset;
    register b size 4 @ $group_offset + 4 {
        method read -> (value) {
            // When register $b is read, return $a
            value = $a;
        }
    }
}

bank regs {
    group block_a is (weird) { parameter group_offset = 128; }
    group block_b is (weird) { parameter group_offset = 1024; }
}
```

## Registers

A *register* is an object contains an integer value.
The value can be either unsigned (the default) or signed. Normally, a
register corresponds to a segment of consecutive locations in the
address space of the bank; however, it is also possible (and often
useful) to have registers that are not mapped to any address within
the bank.  In some cases, registers are mapped to individual register
numbers rather than to address ranges.  All registers must be part of
a register bank.

Every register has a fixed *size* (or *width*), which is
an integral, nonzero number of 8-bit bytes. A single register cannot
be wider than 8 bytes. The size of the register is given by the
<tt>size</tt> parameter,
which can be specified either by a normal parameter assignment, as in

```
register r1 {
  parameter size = 4;
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
<tt>register_size</tt>
parameter of the containing register bank, if that is defined.

Unless the register defines fields (see below), the value of the
register can be accessed directly by referencing the object, as in

```
log info: "the value of register r1 is %d", $r1;
```

or

```
$r1 += 1;
```

Storage for the register value is created by default in the generated C
code. If this is not needed (e.g., if write accesses to the register are
ignored and read accesses always return a constant value such as 0), it
can be disabled in order to save memory by setting the
<tt>allocate</tt>
parameter to `false`. Non-allocated registers cannot be accessed
by value.

### Mapping Addresses To Registers

For a register to be mapped into the internal address space of the
containing bank, its starting address within the bank must be given by
setting the
<tt>offset</tt>
parameter. The address range occupied by the register is then from
`offset` to `offset` + `size` - 1. The offset
can be specified by a normal parameter assignment, as in

```
register r1 {
  parameter offset = 0x0100;
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
  parameter register_size = 4;
  register r1 @ 0x0100;
  register r2 @ 0x0104;
  ...
}
```

The translation from the bank address space to the actual value of the
register is controlled by the <tt>byte_order</tt> parameter. When it is set to
`"little-endian"` (the default), the lowest address, i.e., that
defined by `offset`, corresponds to the least significant byte in
the register, and when set to `"big-endian"`, the lowest address
corresponds to the most significant byte in the register.  The third
allowed value of this parameter is `undefined`, which ensures
that neither assumption is ever made.

### Mapping Indexes To Registers

If the <tt>regnum</tt>
register parameter is set to a non-negative integer, that register is
assigned that number as its *index number*.  Two registers in
the same bank can never have the same index number.  This number is
used by the containing register bank when implementing the
`int_register` interface that can be used by other
objects to access registers by name or by number.

Note that register accesses through the `int_register` interface
are not supposed to cause side effects. The implementation calls the
`get` method of the register for read accesses and the
`set` method for write accesses.
The `before_set` and the `after_set` methods are not
automatically called in the `int_register` interface.

### Not Mapping Addresses To Registers

An important thing to note is that registers do not have to be mapped
at all, neither at an address offset nor with an index number.  This
is often useful for internal registers that are not directly
accessible from software.  By using an unmapped register,
you can get the advantages of using register, such as automatic
checkpointing and register fields.  This internal register can then be
used from the implementations of other registers, or other parts of
the model.

To make a register unmapped, specify the offset as `undefined`.
There is a standard template called `unmapped` that does
this, so the following two registers declarations are equivalent:

```
register r @ undefined;
register r is (unmapped);
```

### Register Attributes

For every register, an attribute of integer type is automatically added
to the Simics configuration class generated from the device model. The
name of the attribute corresponds to the hierarchy in the DML model;
e.g., a register named `r1` in a bank named `bank0` will
get a corresponding attribute named `bank0_r1`.

The register value is automatically saved when Simics creates a
checkpoint, unless the
<tt>configuration</tt>
parameter indicates otherwise.

### Fields

Real hardware registers often have a number of *fields* with
separate meaning. For example, the lowest three bits of the register
could be a status code, the next six bits could be a set of flags, and
the rest of the bits could be reserved.

To make this easy to express, a `register` object can
contain a number of `field` objects. Each field is defined
to correspond to a bit range of the containing register. (Technically, a
register that does not contain any explicitly defined fields
automatically gets a single, anonymous field, which covers the whole
register.)

Only the values of the fields are stored in the generated C code; the
value of the register as a whole is composed from the field values when
needed (for example, when creating a Simics checkpoint). Storage for
individual fields can be controlled by the
<tt>allocate</tt>
parameter, which by default is inherited from the
`register` object.

For example, the register described above could be modeled as follows,
using the default little-endian bit numbering.

```
bank b2 {
  register r0 size 4 @ 0x0000 {
    field status   [2:0];
    field flags    [8:3];
    field reserved [15:9] {
      parameter allocate = false;
    } ;
  }
  ...
}
```

Note that the most significant bit number is always the first number (to
the left of the colon) in the range, regardless of whether little-endian
or big-endian bit numbering is used. (The bit numbering convention used
in a source file can be selected by a <tt>bitorder</tt>
declaration.)

The value of the field can be accessed by referencing the
object, as for a register, e.g.:

```
log info: "the value of the status field is %d", $r0.status;
```

Note however that non-allocated fields cannot be accessed this way,
since they do not have a stored value.

## Attributes

An `attribute` object in DML represents a Simics configuration object attribute
of the device. An attribute is basically a name with an associated pair of `get`
and `set` functions; see Section [x](libraries.html#attribute-methods). The type
of the value read and written through the get/set functions is controlled by the
`type` parameter; see Section [x](libraries.html#attribute-parameters). More
information about configuration object attributes can be found in *Simics Model
Builder User's Guide*.

Attributes can be used to provide synthetic information about the device
for inspection, or as a simple programming interface. By default,
attributes are saved automatically when a Simics checkpoint is created;
this is controlled by the `configuration` parameter.

Note that an attribute is not required to save the written value; this
is entirely dependent on the intended semantics of the attribute. For
example, a very simple attribute could do nothing in the `set`
function, and always return zero from the `get` function.

By defining the `allocate_type` parameter, the `get` and
`set` functions and the `type` parameter can be created
automatically, for simple attribute types that store the written value.

For attributes defined using `allocate_type`, the value of the
attribute can be accessed within DML by referencing the object directly,
e.g.:

```
log info: "the value of attribute a is %d", $dev.a;
```

## Connects

A `connect` object is a container for a reference to an
arbitrary Simics configuration object. An attribute with the same name
as the connect is added to the Simics configuration class generated
from the device model. This attribute can be assigned a value of type
"Simics object".

A `connect` declaration is very similar to a simple
`attribute` declaration, but specialized to handle
connections to other objects.

Typically, the connected object is expected to implement one or more
particular Simics interfaces, such as `simple_interrupt`
(see *Simics Model Builder User's Guide* for details). This is
described using `interface` declarations inside the
`connect`.

Initialization of the connect (i.e., setting the object reference) is done from
outside the device, usually in a Simics configuration file. Just like other
attributes, the parameter <tt>configuration</tt> controls whether the value must
be initialized when the object is created, and whether it is automatically saved
when a checkpoint is created; see Section
[x](libraries.html#connect-parameters).

The actual object pointer, which is of type
<tt>conf_object_t*</tt> is stored in a `data`
member called `obj`.  This means that to access the current
object pointer in a connect called *otherdev*, you need to
write `$otherdev.obj`.

If the `configuration` parameter is not `required`,
the object pointer may have a null value, so any code that tries to
use it must check if it is set first.

This is an example of how a connect can be declared and used:

```
connect plugin {
    parameter configuration = "optional";
}

method mymethod {
    if ($plugin.obj)
        log info: "The plugin is connected";
    else
        log info: "The plugin is not connected";
}
```

### Interfaces

In order to use the Simics interfaces that a connected object
implements, they must be declared within the `connect`.
This is done through `interface` objects. These name the
expected interfaces and may also specify additional properties.

An important property of an interface object is whether or not a connected
object is *required* to implement the interface. This can be controlled through
the interface parameter `required`, which is `true` by default; see Section
[x](libraries.html#interface-parameters). Attempting to connect an object that
does not implement the required interfaces will cause a runtime error.

By default, the C type of the Simics interface corresponding to a
particular interface object is assumed to be the name of the object
itself with the string `"_interface_t"` appended. (The C type is
typically a `typedef`:ed name for a struct containing function
pointers). This can be changed by specifying an explicit `c_type`
parameter in the interface object.

The following is an example of a connect with two interfaces, one of
which is not required:

```
connect plugin {
  interface serial_device;
  interface rs232_device { parameter required = false; }
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
    local int n = $plugin.serial_device.write(value);
    // code to check the return value omitted
}
```

## Implements

When a device needs to export Simics interfaces (such as the
`io_memory` interface defined in the standard library file
`io-memory.dml`), this is specified by an
`implement` object, containing the methods that implement
the interface. The name of the object is also used as the name of the
Simics interface registered for the generated device, and the names and
signatures of the methods must correspond to the C functions of the
Simics interface. (A device object pointer is automatically added as the
first parameter of the generated C functions.)

By default, the C type of the Simics interface is assumed to be the name
of the object itself with the string `"_interface_t"` appended.
(The C type is typically a `typedef`:ed name for a struct
containing function pointers.) This can be changed by specifying an
explicit `c_type` parameter in the interface object.

For example, to create an alternative implementation of
`io_memory`, we can write:

```
implement io_memory {
  method map(addr_space_t sp, map_info_t info)
         -> (int status)
  {
    ...
  }
  method operation (generic_transaction_t *op, map_info_t info)
         -> (exception_type_t exc)
  {
    ...
  }
}
```

The code generated by `dmlc` for this example assumes that the
`io_memory_interface_t` is a C struct type with fields
`map` and `operation` that can hold function pointers of
the corresponding types. (The `io_memory_interface_t` type is
defined in the standard library file `simics-api.dml`, which is
automatically included by the `dmlc` compiler.)

## Events

An *event* object is an encapsulation of a Simics event that can
be posted on a processor time or step queue. The location of event
objects in the object hierarchy of the device is not important, so an
event object can generally be placed wherever it is most convenient.

An event has a built-in `post` method, which inserts the event in
the default queue associated with the device. When the event is
triggered, the `event` method of the event is called. The
built-in default `event` method simply logs that the event has
occurred, which can be useful for testing purposes.

A posted event can have data associated with it. This data is given to
the `post` method and is provided to the `event`
callback method. The event definition also provided methods for
checkpointing this data for each posted event. If the event uses any
data, it needs to define a `get_event_info` method,
a `set_event_info` method and a `destroy` method.

## Ports

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

