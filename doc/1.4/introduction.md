<!--
  © 2021 Intel Corporation
  SPDX-License-Identifier: MPL-2.0
-->

# Introduction

Device Modeling Language (DML) is a domain-specific programming language for
developing device models to be simulated with Simics. DML has been designed to
make it easy to represent the kind of things that are needed by a device model,
and uses special syntactic constructs to describe common elements such as memory
mapped hardware registers, connections to other Simics configuration objects,
and checkpointable device state.

DML is an object-oriented language, where each device is represented
through an object, which &mdash; as members &mdash; may feature pieces
of mutable state, configurable parameters and attributes, subroutines
(called methods), and subobjects that may have their own members. In
contrast to most general-purpose object-oriented languages, objects in
DML are statically declared rather than dynamically created, and
typically represent logical components of the device.

A complete DML model specifies exactly one device model, together with:
* Associated register banks, and how these may be memory mapped
* Specifications of connections to other devices that the device expects to have
access to, and thus may make use of.
* Specifications of connections that other devices may establish to the device,
and how messages sent through those connections are handled by the device.
* Specification of attributes that Simics may access for the purposes of
configuring the device, inspect the device operation, or to checkpoint the
device state.
* The name and description of the device, together with other static
meta-information

These are the crucial properties of device models that must be made visible to
Simics, and each of these have specialized language features in order to declare
them. Beyond these, DML also has a number of features to improve the expressive
power of the language and simplify development; for instance, DML offers
*templates*, a powerful metaprogramming tool that allows for code reduction and
reuse, as well as a means of building abstractions.

The DML compiler is called *Device Modeling Language Compiler*,
`dmlc`. It translates a device model description written in DML into C
source code that can be compiled and loaded as a Simics module.

This document describes the DML language, the standard libraries, and
the `dmlc` compiler, as of version 1.4 of DML. See also *Simics Model
Builder User's Guide* for an introduction to DML.

## Source File Example

The following is an example of a small DML model defining a very
simple device. This lacks many details that would appear in a real
device.

```
dml 1.4;

device contraption;

connect wakeup {
    interface signal;
}

bank config_registers {
    register cfg1 size 4 @ 0x0000 {
        field status @ [7:6] is (read, write) {
            method read() -> (uint64) {
                local uint2 ret;
                ret[0] = enable.val;
                ret[1] = coefficient[1] & 1;
                return ret;
            }
        }
        field enable @ [8] is (read_unimpl, write) {
            method write(uint64 val) {
                if (this.val == 0 and val == 1) {
                    wakeup.signal.signal_raise();
                } else if (this.val == 1 and val == 0) {
                    wakeup.signal.signal_lower();
                }
            }
        }
    }

    register coefficient[i < 4] size 8 @ 0x0008 + i * 8 is (read, write) {}
}
```

* The `device contraption;` statement declares the name of the device.

* The `connect wakeup` part declares that the device can be configured
  to communicate with other devices using the `signal` interface.

* The `bank config_registers` part declares a bank of memory-mapped
  registers. If the bank is mapped into a memory space, then software
  can use this to control the device through reads and writes.

* The bank contains *registers*, which declare sizes and offsets
  statically. When the bank is accessed with a memory transaction, it
  will check which register the transaction hits, and redirect as a
  read or write operation in that register.

* The `cfg1` register is further subdivided into *fields*, one
  covering bits 7 and 6 and one covering bit 8.

* The bank, registers and fields form a *hierarchy* of objects, which
  provides a simple mechanism for encapsulation. Each object is a
  separate namespace, and the object hierarchy naturally forms a
  nested scope.

* The `is` statement specifies a number of *templates* to be instantiated for
  the associated object. The `read` and `write` templates prepare code for the
  targeted field which makes it software readable and writable, as well as
  methods `read` and `write` that may be overridden in order to customize the
  behavior upon a software read or write. In contrast, the `read_unimpl`
  template prepares code that causes the field to log a warning if read by
  software.

* Methods in DML are much like functions in C. The statements of the
  method body are similar to C with some differences; e.g., integers
  support bitslicing using the syntax `value[a:b]`. Methods also have
  a place in the object hierarchy, and can freely access the object's
  state and connections.

* `coefficient` is an *array* of register objects, marked by the use
  of <code>[i < <em>size</em>]</code>. The object specification provided
  to an object array is used for each element of the array, and the
  `i` parameter can be used for element-specific logic. In this case,
  `i` is used to assign each register of the array to different
  address offsets.

## Object types

The [above example](#source-file-example) demonstrates how a DML
device is built from a hierarchy of objects, such as banks and
register. The hierarchy is composed of the following object types:

* Each DML model defines a single [`device` object](language.html#the-device),
  which can be instantiated as a configuration object in Simics. All objects
  declared at the top level are members of the device object.

* An [`attribute` object](language.html#attributes) creates a Simics
  configuration attribute of the device. An attribute usually has one of three
  uses:

  * Configuring some aspect of the device on instantiation
  * Saving and restoring simulation state for checkpointing. For
    simple types this is easier to achieve with [saved
    variables](language.html#saved-variables), but attributes can be
    necessary to serialize more complex types.
  * Exposing a back-door into the model for inspection or debugging,
    called a *pseudo* attribute

* A [`bank` object](language.html#register-banks) makes sets of registers
  accessible by placing them in an *address space*. Register banks can be
  individually mapped into Simics memory spaces.

* A [`register` object](language.html#registers) holds an integer value, and is
  generally used to model a hardware register, used for communication via
  memory-mapped I/O. A register is between 1 and 8 bytes wide. Registers divide
  the address space of a bank into discrete elements with non-overlapping
  addresses.

* [`field` objects](language.html#fields) constitute a further subdivision of
   `register` objects, on the bit level. Each field can be accessed separately,
   both for reading and for writing. The fields of a register may not overlap.

* [`group`](language.html#groups) is the general-purpose object type, without
  any special properties or restrictions. Groups are mainly used as container
  objects &mdash; in particular, to define logical groups of registers within a
  bank. The generic nature of groups also makes them useful as a tool for
  creating abstractions.

* A [`connect` object](language.html#connects) holds a reference to a Simics
  configuration object. (Typically, the connected object is expected to
  implement some particular Simics-interface.) An attribute with the same name
  is added to the `device`; thus, a `connect` is similar to a simple `attribute`
  object. Usually, initialization is done when the device is configured, e.g.
  when loading a checkpoint or instantiating a component.

* An [`interface` object](language.html#interfaces) may be declared within a
  `connect` object, and specifies a Simics interface assumed to be implemented
  by the connected object. In many cases, the name of the interface is
  sufficient, and the body of the object can be left empty.

* A [`port` object](language.html#ports) represents a point where an outside
  device can connect to this device. This is done by creating a separate Simics
  object; if a device has a declaration `port irq` and the device is
  instantiated as `dev` in Simics, then the port object is named `dev.port.irq`.

* An [`implement` object](language.html#implements) specifies an implementation
  of a *Simics interface* that the device implements. An `implement` object is
  normally declared inside a `port`, and defines the interfaces registered on
  the corresponding Simics configuration object; however, `implement` can also
  be declared on the top-level `device` object or in a `bank` object.

  The methods defined within the `implement` object must correspond to
  the functions of the Simics interface.

  A device can implement the same interface several times, by creating
  multiple `port` objects with `implement` objects of the same name.

* An [`event` object](language.html#events) is an encapsulation of a Simics
  event, that can be posted on a time queue (CPU or clock).

* A [`subdevice` object](language.html#subdevices) represents a subsystem of a
  device, which can contain its own ports, banks, and attributes.

## Methods and Mutable Variables
[Methods](language.html#methods-detailed) are the DML representation of
subroutines. They may be declared as members of any object or template. Any
method may have multiple input parameters, specified similarly as C functions.
Unlike C, DML methods may have multiple return values, and the lack of a return
value is indicated through an empty list of return values rather than `void`.
The following demonstrates a method declaration with no input parameters or
return values:

```
method noop() -> () {
    return;
}
```

Alternatively:

```
method noop() {
    return;
}
```

The following demonstrates a method declaration with multiple input
and parameters and return values:

```
method div_mod(uint64 dividend, uint64 divisor)
    -> (uint64, uint64) {
    local uint64 quot = dividend / divisor;
    local uint64 rem = dividend % divisor;
    return (quot, rem);
}
```

This also demonstrates how local, stack-allocated variables within methods may
be declared; through the `local` keyword. This is analogous to C’s `auto`
variable kind, but unlike C, the keyword must be explicitly given. DML features
two other variable kinds: [`session`](language.html#session-variables) and
[`saved`](language.html#session-variables). Unlike `local` variables, `session`
and `saved` variables may also be declared as members of any object within the
DML model, and can only be initialized with constant expressions.

`session` variables represent statically allocated variables, and act as the
DML equivalent of static variables in C. The value of a `session` variable
is preserved for the duration of the current simulation session, but are not
automatically serialized and restored during checkpointing. This means that
it is the model developer’s responsibility to manually serialize and restore
any `session` variables upon saving or restoring a checkpoint.
`saved` variables behave exactly like `session` variables, except the value of
`saved` variables are serialized and restored during checkpointing. Because of
this, a `saved` variable must be of a type that DML knows how to serialize.
Most built-in non-pointer C types are serializable, and any `struct`
that consists solely of serializable types are also considered serializable.
Pointer types are never considered serializable.

Methods have access to a basic exception-handling mechanism through the [`throw`
statement](language.html#throw-statements), which raises an exception without
associated data. Such exceptions may be caught via the [`try { ... } except {
... }` statement](language.html#try-statements). If a method may throw an
uncaught exception, that method must be declared `throws`; for example:

```
method demand(bool condition) throws {
    if (!condition) {
        throw;
    }
}
```

## Templates and Parameters
A [*template*](language.html#templates) specifies a block of code that may be
inserted into objects. Templates may only be declared at the top-level, which is
done as follows:

<pre>
template <em>name</em> { <em>body</em> }
</pre>
where *name* is the name of the template, and *body* is a set of object
statements. A template may be instantiated through the `is` object statement,
which can be used within either objects, or within templates. For example:
```
bank regs {
    // Instantiate a single template: templateA
    is templateA;
    // Instantiate multiple templates: templateB and templateC
    is (templateB, templateC);

    register reg size 1 @0x0;
}
```
The `is` object statement causes the body of the specified templates to be
injected into the object or template in which the statement was
used.

`is` can also be used in a more idiomatic fashion together with the declaration
of an object or template as follows:

```
// Instantiate templates templateA, templateB, and templateC
bank regs is (templateA, templateB, templateC) {
    register reg size 1 @0x0;
}
```

A language feature closely related to templates are
[*parameters*](language.html#parameters). A parameter can be thought of as an
*expression macro* that is a member of a particular object or template.
Parameters may optionally be declared without an accompanying definition &mdash;
which will result in a compile-time error if not overridden &mdash; or with a
*default*, overridable definition. Parameters declared this way can be
overridden by any later declaration of the same parameter. This can be leveraged
by templates in order to declare a parameter that the template may make use of,
while requiring any instance of the template to provide a definition for the
parameter (or allow instances to override the default definition of that
parameter).

Parameters are declared as follows:
* Abstract, without definition:
  <pre>
  param <em>name</em>;
  </pre>
* With overridable definition:
  <pre>
  param <em>name</em> default <em>expression</em>;
  </pre>
* With unoverridable definition:
  <pre>
  param <em>name</em> = <em>expression</em>;
  </pre>

Much of the DML infrastructure, as well as DML’s built-in features, rely heavily
on templates. Due to the importance of templates, DML has a number of features
to generically manipulate and reference template instances, both at compile time
and at run time. These are [*templates as
types*](language.html#templates-as-types), [`each`-`in`
expressions](language.html#each-in-expressions), and [`in each`
declarations](language.html#in-each-declarations).
