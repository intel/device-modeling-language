<!--
  © 2021-2023 Intel Corporation
  SPDX-License-Identifier: MPL-2.0
-->

# Standard Templates

This chapter describes the standard templates included the DML
library. The templates can be used for both registers and fields. The
templates can be accessed after importing `utility.dml`.

The most common device register functionality is included in the
standard templates.

Note that many standard templates has the same functionality and only
differ by name or log-messages printed when writing or reading
them. The name of the template help developers to get a quick overview
of the device functionality. An example are the *undocumented*
and *reserved* templates. Both have the same
functionality. However, the *undocumented* template hints that
something in the device documentation is unclear or missing, and the
*reserved* template that the register or field should not be used
by software.

The sub-sections use *object* as a combined name for registers
and fields. The sub-sections refers to software and hardware reads and
writes. Software reads and writes are defined as accesses using the
`io_memory` interface (write/reads to memory/io mapped
device). Software reads and writes use the DML built-in read and write
methods. Hardware read writes are defined as accesses using Simics
configuration attributes, using the DML built-in set and get
methods. Device code can still modify a register or device even if
hardware modification is prohibited.

## \_read\_unimplemented

<section>

### Description

The object functionality associated to a read access is unimplemented. Write
access is using default implementation and can be overridden (for instance
by the read\_only template).

### Log Output

First software read to a register results in an unimplemented log-message on
log-level 1, remaining reads on log-level 2. Software reads to fields does
not result in a log-message.

### Parameters

log\_level\_high: sets the high log-level (default is 1, see LOG-OUTPUT)

log\_level\_low: sets the low log-level (default is 2, see LOG-OUTPUT)

### Related Templates

unimplemented, \_write\_unimplemented, silent\_unimplemented, design\_limitation

</section>

## \_write\_unimplemented

<section>

### Description

The object functionality associated to a write access is unimplemented. Read
access is using default implementation and can be overridden (for instance
by the write\_only template).

### Log Output

First software write to registers results in an unimplemented log-message on
log-level 1, remaining writes on log-level 2.  First write to a field (if
field value is not equal to write value) results in an unimplemented
log-message on log-level 1, remaining writes on log-level 2.

### Parameters

log\_level\_high: sets the high log-level (default is 1, see LOG-OUTPUT)

log\_level\_low: sets the low log-level (default is 2, see LOG-OUTPUT)

### Related Templates

unimplemented, \_read\_unimplemented, silent\_unimplemented, design\_limitation

</section>

## checkreserved

<section>

### Description

Check if the value written to a register matches the bits in the
`reserved` parameter, and log a spec_violation when it does.

### Log Output

A spec\_violation message.

</section>

## clear\_on\_read

<section>

### Description

Software reads return the object value. The object value is then reset to 0
as a side-effect of the read.

### Log Output

None.

</section>

## constant

<section>

### Description

The object value will remain constant.
Software and hardware writes does not update the object value.
If this template is used on a register, it will not be checkpointed.

### Log Output

First software write to register or field (if field value is not
equal to write value) results in a spec\_violation log-message on
log-level 1, remaining writes on log-level 2.

### Parameters

value: the constant value

init\_val: can be set instead of `value`, for forward
compatibility with DML 1.4

hard_reset_value: if `value` is `undefined`, this
parameter is used instead

### Related Templates

read\_constant, silent\_constant

</section>

## design\_limitation

<section>

### Description

The object's functionality is not in the model's scope and has been
left unimplemented as a design decision. Software and hardware
writes and reads are implemented as default writes and reads.
Debug registers are a prime example of when to use this
template. This is different from *unimplemented* which is
intended to be implement (if required) but is a limitation in
the current model.

### Related Templates

unimplemented, silent\_unimplemented

</section>

## ignore

<section>

### Description

The objects functionality is unimportant for both hardware and software.
Software and hardware reads return 0. Software and hardware writes
are ignored.

### Log Output

None.

</section>

## ignore\_write

<section>

### Description

Software writes are ignored. This template might also be useful for read-only
fields inside an otherwise writable register. See the documentation for the
read\_only template for more information.

### Log Output

None.

</section>

## no\_reset

<section>

### Description

The register's or field's value will not be changed on a hard or soft reset.

</section>

## noalloc

<section>

### Description

Do not allocate any storage for the object value, i.e.
sets the object's *allocate* parameter to `false`.

### Log Output

None.

</section>

## ones

<section>

### Description

The object is constant all 1's.
Software and hardware writes does not update the object value. The
object value is all 1's.

### Log Output

First software write to register or field (if field value is not
equal to write value) results in a spec\_violation log-message on
log-level 1, remaining writes on log-level 2.

</section>

## read\_constant

<section>

### Description

Software reads return a constant value. No storage is
allocated for the object value.  If this template is used on a register,
it will not be checkpointed.

### Log Output

None.

### Parameters

value: the constant value

read\_val: can be set instead of `value`, for forward
compatibility with DML 1.4

hard_reset_value: if `value` is `undefined`, this
parameter is used instead

### Related Templates

constant, silent\_constant

</section>

## read\_only

<section>

### Description

The object value is read-only for software, the object value can be
modified by hardware.

### Log Output

First software write results in a spec\_violation log-message on
log-level 1, remaining writes on log-level 2. Fields will only log if the
written value is different from the old value.

If the register containing the read-only field also contains writable fields,
it may be better to use the ignore\_write template instead, since software
often do not care about what gets written to a read-only field, causing
unnecessary logging.

</section>

## read\_write

<section>

### Description

The object value can be modified by both software and hardware. Uses the
default read and write methods.

### Log Output

None.

</section>

## read\_zero

<section>

### Description

Software reads return 0.

### Log Output

None.

</section>

## reserved

<section>

### Description

The object is marked reserved and should not be used by software.
Software writes update the object value. Reads are not affected.

### Log Output

First software write to register or field (if field value is not
equal to write value) results in a spec\_violation log-message on
log-level 2. No logs on subsequent writes.

</section>

## scratch

<section>

### Description

The object is a data scratch area.
Uses the default read and write methods.

### Log Output

None.

</section>

## signed

<section>

### Description

Defines the object value to be treated as a signed integer, i.e.
sets the object's *signed* parameter to `true`.

### Log Output

None.

</section>

## silent\_constant

<section>

### Description

The object value will remain constant.
Software and hardware writes does not update the initial object value.
If this template is used on a register, it will not be checkpointed.

### Log Output

None.

### Parameters

value: the constant value

init\_val: can be set instead of `value`, for forward
compatibility with DML 1.4

hard_reset_value: if `value` is `undefined`, this
parameter is used instead

### Related Templates

constant, read\_constant

</section>

## silent\_unimplemented

<section>

### Description

The object functionality is unimplemented, but do not print a
lot of log-messages when reading or writing.
Software and hardware writes and reads are implemented as default writes and
reads.

### Log Output

First software read to a register results in an unimplemented log-message on
log-level 3, remaining reads on log-level 4. Software reads to fields does
not result in a log-message. First software write to a register results in
an unimplemented log-message on log-level 3, remaining writes on log-level 4.
First write to a field (if field value is not equal to write value) results
in an unimplemented log-message on log-level 3, remaining writes on
log-level 4.

### Related Templates

unimplemented

### Related Templates

design\_limitation

</section>

## sticky

<section>

### Description

Do not reset object value on soft-reset, keep current value.

### Log Output

None.

</section>

## undocumented

<section>

### Description

The object functionality is undocumented or poorly documented.
Software and hardware writes and reads are implemented as default writes and
reads.

### Log Output

First software write and read result in a spec\_violation log-message on
log-level 1, remaining on log-level 2.

</section>

## unimplemented

<section>

### Description

The object functionality is unimplemented. Warn when software is using the
object. Software and hardware writes and reads are implemented as default
writes and reads.

### Log Output

First software read to a register results in an unimplemented log-message on
log-level 1, remaining reads on log-level 2. Software reads to fields does
not result in a log-message. First software write to registers results in
an unimplemented log-message on log-level 1, remaining writes on log-level 2.
First write to a field (if field value is not equal to write value) results
in an unimplemented log-message on log-level 1, remaining writes on
log-level 2.

### Parameters

log\_level\_high: sets the high log-level (default is 1, see LOG-OUTPUT)

log\_level\_low: sets the low log-level (default is 2, see LOG-OUTPUT)

### Related Templates

\_read\_unimplemented, \_write\_unimplemented, silent\_unimplemented,
design\_limitation

</section>

## unmapped

<section>

### Description

Make the offset of a register undefined.  This means that it is not mapped
in the address space of the bank it is in.

### Log Output

None.

</section>

## write\_0\_only

<section>

### Description

Software can only set bits to 0.
The new object value is the bitwise AND of the
old object value and the value written by software.

### Log Output

None.

### Related Templates

write\_1\_only

</section>

## write\_1\_clears

<section>

### Description

Software can only clear bits. This feature is often when
hardware sets bits and software clears them to acknowledge.
Software write 1's to clear bits. The new object value is
a bitwise AND of the old object value and the bitwise
complement of the value written by software.

### Log Output

None.

</section>

## write\_1\_only

<section>

### Description

Software can only set bits to 1.
The new object value is the bitwise OR of the
old object value and the value written by software.

### Log Output

None.

### Related Templates

write\_0\_only

</section>

## write\_only

<section>

### Description

The objects value can be modified by software but can't be read back, reads
return 0.

### Log Output

The first time the object is read there is a spec\_violation
log-message on log-level 1, remaining reads on log-level 2.

</section>

## zeros

<section>

### Description

The object is constant all 0's.
Software and hardware writes does not update the object value. The
object value is all 0's.

### Log Output

First software write to register or field (if field value is not
equal to write value) results in a spec\_violation log-message on
log-level 1, remaining writes on log-level 2.

</section>
