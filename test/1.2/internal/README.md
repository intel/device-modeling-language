<!--
  © 2021-2024 Intel Corporation
  SPDX-License-Identifier: MPL-2.0
-->

A new parameter `_confidentiality` (note the leading underscore) was
introduced to allow the DML programmer to hide certain information
from the end user. This parameter is a property of the whole device
and is by default inherited by all the banks and registers defined in
the device. The default value of this parameter is `undefined`, which
means no information hidden is required. Each bank/register/bitfield
can also override the default value hence it is possible to
selectively hide confidential information for individual
banks/registers.

When the _confidentiality parameter is set to the value of a positive
integer, the DML compiler will try to anonymize register/bitfield
names with their offsets in the enclosing bank/register. The
anonymized name will then be used as the corresponding attribute name
when the device is registering as a Simics class. The anonymized name
will also be used in log statements where `$name`/`$qname` is used as part
of the log string. All of this is handled by the DML compiler without
interference from the programmer. Attributes of confidential registers
will be registered as internal attributes and their docstring will not
be registered as part of property of that attribute, hence completely
hidden from the on-line help system. Lastly, bank/register/bitfield
marked as confidential will not be processed when generating the xml
information file for the device.

For convenience, there is a global hidden parameter
`_build_confidentiality` that DML compiler recognized as a way to
specify the confidentiality of the target being built. This parameter
is preferably defined in the Makefile as an input parameter to the DML
compiler and is effectively serving as a threshold confidentiality
value of the whole device. For example, if one has
`DMLC_FLAGS=-D_build_confidentiality=4` then any registers defined
with `_confidentiality` being less than or equal to 4 will *not* be
hidden.
