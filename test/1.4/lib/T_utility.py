# © 2021-2023 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest
import dev_util
import sim_commands
import contextlib

[read_write,
 ignore_write,
 read_zero,
 read_only,
 reserved,
 constant0,
 constant1,
 constant_init,
 silent_constant,
 zeros,
 ones,
 ignore,
 undocumented0,
 undocumented1,
 read_constant,
 read_constant_field,
] = [
    dev_util.Register_LE(obj.bank.b, offs, size=4)
    for offs in [4, 8, 12, 16, 52, 56, 60, 64, 68, 72, 76, 80, 84, 88,
                 92, 96]]

# Check attributes in python
obj.simple_bool = True
obj.simple_uint64 = 1
obj.simple_int64 = -2
obj.simple_double = 3.14
stest.expect_equal(obj.simple_bool, True)
stest.expect_equal(obj.simple_uint64, 1)
stest.expect_equal(obj.simple_int64, -2)
stest.expect_equal(obj.simple_double, 3.14)

obj.b_read_write = 0x11111111
stest.expect_equal(read_write.read(), 0x11111111)
read_write.write(0xdeadbeef)
stest.expect_equal(read_write.read(), 0xdeadbeef)

obj.b_ignore_write = 0xdeadbeef
stest.expect_equal(ignore_write.read(), 0xdeadbeef)
ignore_write.write(0)
stest.expect_equal(ignore_write.read(), 0xdeadbeef)

obj.b_read_zero = 0xdeadbeef
stest.expect_equal(read_zero.read(), 0)
read_zero.write(0x11111111)
stest.expect_equal(obj.b_read_zero, 0x11111111)

class LogCapture(object):
    def __init__(self, kind='spec-viol'):
        self.expected_kind = list(conf.sim.log_types).index(kind)
        self.messages = []
        self.filter = sim_commands.logger.filter(self.callback)
    def __enter__(self):
        self.filter.__enter__()
        return self
    def __exit__(self, *args):
        return self.filter.__exit__(*args)
    def callback(self, obj_, kind, msg):
        stest.expect_equal(obj_, obj)
        stest.expect_equal(kind, self.expected_kind,
                           'unexpected log type for message %r' % (msg,))
        self.messages.append(msg)

obj.b_read_only = 0xdeadbeef
obj.log_level = 1
stest.expect_equal(read_only.read(), 0xdeadbeef)
# partial access, second byte of register
read_only_1 = dev_util.Register_LE(obj.bank.b, 17, size=1)
with LogCapture() as capture, stest.allow_log_mgr(obj, 'spec-viol'):
    read_only.write(0xdeadbeef)
    # remaining on log-level 2
    read_only.write(0x11111111)
    obj.log_level = 2
    read_only_1.write(0xbe)
    stest.expect_equal(capture.messages, [
        'Write to read-only register b.read_only'
        + ' (value written = 0xdeadbeef, contents = 0xdeadbeef).',
        'Write to read-only register b.read_only'
        + ' (value written = 0xbe00, contents = 0xdeadbeef).'])
obj.log_level = 1
# second time, log level 2
read_only.write(0x11111111)
obj.log_level = 2
with stest.expect_log_mgr(obj, 'spec-viol'):
    read_only.write(0x11111111)

with LogCapture() as capture, stest.allow_log_mgr(obj, 'spec-viol'):
    # read is OK, and fetches current value
    obj.log_level = 3
    obj.b_reserved = 0xdeadbeef
    stest.expect_equal(reserved.read(), 0xdeadbeef)
    stest.expect_equal(capture.messages, [])
    reserved.write(0xdeadbeef)
    obj.log_level = 2
    # log on level 2 on first write, even if value isn't changed
    stest.expect_equal(capture.messages, [
        "Write to reserved register b.reserved (value written = 0xdeadbeef,"
        + " contents = 0xdeadbeef), will not warn again."])
    del capture.messages[:]
    obj.log_level = 3
    # no subsequent logging
    reserved.write(0x12345678)
    # writes update the value
    stest.expect_equal(obj.b_reserved, 0x12345678)
    stest.expect_equal(capture.messages, [])
obj.log_level = 1

fields0 = dev_util.Register_LE(
    obj.bank.b, 0, size=1, bitfield=dev_util.Bitfield_LE({
        'read_write': 0,
        'ignore_write': 1,
        'read_zero': 2,
        'read_only': 3,
        'reserved1': 4,
        'reserved2': 5}))

with LogCapture() as capture, stest.allow_log_mgr(obj, 'spec-viol'):
    stest.expect_equal(fields0.read(), 0)
    fields0.write(read_only=0)
    # no messages when writing the same value from read_only reg
    stest.expect_equal(capture.messages, [])
    fields0.write(0, read_write=1, ignore_write=1, read_zero=1, read_only=1,
                  reserved1=1, reserved2=0)
    stest.expect_equal(obj.b_fields0, fields0.bitfield.value(
        read_write=1, read_zero=1, reserved1=1))
    # message logged on level 2 (hidden) for reserved1. Nothing logged
    # on reserved2 (unchanged)
    stest.expect_equal(capture.messages, [
        'Write to read-only field b.fields0.read_only'
        + ' (value written = 0x1, contents = 0).'])

    del capture.messages[:]
    # second time: read_only logs on level 2 (hidden), reserved logs nothing
    fields0.write(0, read_write=1, ignore_write=1, read_zero=1, read_only=1,
                  reserved1=1, reserved2=0)
    stest.expect_equal(capture.messages, [])
    # third time: read_only logs on level 2, reserved2 logs on level 2
    obj.log_level = 2
    obj.b_fields0 = fields0.bitfield.value(reserved2=1)
    fields0.write(0, read_write=1, ignore_write=1, read_zero=1, read_only=1,
                  reserved1=1, reserved2=0)
    stest.expect_equal(capture.messages, [
        'Write to read-only field b.fields0.read_only'
        + ' (value written = 0x1, contents = 0).',
        'Write to reserved field b.fields0.reserved2'
        + ' (value written = 0, contents = 0x1), will not warn again.'])
    stest.expect_equal(obj.b_fields0, fields0.bitfield.value(
        read_write=1, read_zero=1, reserved1=1))
obj.log_level = 1

obj.b_fields0 = 0xf
stest.expect_equal(obj.b_fields0, 0xf)

[write_1_clears, clear_on_read, write_1_only, write_0_only] = [
     dev_util.Register_LE(obj.bank.b, offs + 1, size=2)
     for offs in [20, 24, 28, 32]]

obj.b_write_1_clears = 0x12345678
write_1_clears.write(0xff00)
stest.expect_equal(write_1_clears.read(), 0x0056)
stest.expect_equal(obj.b_write_1_clears, 0x12005678)

obj.b_clear_on_read = 0x12345678
# write works as normal
clear_on_read.write(0xdead)
stest.expect_equal(obj.b_clear_on_read, 0x12dead78)
# read returns current value but clears entire register
stest.expect_equal(clear_on_read.read(), 0xdead)
stest.expect_equal(obj.b_clear_on_read, 0)

obj.b_write_1_only = 0x12345678
write_1_only.write(0xff00)
stest.expect_equal(write_1_only.read(), 0xff56)
stest.expect_equal(obj.b_write_1_only, 0x12ff5678)

obj.b_write_0_only = 0x12345678
write_0_only.write(0xff00)
stest.expect_equal(write_0_only.read(), 0x3400)
stest.expect_equal(obj.b_write_0_only, 0x12340078)

# read_constant: always reads out a constant, but does not affect
# write behaviour.
read_constant.write(0x22)
read_constant_field.write(0x22)
stest.expect_equal(obj.b_read_constant, 0x22)
stest.expect_equal(obj.b_read_constant_field, 0x22)
stest.expect_equal(read_constant.read(), 0x44)
stest.expect_equal(read_constant_field.read(), 0x44)

fields1 = dev_util.Register_LE(
    obj.bank.b, 1, size=1, bitfield=dev_util.Bitfield_LE({
        'write_0_only': (7, 6),
        'write_1_only': (5, 4),
        'clear_on_read': (3, 2),
        'write_1_clears': (1, 0)}))

obj.b_fields1 = 0x55
fields1.write(0xff)
stest.expect_equal(obj.b_fields1, fields1.bitfield.value(
    write_0_only=1, write_1_only=3, clear_on_read=3))
obj.b_fields1 = 0xaa
fields1.write(0x55)
stest.expect_equal(obj.b_fields1, fields1.bitfield.value(
    write_1_only=3, clear_on_read=1, write_1_clears=2))
obj.b_fields1 = 0x55
stest.expect_equal(fields1.read(), 0x55)
# clear_on_read field cleared!
stest.expect_equal(fields1.read(), 0x51)

write_only = dev_util.Register_LE(obj.bank.b, 100, size=4)
write_only.write(0xdeadbeef)
stest.expect_equal(obj.b_write_only, 0xdeadbeef)

with LogCapture() as capture, stest.allow_log_mgr(obj, 'spec-viol'):
    stest.expect_equal(write_only.read(), 0)
    stest.expect_equal(capture.messages, [
        'Read from write-only register b.write_only (returning 0).'])

# second time, log level 2
stest.expect_equal(write_only.read(), 0) # log-level filters spec-viol filtered
obj.log_level = 2
with stest.expect_log_mgr(obj, 'spec-viol'):
    stest.expect_equal(write_only.read(), 0)

obj.log_level = 1

# Access outside fields
(fields3_0, fields3_2_0, fields3_2_1) = (dev_util.Register_LE(
    obj.bank.b, offset, size=1, bitfield=dev_util.Bitfield_LE({
        'f76': (7, 6), 'y': 5, 'f43': (4, 3), 'x': 2, 'f10': (1, 0)}))
                                     for offset in (3, 128, 129))
with LogCapture() as capture, stest.allow_log_mgr(obj, 'spec-viol'):
    fields3_0.write(0)
    stest.expect_equal(capture.messages, [])

    fields3_2_0.write(f76=3, f43=3, f10=3)
    # writes outside fields are masked out
    stest.expect_equal(obj.b_fields3_2[0], 0)
    # and one message is output
    stest.expect_equal(
        capture.messages,
        ["Write outside fields in register b.fields3_2[0], bitranges;%s" % "".join(
            ["\n\t%s (value written = 0b11, previous value = 0b00)" % bits
             for bits in ["7:6", "4:3", "1:0"]])])
    del capture.messages[:]

    obj.b_fields3_2[0] = fields3_2_0.bitfield.value(f43=3)
    # outside bits are not masked out by set (they were in DML 1.2)
    stest.expect_equal(obj.b_fields3_2[0], fields3_2_0.bitfield.value(f43=3))

    obj.b_fields3_2[1] = fields3_2_1.bitfield.value(f43=3)
    # No message for bit 4:3 this time, because it didn't change
    fields3_2_1.write(f76=1, f43=3, f10=2)
    stest.expect_equal(
        capture.messages,
        ["Write outside fields in register b.fields3_2[1], bitranges;%s" % "".join(
            ["\n\t%s (value written = 0b%s, previous value = 0b00)" % (bits,
                                                                       val)
             for (bits, val) in [("7:6", "10"), ("1:0", "01")]])])

[unimpl, read_unimpl, write_unimpl, silent_unimpl] = [
     dev_util.Register_LE(obj.bank.b, offs, size=4)
     for offs in [36, 40, 44, 48]]
fields = dev_util.Register_LE(
    obj.bank.b, 2, size=1, bitfield=dev_util.Bitfield_LE({
        'unimpl': 0, 'read_unimpl': 1, 'write_unimpl': 2, 'silent_unimpl': 3}))
with LogCapture('unimpl') as capture:
    unimpl.write(5)
    read_unimpl.write(5)
    write_unimpl.write(5)
    silent_unimpl.write(5)
    unimpl.read()
    read_unimpl.read()
    write_unimpl.read()
    silent_unimpl.read()
    # accesses to silent_unimpl are suppressed
    stest.expect_equal(capture.messages, [
        "Write to unimplemented register b.unimpl (value written "
        + "= 0x5, contents = 0).",
        "Write to unimplemented register b.write_unimpl (value "
        + "written = 0x5, contents = 0).",
        "Read from unimplemented register b.unimpl (contents = 0x5).",
        "Read from unimplemented register b.read_unimpl (contents = 0x5)."
    ])
    del capture.messages[:]

    obj.log_level = 2
    # Field reads do not log
    fields.write(0xf)
    fields.read()
    stest.expect_equal(capture.messages, [
        "Write to unimplemented field b.fields2.unimpl (value written = 0x1, contents = 0).",
        "Write to unimplemented field b.fields2.write_unimpl (value written = 0x1, contents = 0).",
        "Write to unimplemented field b.fields2.silent_unimpl (value written = 0x1, contents = 0).",
    ])
    del capture.messages[:]

    # repeated accesses are not logged on level 2 ...
    unimpl.write(4)
    write_unimpl.write(4)
    silent_unimpl.write(4)
    unimpl.read()
    read_unimpl.read()
    silent_unimpl.read()
    fields.read()
    stest.expect_equal(capture.messages, [])

    # ... but they are logged on level 3
    obj.log_level = 3
    for reg in [unimpl, write_unimpl, silent_unimpl]:
        reg.write(5)
    for reg in [unimpl, read_unimpl, silent_unimpl]:
        reg.read()
    stest.expect_equal(len(capture.messages), 6)
    del capture.messages[:]

    # Re-writing the same value to fields yield no message ...
    fields.write(0xf)
    stest.expect_equal(capture.messages, [])
    # ... but writing a different value does (except in read_unimpl)
    fields.write(0)
    stest.expect_equal(len(capture.messages), 3)
    del capture.messages[:]

fields4 = dev_util.Register_LE(
    obj.bank.b, 104, size=1, bitfield=dev_util.Bitfield_LE({
        'constant0': 0,
        'constant1': (2, 1)}))

fields5 = dev_util.Register_LE(
    obj.bank.b, 108, size=1, bitfield=dev_util.Bitfield_LE({
        'constant0': 0,
        'constant1': (2, 1)}))

with LogCapture() as capture, stest.allow_log_mgr(obj, "spec-viol"):
    # Check initial values
    stest.expect_equal(obj.b_constant0, 0)
    stest.expect_equal(obj.b_constant1, 1)
    stest.expect_equal(obj.b_fields4,
                       fields4.bitfield.value(
                           constant0=0, constant1=3))
    # No logs when setting attributes directly
    obj.log_level = 4
    obj.b_constant0 = 1
    obj.b_constant1 = 3
    obj.b_fields4 = fields4.bitfield.value(
        constant0=1, constant1=0)
    # Check that values changed
    stest.expect_equal(obj.b_constant0, 1)
    stest.expect_equal(obj.b_constant1, 3)
    stest.expect_equal(obj.b_fields4,
                       fields4.bitfield.value(
                           constant0=1, constant1=0))
    # no message if value matches
    obj.b_constant0 = 1
    obj.b_constant1 = 3
    obj.b_fields4 = fields4.bitfield.value(
        constant0=1, constant1=3)
    stest.expect_equal(capture.messages, [])
    # reset the values
    obj.b_constant0 = 0
    obj.b_constant1 = 1
    obj.b_fields4 = fields4.bitfield.value(
        constant0=0, constant1=3)
    stest.expect_equal(capture.messages, [])

with LogCapture() as capture, stest.allow_log_mgr(obj, "spec-viol"):
    # reading constant field or register produces log on level 4 from
    # builtin, but no log on level 3
    obj.log_level = 3
    constant0.read()
    constant1.read()
    fields4.read()
    stest.expect_equal(capture.messages, [])
    # even on log-level 1, no messages if writing equal to fields
    obj.log_level = 1
    fields4.write(fields4.bitfield.value(
        constant0=0, constant1=3))
    stest.expect_equal(capture.messages, [])
    # message when writing different value
    constant0.write(1)
    fields4.write(fields4.bitfield.value(
        constant0=0, constant1=0))
    stest.expect_equal(
        capture.messages,
        ["Write to constant register b.constant0 "
         "(value written = 0x1, contents = 0).",
         "Write to constant field b.fields4.constant1 "
         "(value written = 0, contents = 0x3)."])
    # and no change in value
    stest.expect_equal(constant0.read(), 0)
    stest.expect_equal(fields4.read(),
                       fields4.bitfield.value(
                           constant0=0, constant1=3))
    del capture.messages[:]
    # remaining messages are on log-level 2
    constant0.write(1)
    constant1.write(0)
    fields4.write(fields4.bitfield.value(
        constant0=1, constant1=0))
    stest.expect_equal(
        capture.messages,
        ["Write to constant register b.constant1 "
         "(value written = 0, contents = 0x1).",
         "Write to constant field b.fields4.constant0 "
         "(value written = 0x1, contents = 0)."])
    del capture.messages[:]
    # repeated messages on log-level 2
    obj.log_level = 2
    constant0.write(10)
    constant1.write(10)
    fields4.write(fields4.bitfield.value(
        constant0=1, constant1=0))
    stest.expect_equal(len(capture.messages), 4)
    del capture.messages[:]
    # Check initial value of register defined
    # with 'init_val' instead of 'value'
    stest.expect_equal(obj.b_constant_init, 2)
    stest.expect_equal(constant_init.read(), 2)

    # silent constants do not output log messages
    # so expect error message from the non-silent
    # constant field here only
    obj.log_level = 3
    silent_constant.write(0)
    fields5.write(fields5.bitfield.value(
        constant0=1, constant1=1));
    stest.expect_equal(
        capture.messages,
        ["Write to constant field b.fields5.constant0 "
         "(value written = 0x1, contents = 0)."])
    del capture.messages[:]

fields6 = dev_util.Register_LE(
    obj.bank.b, 112, size=1, bitfield=dev_util.Bitfield_LE({
        'zeros': 0,
        'ones': 1,
        'many_ones' : (5, 2)}))

with LogCapture() as capture, stest.allow_log_mgr(obj, "spec-viol"):
    # check that values of zeros and ones are as expected
    stest.expect_equal(zeros.read(), 0)
    stest.expect_equal(ones.read(), 0xffffffff)
    stest.expect_equal(fields6.read(),
                       fields6.bitfield.value(
                           zeros = 0,
                           ones = 1,
                           many_ones = 0xf))
    # check that writing gets the expected error messages
    zeros.write(1)
    ones.write(0)
    fields6.write(fields6.bitfield.value(
        zeros = 1, ones = 0, many_ones = 0xe))
    stest.expect_equal(
        capture.messages,
        ["Write to constant register b.zeros "
         + "(value written = 0x1, contents = 0).",
         "Write to constant register b.ones "
         + "(value written = 0, contents = 0xffffffff).",
         "Write to constant field b.fields6.zeros "
         + "(value written = 0x1, contents = 0).",
         "Write to constant field b.fields6.ones "
         + "(value written = 0, contents = 0x1).",
         "Write to constant field b.fields6.many_ones "
         + "(value written = 0xe, contents = 0xf)."])
    del capture.messages[:]

fields7 = dev_util.Register_LE(
    obj.bank.b, 116, size=1, bitfield=dev_util.Bitfield_LE({
        'ignore': 0}))

# No logs from ignore
with LogCapture() as capture:
    # Writing values does not matter
    obj.log_level = 3
    ignore.write(2)
    fields7.write(fields7.bitfield.value(ignore = 1))
    stest.expect_equal(ignore.read(), 0)
    stest.expect_equal(obj.b_fields7, 0)
    fields7.write(fields7.bitfield.value(ignore = 1))
    stest.expect_equal(fields7.read(), 0)
    stest.expect_equal(capture.messages, [])
    # Setting attribute does change the value
    obj.b_fields7 = fields7.bitfield.value(ignore = 1)
    obj.b_ignore = 2
    stest.expect_equal(fields7.read(), fields7.bitfield.value(ignore = 1))
    stest.expect_equal(ignore.read(), 2)
    stest.expect_equal(capture.messages, [])

fields8 = dev_util.Register_LE(
    obj.bank.b, 120, size=1, bitfield=dev_util.Bitfield_LE({
        'undocumented': 0}))

with LogCapture() as capture, stest.allow_log_mgr(obj, "spec-viol"):
    # even reads give messages on log-level 1 first time
    obj.log_level = 1
    undocumented0.read()
    fields8.read()
    stest.expect_equal(
        capture.messages,
        ["Read from poorly or non-documented register b.undocumented0 "
         + "(contents = 0).",
         "Read from poorly or non-documented field b.fields8.undocumented "
         + "(contents = 0)."])
    del capture.messages[:]
    # but similar operation does not show on log-level 1
    undocumented0.read()
    fields8.read()
    stest.expect_equal(capture.messages, [])
    # and then on log-level 2 they appear again
    obj.log_level = 2
    undocumented0.read()
    undocumented1.read()
    fields8.read()
    stest.expect_equal(len(capture.messages), 3)
    del capture.messages[:]
    # check similar for writes, even when writing similar
    undocumented0.write(0)
    fields8.write(0)
    stest.expect_equal(
        capture.messages,
        ["Write to poorly or non-documented register b.undocumented0 "
         + "(value written = 0, contents = 0).",
        "Write to poorly or non-documented field b.fields8.undocumented "
         + "(value written = 0, contents = 0)."])
    del capture.messages[:]
    # check that we get no additional messages on log-level 1
    obj.log_level = 1
    undocumented0.write(1)
    fields8.write(fields8.bitfield.value(undocumented = 1))
    stest.expect_equal(capture.messages, [])
    # check that write still goes through
    stest.expect_equal(obj.b_undocumented0, 1)
    stest.expect_equal(obj.b_fields8, fields8.bitfield.value(undocumented = 1))
    obj.log_level = 2
