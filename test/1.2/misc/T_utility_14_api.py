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
] = [
    dev_util.Register_LE(obj.bank.b, offs, size=4)
    for offs in [4, 8, 12, 16, 52, 56, 60, 64, 68, 72, 76, 80, 84, 88]]

# TODO: Currently DML 1.2 logs on bank objects and DML 1.4 logs on conf_objects
#       not sure which is correct
# Check 1.4 attribute API in python
obj.simple_bool = True
obj.simple_uint64 = 1
obj.simple_int64 = -2
obj.simple_double = 3.14
stest.expect_equal(obj.simple_bool, True)
stest.expect_equal(obj.simple_uint64, 1)
stest.expect_equal(obj.simple_int64, -2)
stest.expect_equal(obj.simple_double, 3.14)

# We do not check the contents/type of log messages here, since that can change
# between 1.2 and 1.4, we only check that general semantics are reasonable

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
    def __init__(self):
        self.messages = []
        self.filter = sim_commands.logger.filter(self.callback)
    def __enter__(self):
        self.filter.__enter__()
        return self
    def __exit__(self, *args):
        return self.filter.__exit__(*args)
    def callback(self, obj_, kind, msg):
        stest.expect_equal(obj_, obj.bank.b)
        self.messages.append(msg)

class AllowLogs(object):
    def __init__(self):
        self.allow_spec = stest.allow_log_mgr(obj, 'spec-viol')
        self.allow_info = stest.allow_log_mgr(obj, 'info')
        self.allow_error = stest.allow_log_mgr(obj, 'error')
    def __enter__(self):
        stest.untrap_log('error')
        stest.untrap_log('spec-viol')
        self.allow_spec.__enter__()
        self.allow_info.__enter__()
        self.allow_error.__enter__()
    def __exit__(self, *args):
        stest.trap_log('error')
        stest.trap_log('spec-viol')
        self.allow_spec.__exit__(*args)
        self.allow_info.__exit__(*args)
        self.allow_error.__exit__(*args)

# Logs from bank
def set_log_level(level):
    obj.bank.b.log_level = level

obj.b_read_only = 0xdeadbeef
set_log_level(1)
stest.expect_equal(read_only.read(), 0xdeadbeef)
with LogCapture() as capture, AllowLogs():
    read_only.write(0xdeadbeef) # error even on write same value on register
    stest.expect_equal(len(capture.messages), 1)
    # second time, log level 2
    read_only.write(0x11111111)
    stest.expect_equal(len(capture.messages), 1)
    obj.bank.b.log_level = 2
    read_only.write(0x11111112)
    stest.expect_equal(len(capture.messages), 2)
    del capture.messages[:]

with LogCapture() as capture, AllowLogs():
    # read is OK, and fetches current value
    set_log_level(3)
    obj.b_reserved = 0xdeadbeef
    stest.expect_equal(reserved.read(), 0xdeadbeef)
    stest.expect_equal(capture.messages, [])
    set_log_level(2)
    reserved.write(0xdeadbeef)
    # log on level 2 on first write, even if value isn't changed
    stest.expect_equal(len(capture.messages), 1)
    set_log_level(3)
    # no subsequent logging
    reserved.write(0x12345678)
    # writes update the value
    stest.expect_equal(obj.b_reserved, 0x12345678)
    stest.expect_equal(len(capture.messages), 1)
set_log_level(1)

fields0 = dev_util.Register_LE(
    (obj, 'b', 0), size=1, bitfield=dev_util.Bitfield_LE({
        'read_write': 0,
        'ignore_write': 1,
        'read_zero': 2,
        'read_only': 3,
        'reserved1': 4,
        'reserved2': 5}))

with LogCapture() as capture, AllowLogs():
    stest.expect_equal(fields0.read(), 0)
    fields0.write(0, read_write=1, ignore_write=1, read_zero=1, read_only=1,
                  reserved1=1, reserved2=0)
    stest.expect_equal(obj.b_fields0, fields0.bitfield.value(
        read_write=1, read_zero=1, reserved1=1))
    # message logged on level 2 (hidden) for reserved1. Nothing logged
    # on reserved2 (unchanged)
    stest.expect_equal(len(capture.messages), 1)
    del capture.messages[:]
    # second time: read_only logs on level 2 (hidden), reserved logs nothing
    fields0.write(0, read_write=1, ignore_write=1, read_zero=1, read_only=1,
                  reserved1=1, reserved2=0)
    stest.expect_equal(capture.messages, [])
    # third time: read_only logs on level 2, reserved2 logs on level 2
    set_log_level(2)
    obj.b_fields0 = fields0.bitfield.value(reserved2=1)
    fields0.write(0, read_write=1, ignore_write=1, read_zero=1, read_only=1,
                  reserved1=1, reserved2=0)
    stest.expect_equal(len(capture.messages), 2)
    stest.expect_equal(obj.b_fields0, fields0.bitfield.value(
        read_write=1, read_zero=1, reserved1=1))
set_log_level(1)

obj.b_fields0 = 0xf
stest.expect_equal(obj.b_fields0, 0xf)

# In 1.4, write_1_clears supports partial access, but not in 1.2
[write_1_clears, clear_on_read, write_1_only, write_0_only] = [
     dev_util.Register_LE(obj.bank.b, offs, size=4)
     for offs in [20, 24, 28, 32]]

obj.b_write_1_clears = 0x12345678
write_1_clears.write(0xff0000)
stest.expect_equal(write_1_clears.read(), 0x12005678)
stest.expect_equal(obj.b_write_1_clears, 0x12005678)

obj.b_clear_on_read = 0x12345678
# write works as normal
clear_on_read.write(0x1234dead)
stest.expect_equal(obj.b_clear_on_read, 0x1234dead)
# read returns current value but clears entire register
stest.expect_equal(clear_on_read.read(), 0x1234dead)
stest.expect_equal(obj.b_clear_on_read, 0)

obj.b_write_1_only = 0x12345678
write_1_only.write(0xff0000)
stest.expect_equal(write_1_only.read(), 0x12ff5678)
stest.expect_equal(obj.b_write_1_only, 0x12ff5678)

obj.b_write_0_only = 0x12345678
write_0_only.write(0xff0000)
stest.expect_equal(write_0_only.read(), 0x00340000)
stest.expect_equal(obj.b_write_0_only, 0x00340000)

fields1 = dev_util.Register_LE(
    (obj, 'b', 1), size=1, bitfield=dev_util.Bitfield_LE({
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

with LogCapture() as capture, AllowLogs():
    stest.expect_equal(write_only.read(), 0)
    stest.expect_equal(len(capture.messages), 1)

# second time, log level 2
stest.expect_equal(write_only.read(), 0) # log-level filters spec-viol filtered
set_log_level(2)
with LogCapture() as capture, AllowLogs():
    stest.expect_equal(write_only.read(), 0)
    stest.expect_equal(len(capture.messages), 1)
set_log_level(1)

# Access outside fields
fields3 = dev_util.Register_LE(
    (obj, 'b', 3), size=1, bitfield=dev_util.Bitfield_LE({
        'f76': (7, 6), 'y': 5, 'f43': (4, 3), 'x': 2, 'f10': (1, 0)}))
with LogCapture() as capture, AllowLogs():
    fields3.write(0)
    stest.expect_equal(capture.messages, [])
    fields3.write(f76=3, f43=3, f10=3)
    # writes outside fields are masked out
    stest.expect_equal(obj.b_fields3, 0)
    # .. and one message is written for each bit range

    obj.b_fields3 = fields3.bitfield.value(f43=3)

fields4 = dev_util.Register_LE(
    (obj, 'b', 104), size=1, bitfield=dev_util.Bitfield_LE({
        'constant0': 0,
        'constant1': (2, 1)}))

fields5 = dev_util.Register_LE(
    (obj, 'b', 108), size=1, bitfield=dev_util.Bitfield_LE({
        'constant0': 0,
        'constant1': (2, 1)}))

with LogCapture() as capture, AllowLogs():
    # Check initial values
    stest.expect_equal(obj.b_constant0, 0)
    stest.expect_equal(obj.b_constant1, 1)
    stest.expect_equal(obj.b_fields4,
                       fields4.bitfield.value(
                           constant0=0, constant1=3))
    # reading constant field or register produces no log
    set_log_level(2)
    constant0.read()
    constant1.read()
    fields4.read()
    stest.expect_equal(capture.messages, [])
    # even on log-level 1, no messages if writing equal to fields
    set_log_level(1)
    fields4.write(fields4.bitfield.value(
        constant0=0, constant1=3))
    stest.expect_equal(capture.messages, [])
    # message when writing different value
    constant0.write(1)
    fields4.write(fields4.bitfield.value(
        constant0=0, constant1=0))
    stest.expect_equal(len(capture.messages), 2)
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
    stest.expect_equal(len(capture.messages), 2)
    del capture.messages[:]
    # repeated messages on log-level 2
    set_log_level(2)
    constant0.write(10)
    constant1.write(10)
    fields4.write(fields4.bitfield.value(
        constant0=1, constant1=0))
    stest.expect_equal(len(capture.messages), 4)
    del capture.messages[:]

    # silent constants do not output log messages
    # so expect error message from the non-silent
    # constant field here only
    set_log_level(3)
    silent_constant.write(0)
    fields5.write(fields5.bitfield.value(
        constant0=1, constant1=1))
    stest.expect_equal(len(capture.messages), 1)
    del capture.messages[:]

fields6 = dev_util.Register_LE(
    (obj, 'b', 112), size=1, bitfield=dev_util.Bitfield_LE({
        'zeros': 0,
        'ones': 1,
        'many_ones' : (5, 2)}))

with LogCapture() as capture, AllowLogs():
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
    stest.expect_equal(len(capture.messages), 5)
    del capture.messages[:]

fields7 = dev_util.Register_LE(
    (obj, 'b', 116), size=1, bitfield=dev_util.Bitfield_LE({
        'ignore': 0}))

# No logs from ignore
with LogCapture() as capture, AllowLogs():
    # Writing values does not matter
    set_log_level(3)
    ignore.write(2)
    fields7.write(fields7.bitfield.value(ignore = 1))
    stest.expect_equal(ignore.read(), 0)
    stest.expect_equal(obj.b_fields7, 0)
    fields7.write(fields7.bitfield.value(ignore = 1))
    stest.expect_equal(fields7.read(), 0)
    stest.expect_equal(capture.messages, [])


fields8 = dev_util.Register_LE(
    (obj, 'b', 120), size=1, bitfield=dev_util.Bitfield_LE({
        'undocumented': 0}))

with LogCapture() as capture, AllowLogs():
    # even reads give messages on log-level 1 first time
    set_log_level(1)
    undocumented0.read()
    fields8.read()
    stest.expect_equal(len(capture.messages), 2)
    del capture.messages[:]
    # but similar operation does not show on log-level 1
    undocumented0.read()
    fields8.read()
    stest.expect_equal(capture.messages, [])
    # and then on log-level 2 they appear again
    set_log_level(2)
    undocumented0.read()
    undocumented1.read()
    fields8.read()
    stest.expect_equal(len(capture.messages), 3)
    del capture.messages[:]
    set_log_level(1)
    # check similar for writes, even when writing similar
    undocumented0.write(0)
    fields8.write(0)
    stest.expect_equal(len(capture.messages), 2)
    del capture.messages[:]
    # check that we get no additional messages on log-level 1
    undocumented0.write(1)
    fields8.write(fields8.bitfield.value(undocumented = 1))
    stest.expect_equal(capture.messages, [])
    # check that write still goes through
    stest.expect_equal(obj.b_undocumented0, 1)
    stest.expect_equal(obj.b_fields8, fields8.bitfield.value(undocumented = 1))
    set_log_level(2)
    # and then on log-level 2 messages appear again
    undocumented0.write(0)
    undocumented1.write(2)
    fields8.write(fields8.bitfield.value(undocumented = 0))
    stest.expect_equal(len(capture.messages), 3)
