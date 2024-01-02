# © 2021-2024 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import contextlib
import stest, dev_util
from stest import expect_equal

mkR = dev_util.Register_LE

def test_overlap():
    def mkRegs(bank):
        return (
            (mkR(bank, 0x00, size=2),     0xFFFF),
            (mkR(bank, 0x00, size=4), 0x0203FFFF),
            (mkR(bank, 0x02, size=4), 0xFFFF0203),
            (mkR(bank, 0x04, size=4), 0x0607FFFF),
            (mkR(bank, 0x06, size=4), 0x08090607),
            (mkR(bank, 0x08, size=4), 0xFFFF0809),
            (mkR(bank, 0x0a, size=4), 0x0c0dFFFF),
            (mkR(bank, 0x00, size=8), 0x0607FFFF0203FFFF),
            (mkR(bank, 0x08, size=8), 0xFFFF0c0dFFFF0809),
            (mkR(bank, 0x10, size=2),     0xFFFF),
            (mkR(bank, 0x10, size=4), 0x1213FFFF),
            (mkR(bank, 0x12, size=4), 0xFFFF1213),
            (mkR(bank, 0x14, size=4), 0x1617FFFF),
            (mkR(bank, 0x16, size=4), 0x18191617),
            (mkR(bank, 0x18, size=4), 0xFFFF1819),
            (mkR(bank, 0x1a, size=4), 0x1c1dFFFF),
            (mkR(bank, 0x10, size=8), 0x1617FFFF1213FFFF),
            (mkR(bank, 0x18, size=8), 0xFFFF1c1dFFFF1819),

            (mkR(bank, 0x20, size=4), 0xFF2122FF),
            (mkR(bank, 0x21, size=4), 0xFFFF2122),
            (mkR(bank, 0x20, size=8), 0xFF2526FFFF2122FF),
            (mkR(bank, 0x30, size=4), 0xFF3132FF),
            (mkR(bank, 0x31, size=4), 0xFFFF3132),
            (mkR(bank, 0x30, size=8), 0xFF3536FFFF3132FF),
            )

    for b in (obj.bank.ob, obj.bank.pb):
        for (r, val) in mkRegs(b):
            expect_equal(r.read(), val)

def expect_error(obj):
    ctx = contextlib.ExitStack()
    ctx.enter_context(stest.expect_log_mgr(obj, 'spec-viol'))
    ctx.enter_context(stest.expect_exception_mgr(dev_util.MemoryError))
    return ctx

def test_partial():

    r1 = mkR(obj.bank.ob, 0x01, size=2)
    r2 = mkR(obj.bank.ob, 0x02, size=2)
    r3 = mkR(obj.bank.ob, 0x03, size=2)
    ru = mkR(obj.bank.ob, 0x21, size=2)
    r4 = mkR(obj.bank.ob, 0x20, size=2)
    r5 = mkR(obj.bank.ob, 0x22, size=2)
    with expect_error(obj.bank.ob):
        r1.read()
    with expect_error(obj.bank.ob):
        r3.read()
    with expect_error(obj.bank.ob):
        r4.read()
    with expect_error(obj.bank.ob):
        r5.read()
    with expect_error(obj.bank.ob):
        r1.write(0xabcd)
    expect_equal(r2.read(), 0x0203)
    with expect_error(obj.bank.ob):
        r3.write(0xabcd)
    expect_equal(r2.read(), 0x0203)
    with expect_error(obj.bank.ob):
        r4.write(0xabcd)
    expect_equal(ru.read(), 0x2122)
    with expect_error(obj.bank.ob):
        r5.write(0xabcd)
    expect_equal(ru.read(), 0x2122)

    r1 = mkR(obj.bank.ob, 0x11, size=2)
    r2 = mkR(obj.bank.ob, 0x12, size=2)
    r3 = mkR(obj.bank.ob, 0x13, size=2)
    ru = mkR(obj.bank.ob, 0x31, size=2)
    r4 = mkR(obj.bank.ob, 0x30, size=2)
    r5 = mkR(obj.bank.ob, 0x32, size=2)
    with expect_error(obj.bank.ob):
        r1.read()
    with expect_error(obj.bank.ob):
        r3.read()
    with expect_error(obj.bank.ob):
        r1.write(0xabcd)
    expect_equal(r2.read(), 0x1213)
    with expect_error(obj.bank.ob):
        r3.write(0xabcd)
    expect_equal(r2.read(), 0x1213)
    with expect_error(obj.bank.ob):
        r4.write(0xabcd)
    expect_equal(ru.read(), 0x3132)
    with expect_error(obj.bank.ob):
        r5.write(0xabcd)
    expect_equal(ru.read(), 0x3132)

    r1 = mkR(obj.bank.pb, 0x01, size=2)
    r2 = mkR(obj.bank.pb, 0x02, size=2)
    r3 = mkR(obj.bank.pb, 0x03, size=2)
    ru = mkR(obj.bank.pb, 0x21, size=2)
    r4 = mkR(obj.bank.pb, 0x20, size=2)
    r5 = mkR(obj.bank.pb, 0x22, size=2)
    expect_equal(r1.read(), 0x03FF)
    expect_equal(r3.read(), 0xFF02)
    r1.write(0xabcd)
    expect_equal(r2.read(), 0x02ab)
    r3.write(0xabcd)
    expect_equal(r2.read(), 0xcdab)
    r4.write(0xabcd)
    expect_equal(ru.read(), 0x21ab)
    r5.write(0xabcd)
    expect_equal(ru.read(), 0xcdab)

    r1 = mkR(obj.bank.pb, 0x11, size=2)
    r2 = mkR(obj.bank.pb, 0x12, size=2)
    r3 = mkR(obj.bank.pb, 0x13, size=2)
    ru = mkR(obj.bank.pb, 0x31, size=2)
    r4 = mkR(obj.bank.pb, 0x30, size=2)
    r5 = mkR(obj.bank.pb, 0x32, size=2)
    expect_equal(r1.read(), 0x13FF)
    expect_equal(r3.read(), 0xFF12)
    r1.write(0xabcd)
    expect_equal(r2.read(), 0x12ab)
    r3.write(0xabcd)
    expect_equal(r2.read(), 0xcdab)
    r4.write(0xabcd)
    expect_equal(ru.read(), 0x31ab)
    r5.write(0xabcd)
    expect_equal(ru.read(), 0xcdab)

#conf.obj.log_level = 4

test_overlap()
test_partial()

# Current behaviour for non-overlapping: unmapped_*_access called once
# for entire access
f5 = dev_util.Register_LE(obj.bank.f, 5, size=4)
expect_equal(f5.read(), 0x12345678)
f5.write(0x87654321)
expect_equal(obj.f_size, 4)
expect_equal(obj.f_offset, 5)
expect_equal(obj.f_writevalue, 0x87654321)
# violations of partial and overlapping are currently not forgiven.
f2 = dev_util.Register_LE(obj.bank.f, 2, size=4)
with expect_error(obj.bank.f):
    f2.read()
f3 = dev_util.Register_LE(obj.bank.f, 3, size=1)
with expect_error(obj.bank.f):
    f3.read()
# Current behaviour for non-overlapping: unmapped_*_access called once per byte
t8 = dev_util.Register_LE(obj.bank.t, 8, size=8)
expect_equal(t8.read(), 0x0f000d0c0b0a09cc)
t8.write(0x1122334455667788)
expect_equal(obj.t_count, 6)
expect_equal(obj.t_writevalue, [0x00, 0x77, 0x66, 0x55, 0x44, 0x33, 0x00, 0x11])

tb8 = dev_util.Register_BE(obj.bank.tb, 8, size=8)
tb8.write(0x1122334455667788)
expect_equal(obj.t_count, 6)
expect_equal(obj.tb_writevalue,
             [0x00, 0x22, 0x33, 0x44, 0x55, 0x66, 0x00, 0x88])
