# © 2021-2022 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import contextlib
import stest, dev_util
from stest import expect_equal

mkR = dev_util.Register_LE

def test_overlap():
    def mkRegs(b):
        return (
            (mkR((obj, b, 0x00), size = 2),     0xFFFF),
            (mkR((obj, b, 0x00), size = 4), 0x0203FFFF),
            (mkR((obj, b, 0x02), size = 4), 0xFFFF0203),
            (mkR((obj, b, 0x04), size = 4), 0x0607FFFF),
            (mkR((obj, b, 0x06), size = 4), 0x08090607),
            (mkR((obj, b, 0x08), size = 4), 0xFFFF0809),
            (mkR((obj, b, 0x0a), size = 4), 0x0c0dFFFF),
            (mkR((obj, b, 0x00), size = 8), 0x0607FFFF0203FFFF),
            (mkR((obj, b, 0x08), size = 8), 0xFFFF0c0dFFFF0809),
            (mkR((obj, b, 0x10), size = 2),     0xFFFF),
            (mkR((obj, b, 0x10), size = 4), 0x1213FFFF),
            (mkR((obj, b, 0x12), size = 4), 0xFFFF1213),
            (mkR((obj, b, 0x14), size = 4), 0x1617FFFF),
            (mkR((obj, b, 0x16), size = 4), 0x18191617),
            (mkR((obj, b, 0x18), size = 4), 0xFFFF1819),
            (mkR((obj, b, 0x1a), size = 4), 0x1c1dFFFF),
            (mkR((obj, b, 0x10), size = 8), 0x1617FFFF1213FFFF),
            (mkR((obj, b, 0x18), size = 8), 0xFFFF1c1dFFFF1819),

            (mkR((obj, b, 0x20), size = 4), 0xFF2122FF),
            (mkR((obj, b, 0x21), size = 4), 0xFFFF2122),
            (mkR((obj, b, 0x20), size = 8), 0xFF2526FFFF2122FF),
            (mkR((obj, b, 0x30), size = 4), 0xFF3132FF),
            (mkR((obj, b, 0x31), size = 4), 0xFFFF3132),
            (mkR((obj, b, 0x30), size = 8), 0xFF3536FFFF3132FF),
            )

    for b in ('ob', 'pb'):
        for r, val in mkRegs(b):
            expect_equal(r.read(), val)

def expect_error():
    return contextlib.nested(
        stest.expect_log_mgr(conf.obj, 'spec-viol'),
        stest.expect_exception_mgr(dev_util.MemoryError))

def test_partial():
    # TODO: this is copied from 1.2/registers/T_miss_pattern. The
    # logic is simpler in 1.4, and much is covered elsewhere, so we
    # could probably remove most test cases.

    r1 = mkR((obj, 'ob', 0x01), size = 2)
    r2 = mkR((obj, 'ob', 0x02), size = 2)
    r3 = mkR((obj, 'ob', 0x03), size = 2)
    ru = mkR((obj, 'ob', 0x21), size = 2)
    r4 = mkR((obj, 'ob', 0x20), size = 2)
    r5 = mkR((obj, 'ob', 0x22), size = 2)
    expect_equal(r1.read(), 0xffff)
    expect_equal(r3.read(), 0xffff)
    expect_equal(r4.read(), 0xffff)
    expect_equal(r5.read(), 0xffff)
    r1.write(0xabcd)
    expect_equal(r2.read(), 0x0203)
    r3.write(0xabcd)
    expect_equal(r2.read(), 0x0203)
    r4.write(0xabcd)
    expect_equal(ru.read(), 0x2122)
    r5.write(0xabcd)
    expect_equal(ru.read(), 0x2122)

    r1 = mkR((obj, 'ob', 0x11), size = 2)
    r2 = mkR((obj, 'ob', 0x12), size = 2)
    r3 = mkR((obj, 'ob', 0x13), size = 2)
    ru = mkR((obj, 'ob', 0x31), size = 2)
    r4 = mkR((obj, 'ob', 0x30), size = 2)
    r5 = mkR((obj, 'ob', 0x32), size = 2)
    expect_equal(r1.read(), 0xffff)
    expect_equal(r3.read(), 0xffff)
    r1.write(0xabcd)
    expect_equal(r2.read(), 0x1213)
    r3.write(0xabcd)
    expect_equal(r2.read(), 0x1213)
    r4.write(0xabcd)
    expect_equal(ru.read(), 0x3132)
    r5.write(0xabcd)
    expect_equal(ru.read(), 0x3132)

    r1 = mkR((obj, 'pb', 0x01), size = 2)
    r2 = mkR((obj, 'pb', 0x02), size = 2)
    r3 = mkR((obj, 'pb', 0x03), size = 2)
    ru = mkR((obj, 'pb', 0x21), size = 2)
    r4 = mkR((obj, 'pb', 0x20), size = 2)
    r5 = mkR((obj, 'pb', 0x22), size = 2)
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

    r1 = mkR((obj, 'pb', 0x11), size = 2)
    r2 = mkR((obj, 'pb', 0x12), size = 2)
    r3 = mkR((obj, 'pb', 0x13), size = 2)
    ru = mkR((obj, 'pb', 0x31), size = 2)
    r4 = mkR((obj, 'pb', 0x30), size = 2)
    r5 = mkR((obj, 'pb', 0x32), size = 2)
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
