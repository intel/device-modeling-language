# © 2021-2024 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import contextlib
from simics import *
import stest, dev_util

def byte_at(offs, big_endian_regsize):
    if big_endian_regsize:
        i = offs % big_endian_regsize
        return ((offs - i) + ((big_endian_regsize - 1) - i)) % 256
    return offs % 256

def expected_data(offset, length, big_endian):
    if big_endian:
        if 0x1000000 <= offset:
            big_endian_regsize = offset // 0x1000000
        else:
            big_endian_regsize = 4
    else:
        big_endian_regsize = None
    return sum(
        byte_at(offset + i, big_endian_regsize) << (i * 8)
        for i in range(length))

def expect_miss(obj):
    ctx = contextlib.ExitStack()
    ctx.enter_context(stest.allow_log_mgr(obj, 'spec-viol'))
    ctx.enter_context(stest.expect_exception_mgr(dev_util.MemoryError))
    return ctx

def write(bank, offset, length, partial = False, overlapping = False,
          illegal = False, little_endian = False):
    print("Writing %d bytes to offset %#x" % (length, offset))
    reg = dev_util.Register_LE(bank, offset, size=length)
    data = expected_data(offset, length, not little_endian)
    print("DATA %x" % (data,))
    illegal = (illegal
               or (partial and 'nonpar' in SIM_object_name(bank))
               or (overlapping and 'nonover' in SIM_object_name(bank)))
    if illegal:
        with expect_miss(bank):
            reg.write(data)
    else:
        reg.write(data)

def read(bank, offset, length,
         partial = False, overlapping = False, illegal = False,
         little_endian = False):
    print("Reading %d bytes from offset %#x" % (length, offset))
    reg = dev_util.Register_LE(bank, offset, size=length)
    illegal = (illegal
               or (partial and 'nonpar' in SIM_object_name(bank))
               or (overlapping and 'nonover' in SIM_object_name(bank)))
    if illegal:
        with expect_miss(bank):
            reg.read()
    else:
        expect = expected_data(offset, length, not little_endian)
        data = reg.read()
        stest.expect_equal(data, expect)

def test_some(bank, little_endian):
    bank.log_level = 4

    def r(offs, size, force_little_endian=False, **args):
        read(bank, offs, size,
             little_endian=force_little_endian or little_endian,
             **args)
    def w(offs, size, force_little_endian=False, **args):
        write(bank, offs, size,
              little_endian=force_little_endian or little_endian, **args)
    # Fill the memory with some kind of pattern.
    w(0, 4)
    w(4, 4)
    w(8, 4)

    # Read the registers
    r(0, 4)
    r(4, 4)
    r(8, 4)

    print("Now read some partial ranges")
    for o in range(8):
        r(o, 1, partial = True)
    for o in range(3):
        r(o, 2, partial = True)
    for o in range(4,7):
        r(o, 2, partial = True)

    print("Now read/write some overlapping ranges")
    r(0, 8, overlapping = True)
    r(4, 8, overlapping = True)
    w(0, 8, overlapping = True)
    w(4, 8, overlapping = True)

    print("Now read/write some partial and overlapping ranges")
    r(2, 4, partial = True, overlapping = True)
    r(6, 4, partial = True, overlapping = True)
    w(2, 4, partial = True, overlapping = True)
    w(6, 4, partial = True, overlapping = True)

    print("Reading registers of various sizes")
    for sz in range(8):
        offset = sz * 0x1000000
        # read the upper bytes of the register
        for i in range(sz):
            w(offset + i, sz - i, partial = (i > 0))
            r(offset + i, sz - i, partial = (i > 0))

    print("Accessing arrays")
    # The dense array
    r(0x100, 4)
    w(0x100, 4)
    r(0x100, 8, overlapping = True)
    w(0x100, 8, overlapping = True)
    r(0x102, 8, partial = True, overlapping = True)
    w(0x102, 8, partial = True, overlapping = True)

    # The sparse array
    r(0x200, 4)
    w(0x200, 4)
    r(0x200, 8, illegal = True)
    w(0x200, 8, illegal = True)
    r(0x202, 8, illegal = True)
    w(0x202, 8, illegal = True)

    # The interleaved arrays
    r(0x300, 4)
    w(0x300, 4)
    r(0x300, 8, overlapping = True)
    w(0x300, 8, overlapping = True)
    r(0x302, 8, partial = True, overlapping = True)
    w(0x302, 8, partial = True, overlapping = True)

    # The unaligned array
    r(0x400, 4, illegal = True)
    r(0x404, 4, illegal = True)
    r(0x408, 4)
    r(0x418, 4, illegal = True)
    r(0x41c, 4, illegal = True)

    # interleaved with different strides. Since the size of the
    # registers is 1, the registers behave like a single little-endian
    # register.
    r(0x500, 8, overlapping = True, force_little_endian = True)
    w(0x500, 8, overlapping = True, force_little_endian = True)
    r(0x501, 8, overlapping = True, force_little_endian = True)
    w(0x501, 8, overlapping = True, force_little_endian = True)

test_some(obj.bank.nonpar_nonover_be, False)
exit(0)

for p in ['nonpar', 'par']:
    for o in ['nonover', 'over']:
        for e in ['le', 'be']:
            test_some(SIM_object_descendant(obj.bank, '_'.join([p, o, e])),
                      e == 'le')
