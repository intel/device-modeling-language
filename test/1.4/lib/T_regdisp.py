# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import contextlib
from simics import *
import stest
import dev_util

def test_some(mem, obj, port, allow_partial, allow_overlapping,
              bigendian=False):
    def endian_convert(x, regsize):
        if bigendian:
            i = x % regsize
            x = (x - i) + ((regsize - 1) - i)
        return x

    def expected_data(offset, length):
        if 0x1000000 <= offset:
            regsize = offset // 0x1000000
            offset = offset % 256
        else:
            regsize = 4

        return tuple(endian_convert(x%256, regsize)
                     for x in range(offset, offset+length))

    def write(offset, length, partial, overlapping, illegal):
        data = expected_data(offset, length)
        print("Writing %d bytes to offset %#x: %r" % (length, offset, data))
        illegal = (illegal
                   or (partial and not allow_partial)
                   or (overlapping and not allow_overlapping))
        with contextlib.ExitStack() as ctx:
            if illegal:
                ctx.enter_context(stest.expect_exception_mgr(SimExc_Memory))
                ctx.enter_context(stest.expect_log_mgr(None, 'spec-viol'))
            exc = mem.iface.memory_space.write(None, offset, data, 0)
            if exc != Sim_PE_No_Exception:
                raise SimExc_Memory
            print("Wrote %r to %#x" % (data, offset))

    def read(offset, length, partial, overlapping, illegal):
        print("Reading %d bytes from offset %#x" % (length, offset))
        expect = expected_data(offset, length)
        illegal = (illegal
                   or (partial and not allow_partial)
                   or (overlapping and not allow_overlapping))
        with contextlib.ExitStack() as ctx:
            if illegal:
                ctx.enter_context(stest.expect_exception_mgr(SimExc_Memory))
                ctx.enter_context(stest.expect_log_mgr(None, 'spec-viol'))
            data = mem.iface.memory_space.read(None, offset, length, 0)
            print("Read %r from %#x" % (data, offset))
            stest.expect_equal(data, expect, "Wrong value read")
            return data

    def access(offset, length, partial = False, overlapping = False,
               illegal = False):
        obj.ports.HRESET.signal.signal_raise()
        write(offset, length, partial, overlapping, illegal)
        read(offset, length, partial, overlapping, illegal)

    obj.log_level = 4
    mem.map = [[0, [obj, port], 0, 0, 0x10000000, None, 0, 8192]]

    access(0, 4)
    access(4, 4)
    access(8, 4)

    print("Now read some partial ranges")
    for o in range(8):
        access(o, 1, partial = True)
    for o in range(3):
        access(o, 2, partial = True)
    for o in range(4,7):
        access(o, 2, partial = True)

    print("Now read/access some overlapping ranges")
    access(0, 8, overlapping = True)
    access(4, 8, overlapping = True)

    print("Now read/access some partial and overlapping ranges")
    access(2, 4, partial = True, overlapping = True)
    access(6, 4, partial = True, overlapping = True)

    print("Reading registers of various sizes")
    for sz in range(8):
        offset = sz * 0x1000000
        # read the upper bytes of the register
        for i in range(sz):
            access(offset + i, sz - i, partial = (i > 0))

    print("Accessing arrays")
    # The dense array
    access(0x100, 4)
    access(0x100, 8, overlapping = True)
    access(0x102, 8, partial = True, overlapping = True)
    access(0x110, 8, overlapping = True)
    access(0x112, 8, partial = True, overlapping = True)

    # The sparse array
    access(0x200, 4)
    access(0x200, 8, illegal = True)
    access(0x202, 8, illegal = True)

    # The interleaved arrays
    access(0x300, 4)
    access(0x300, 8, overlapping = True)
    access(0x302, 8, partial = True, overlapping = True)

    # The unaligned array
    access(0x400, 4, illegal = True)
    access(0x404, 4, illegal = True)
    access(0x408, 4)
    access(0x418, 4, illegal = True)
    access(0x41c, 4, illegal = True)

    # interleaved with different strides. Since the size of the
    # registers is 1, the registers behave like a single little-endian
    # register.
    bigendian = False
    access(0x500, 8, overlapping = True)
    access(0x501, 8, overlapping = True)

mem = SIM_create_object("memory-space", "mem", [])

test_some(mem, obj, 'par_over',
          allow_partial = True,
          allow_overlapping = True)

test_some(mem, obj, 'nonpar_over',
          allow_partial = False,
          allow_overlapping = True)

test_some(mem, obj, 'par_nonover',
          allow_partial = True,
          allow_overlapping = False)

test_some(mem, obj, 'nonpar_nonover',
          allow_partial = False,
          allow_overlapping = False)

test_some(mem, obj, 'par_over_be', allow_partial = True,
          allow_overlapping = True, bigendian = True)
