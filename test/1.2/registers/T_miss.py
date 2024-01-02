# © 2021-2024 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

# Test how side-effects are carried out in failing accesses.

# The semantics are not well documented; the main motivation of the
# test is to catch involuntary semantic changes.

# The way it currently works, the register dispatcher performs the
# memop sequentially one register at a time, starting from the lowest
# address. The operation is aborted on the first error, which
# potentially leaves some remaining register accesses undone. There
# are four sources of failures:
# - The address might be outside all registers
# - It might be overlapping in a bank with overlapping=false
# - It might be a partial register access in a bank with partial=false
# - The read_access or write_access method might throw an exception.

import stest
import dev_util

def read(bank, offs, len):
    return dev_util.Register_LE(bank, offs, size=len).read()
def write(bank, offs, len, val):
    return dev_util.Register_LE(bank, offs, size=len).write(val)

def test_access_outside_reg():
    # Test both read and write side-effects for accesses outside registers
    obj.b_a = 0
    obj.missed = False
    read(obj.bank.b, 8, 4)
    stest.expect_equal((obj.b_a, obj.missed), (0, True))
    obj.missed = False
    read(obj.bank.b, 10, 4)
    stest.expect_equal((obj.b_a, obj.missed), (0x1234, True))
    obj.missed = False
    obj.b_a = 0
    write(obj.bank.b, 8, 4, 0xffffffff)
    stest.expect_equal((obj.b_a, obj.missed), (0, True))
    obj.missed = False
    write(obj.bank.b, 10, 4, 0xffffffff)
    stest.expect_equal((obj.b_a, obj.missed), (0xffff, True))
    obj.missed = False

test_access_outside_reg()

def test_exception():
    obj.missed = False
    obj.b_b = [0, 0, 0, 0]
    # b[2] throws an exception in write(). For some reason it is not
    # caught, so miss_access() is never invoked.
    with stest.expect_exception_mgr(dev_util.MemoryError):
        write(obj.bank.b, 20, 4, 0x11111111)
    stest.expect_equal((obj.missed, obj.b_b), (False, [0x11, 0x11, 0, 0]))

test_exception()

# Test violations to 'partial=false'. When the lower address of an
# access doesn't align with a register boundary, the entire access is
# discarded and a miss signalled, just as one would expect. If the
# upper address doesn't align with a register boundary, then only the
# part of the access that overlaps with the misaligned register is
# discarded.
def test_partial_violations():
    obj.nopart_a = 0
    obj.nopart_b = 0
    obj.nopart_r = [0, 0, 0, 0]
    obj.missed = False

    write(obj.bank.nopart, 10, 8, 0)
    assert not obj.missed
    write(obj.bank.nopart, 11, 4, 0x33444455)
    stest.expect_true(obj.missed)
    stest.expect_equal(obj.nopart_r, [0, 0, 0, 0])

    # side-effects happen before detecting violations on the end of the access!
    obj.missed = False
    write(obj.bank.nopart, 10, 1, 0x11)
    # not anymore after bug 20695
    #stest.expect_equal((obj.missed, obj.nopart_r), (True, [0x11, 0, 0, 0]))
    stest.expect_equal((obj.missed, obj.nopart_r), (True, [0, 0, 0, 0]))

    obj.nopart_r = [0, 0, 0, 0]
    obj.missed = False
    write(obj.bank.nopart, 11, 1, 0x11)
    stest.expect_true(obj.missed)
    stest.expect_equal((obj.missed, obj.nopart_r), (True, [0, 0, 0, 0]))

    # another fairly realistic case
    obj.missed = False
    write(obj.bank.nopart, 20, 4, 0xaabbccdd)
    # register 'b' no longer being affected after bug 20695
    #stest.expect_equal((obj.missed, obj.nopart_a, obj.nopart_b),
    #                   (True, 0xccdd, 0xaabb))
    stest.expect_equal((obj.missed, obj.nopart_a, obj.nopart_b),
                       (True, 0xccdd, 0))

    # Read accesses follow the same pattern as writes
    obj.missed = False
    obj.nopart_r = [0, 0, 0, 0]
    read(obj.bank.nopart, 10, 1)
    # r[0] not being accessed anymore, again, after bug 20695
    #stest.expect_equal((obj.missed, obj.nopart_r),
    #                   (True, [0x1111, 0, 0, 0]))
    stest.expect_equal((obj.missed, obj.nopart_r),
                       (True, [0, 0, 0, 0]))

    obj.missed = False
    obj.nopart_r = [0, 0, 0, 0]
    read(obj.bank.nopart, 11, 7)
    stest.expect_equal((obj.missed, obj.nopart_r),
                       (True, [0, 0, 0, 0]))
    obj.missed = False
    obj.nopart_r = [0, 0, 0, 0]
    read(obj.bank.nopart, 10, 7)
    # behavior changed after bug 20695
    #stest.expect_equal((obj.missed, obj.nopart_r),
    #                   (True, [0x1111, 0x1111, 0x1111, 0x1111]))
    stest.expect_equal((obj.missed, obj.nopart_r),
                       (True, [0x1111, 0x1111, 0x1111, 0]))

test_partial_violations()

def test_overlapping_violations():
    # Somewhat unexpectedly, overlapping accesses do have side-effects
    # in the first of the overlapped registers.
    obj.missed = False
    obj.noover_r = [0, 0]
    write(obj.bank.noover, 10, 4, 0x11112222)
    stest.expect_equal((obj.missed, obj.noover_r), (True, [0x2222, 0]))
    obj.missed = False
    obj.noover_r = [0x3333, 0x3333]
    write(obj.bank.noover, 11, 3, 0x111122)
    stest.expect_equal((obj.missed, obj.noover_r), (True, [0x2233, 0x3333]))

test_overlapping_violations()
