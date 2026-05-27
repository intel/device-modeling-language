# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import simics
import stest
import dev_util as du
from functools import reduce

simics.SIM_run_command("log-level 4")

def reset_dev(d, r_val):
    d.b_r = r_val
    d.before_read_called = False
    d.after_read_called = False
    d.before_set_called = False
    d.after_set_called = False
    d.write_val = 0
    d.before_write_called = False
    d.after_write_called = False

def expected_int(bytes, offs):
    """Return the integer expected from writing the bytes at offs"""
    bytes = list(bytes)
    bytes.reverse()
    return reduce(lambda val, x: val << 8 | x, bytes, 0) << (offs * 8)

for offs in range(8):
    print("offset:", offs)
    for size in range(1, 8 - offs + 1):
        r = du.Register_LE(obj.bank.b, offs, size=size)
        r_inq = du.Register_LE(obj.bank.b, offs, size=size)
        r_inq.read_transaction.inquiry = True
        r_inq.write_transaction.inquiry = True
        print("  size:", size)
        val = 0x8877665544332211 & ((1 << size * 8) - 1)

        reset_dev(obj, 0)
        r.write(val)
        stest.expect_equal(obj.write_val, val << offs * 8)
        stest.expect_equal(obj.before_write_called, True)
        stest.expect_equal(obj.after_write_called, True)
        stest.expect_equal(obj.before_read_called, False)
        stest.expect_equal(obj.after_read_called, False)
        stest.expect_equal(obj.before_set_called, False)
        stest.expect_equal(obj.after_set_called, False)

        reset_dev(obj, 0)
        r_inq.write(val)
        stest.expect_equal(obj.b_r, val << offs * 8)
        stest.expect_equal(obj.write_val, 0)
        stest.expect_equal(obj.before_write_called, False)
        stest.expect_equal(obj.after_write_called, False)
        stest.expect_equal(obj.before_read_called, False)
        stest.expect_equal(obj.after_read_called, False)
        stest.expect_equal(obj.before_set_called, True)
        stest.expect_equal(obj.after_set_called, True)

        READ_VALUE = (0xdeadbeefbaadc0de >> offs * 8) & ((1 << size * 8) - 1)
        reset_dev(obj, val)
        ret = r.read()
        stest.expect_equal(ret, READ_VALUE)
        stest.expect_equal(obj.b_r, val)
        stest.expect_equal(obj.write_val, 0)
        stest.expect_equal(obj.before_write_called, False)
        stest.expect_equal(obj.after_write_called, False)
        stest.expect_equal(obj.before_read_called, True)
        stest.expect_equal(obj.after_read_called, True)
        stest.expect_equal(obj.before_set_called, False)
        stest.expect_equal(obj.after_set_called, False)

        reset_dev(obj, val << offs * 8)
        ret = r_inq.read()
        stest.expect_equal(ret, val)
        stest.expect_equal(obj.b_r, val << offs * 8)
        stest.expect_equal(obj.write_val, 0)
        stest.expect_equal(obj.before_write_called, False)
        stest.expect_equal(obj.after_write_called, False)
        stest.expect_equal(obj.before_read_called, False)
        stest.expect_equal(obj.after_read_called, False)
        stest.expect_equal(obj.before_set_called, False)
        stest.expect_equal(obj.after_set_called, False)
