# © 2021-2023 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

from dev_util import Register_LE
from functools import reduce
from stest import expect_equal

SIM_run_command("log-level 4")

def reset_dev(d, r_val):
    d.b_r = r_val
    d.read_register_called = False
    d.write_register_called = False
    d.set_called = False
    d.write_val = 0

def expected_int(bytes, offs):
    """Return the integer expected from writing the bytes at offs"""
    bytes = list(bytes)
    bytes.reverse()
    return reduce(lambda val, x: val << 8 | x, bytes, 0) << (offs * 8)

def expect_attributes(write_val = 0, read_called = False, write_called = False,
                      set_called = False):
    expect_equal(obj.read_register_called, read_called)
    expect_equal(obj.write_register_called, write_called)
    expect_equal(obj.set_called, set_called)
    expect_equal(obj.write_val, write_val)

for offs in range(8):
    for size in range(1, 8 - offs + 1):
        r = Register_LE(obj.bank.b, offs, size=size)
        r.read_transaction.inquiry = True
        r.write_transaction.inquiry = True
        val = 0x8877665544332211 & ((1 << size * 8) - 1)

        reset_dev(obj, 0)
        r.write(val)
        expect_equal(obj.b_r, val << offs * 8)
        expect_attributes(set_called = True)

        reset_dev(obj, val << offs * 8)
        ret = r.read()
        expect_equal(ret, val)
        expect_equal(obj.b_r, val << offs * 8)
        expect_attributes()
