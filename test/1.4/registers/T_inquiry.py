# © 2021-2024 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

from dev_util import Register_LE
from stest import expect_equal, expect_true, expect_false

SIM_run_command("log-level 4")

aaaa = 0xaaaaaaaaaaaaaaaa
bbbb = 0xbbbbbbbbbbbbbbbb

def test(offs, data, expected_r, expected_overflow=bbbb, get_called=True):
    obj.bank.b.r = aaaa
    obj.bank.b.overflow = bbbb
    r = Register_LE(obj.bank.b, offs, size=len(data))
    r.read_transaction.inquiry = True
    r.write_transaction.inquiry = True
    val = int.from_bytes(data, 'little')
    r.write(val)
    expect_true(obj.set_called)
    expect_equal(obj.get_called, get_called)
    obj.set_called = obj.get_called = False
    expect_equal(obj.bank.b.r, expected_r)
    expect_equal(obj.bank.b.overflow, expected_overflow)
    expect_equal(r.read(), val)
    expect_false(obj.set_called)
    expect_true(obj.get_called)
    obj.get_called = False

test(0, b'\x11',     0xaaaaaaaaaaaaaa11)
test(7, b'\x11',     0x11aaaaaaaaaaaaaa)
test(1, b'\x11\x22', 0xaaaaaaaaaa2211aa)
test(7, b'\x11\x22', 0x11aaaaaaaaaaaaaa, 0xbbbbbbbbbbbbbb22)
test(0, b'\x11\x22\x33\x44\x55\x66\x77\x88', 0x8877665544332211,
     get_called=False)
test(3, b'\x11\x22\x33\x44\x55\x66\x77\x88', 0x5544332211aaaaaa,
     0xbbbbbbbbbb887766)
