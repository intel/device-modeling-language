# © 2021-2022 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import dev_util

from simics import *
from stest import expect_equal, expect_true

def test_description(b, expected_desc):
    expect_equal(b.description(), expected_desc)

def test_bitorder(b, expected_desc):
    expect_equal(b.big_endian_bitorder(), expected_desc)

def test_number_of_registers(b):
    expect_equal(b.number_of_registers(), 6)

def test_register_info(b):
    # In 1.4, we are no longer guaranteed offset order for register for whatever
    # reason so we work around that by checking the results of reg info
    # out-of-order
    expected_regs = set([('R1', 'R1', 4, 0),
                          ('R2', 'R2', 4, 4),
                          ('R3[0][0]', 'R3', 4, 8),
                          ('R3[0][1]', 'R3', 4, 12),
                          ('R3[1][0]', 'R3', 4, 16),
                          ('R3[1][1]', 'R3', 4, 20),
                          None # Intentionally check one-past the last reg
    ])
    b_regs = {tuple(r[:4]) if r else r for r in
              [b.register_info(i)
               for i in range(b.number_of_registers() + 1)]}
    expect_equal(b_regs, expected_regs)

def test_register_value(bank, register):
    assert bank.number_of_registers() == 1

    register.write(0x0a0b0c0d)
    expect_equal(bank.get_register_value(0), 0x0a0b0c0d)
    bank.set_register_value(0, 0x01020304)
    expect_equal(register.read(), 0x01020304)
    expect_equal(bank.get_register_value(0), 0x01020304)

def test_array_names(bg):
    expect_equal(
        sorted([bg.register_info(i)[0] for i in range(0, bg.number_of_registers())]),
        ['g[0].h.i[0].r',
         'g[0].h.i[0].rma[0][0]',
         'g[0].h.i[0].rma[0][1]',
         'g[0].h.i[0].rma[1][0]',
         'g[0].h.i[0].rma[1][1]',
         'g[0].h.i[0].ru[0]',
         'g[0].h.i[0].ru[2]',
         'g[0].h.i[1].r',
         'g[0].h.i[1].rma[0][0]',
         'g[0].h.i[1].rma[0][1]',
         'g[0].h.i[1].rma[1][0]',
         'g[0].h.i[1].rma[1][1]',
         'g[0].h.i[1].ru[0]',
         'g[0].h.i[1].ru[2]',
         'g[1].h.i[0].r',
         'g[1].h.i[0].rma[0][0]',
         'g[1].h.i[0].rma[0][1]',
         'g[1].h.i[0].rma[1][0]',
         'g[1].h.i[0].rma[1][1]',
         'g[1].h.i[0].ru[0]',
         'g[1].h.i[0].ru[2]',
         'g[1].h.i[1].r',
         'g[1].h.i[1].rma[0][0]',
         'g[1].h.i[1].rma[0][1]',
         'g[1].h.i[1].rma[1][0]',
         'g[1].h.i[1].rma[1][1]',
         'g[1].h.i[1].ru[0]',
         'g[1].h.i[1].ru[2]'])

def test(obj):
    test_description(obj.bank.b.iface.register_view, "Description for b")

    test_description(obj.bank.ba[0].iface.register_view, "")
    test_description(obj.bank.u.iface.register_view, "")

    test_description(obj.bank.baa[0][1].iface.register_view, "Description for baa")
    test_bitorder(obj.bank.baa[1][0].iface.register_view, False)

    test_number_of_registers(obj.bank.b.iface.register_view)
    test_number_of_registers(obj.bank.ba[0].iface.register_view)
    test_number_of_registers(obj.bank.u.iface.register_view)

    test_register_info(obj.bank.b.iface.register_view)
    test_register_info(obj.bank.ba[0].iface.register_view)
    test_register_info(obj.bank.baa[1][1].iface.register_view)
    test_register_info(obj.bank.u.iface.register_view)

    test_array_names(obj.bank.bg.iface.register_view)

    test_register_value(obj.bank.le.iface.register_view,
                        dev_util.Register_LE((obj, 'le', 0x0), 4))
    test_register_value(obj.bank.be.iface.register_view,
                        dev_util.Register_BE((obj, 'be', 0x0), 4))
    expect_equal(obj.bank.le.iface.register_view.register_info(0)[5], False)
    expect_equal(obj.bank.be.iface.register_view.register_info(0)[5], True)
