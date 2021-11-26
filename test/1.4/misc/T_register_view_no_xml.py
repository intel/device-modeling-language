# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest

from simics import SIM_get_port_interface

b = SIM_get_port_interface(obj, 'register_view', 'b')
stest.expect_equal(b.description(), "")

# relies on register-view python implementation
stest.expect_equal(b.big_endian_bitorder(), False)

stest.expect_equal(b.number_of_registers(), 1)
stest.expect_true(b.register_info(0) != None)

b.set_register_value(0, 4711)
stest.expect_equal(b.get_register_value(0), 4711)

# check that bank arrays work
reg_views = [pa.iface.register_view
             for paa in obj.bank.c for pa in paa]
for tested_regview in reg_views:
    stest.expect_equal(tested_regview.number_of_registers(), 2)
    tested_regview.set_register_value(0, 4711)
    stest.expect_equal(tested_regview.get_register_value(0), 4711)
    for other_regview in reg_views:
        if tested_regview != other_regview:
            stest.expect_equal(other_regview.get_register_value(0), 0)
    tested_regview.set_register_value(0, 0)

# check that unmappable bank works
u = obj.bank.u.iface.register_view
stest.expect_equal(u.number_of_registers(), 1)
u.set_register_value(0, 4711)
stest.expect_equal(u.get_register_value(0), 4711)
