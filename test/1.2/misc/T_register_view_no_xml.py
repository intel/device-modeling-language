# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest

from simics import SIM_get_port_interface

b = SIM_get_port_interface(obj, 'register_view', 'b')
stest.expect_equal(b.description(), "")
stest.expect_equal(b.big_endian_bitorder(), False)
stest.expect_equal(b.number_of_registers(), 0)
stest.expect_equal(b.register_info(0), None)

b.set_register_value(0, 4711)
stest.expect_equal(b.get_register_value(0), 0)
