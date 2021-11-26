# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import simics
import stest

def test(obj):
    register_view = simics.SIM_get_port_interface(
        obj, 'register_view', 'inquiry')

    register_view.set_register_value(0, 4711)
    stest.expect_false(simics.SIM_get_attribute(obj, "write_raised"))

    stest.expect_equal(register_view.get_register_value(0), 4711)
    stest.expect_false(simics.SIM_get_attribute(obj, "read_raised"))

