# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

from simics import SIM_get_port_interface
from stest import expect_equal

b = SIM_get_port_interface(obj, 'register_view_read_only', 'b')
expect_equal(b.is_read_only(0), False)
expect_equal(b.is_read_only(1), True)
