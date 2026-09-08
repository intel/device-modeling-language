# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import simics
from stest import expect_false
import testenv
obj = testenv.instantiate()

b = simics.SIM_get_port_interface(obj, 'register_view', 'b')
expect_false(b.big_endian_bitorder())
