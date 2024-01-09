# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

from stest import expect_true

b = SIM_get_port_interface(obj, 'register_view', 'b')
expect_true(b.big_endian_bitorder())
