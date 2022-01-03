# © 2021-2022 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

from stest import expect_false

b = SIM_get_port_interface(obj, 'register_view', 'b')
expect_false(b.big_endian_bitorder())
