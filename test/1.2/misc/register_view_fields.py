# © 2021-2024 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

from simics import *
from stest import expect_equal

def test(obj):
    b = SIM_get_port_interface(obj, 'register_view', 'b')

    expect_equal(b.register_info(0)[4], [['all', '', 0, 31]])
    expect_equal(b.register_info(1)[4], [['g', '7', 7, 7],
                                         ['d[1]', '4', 5, 5],
                                         ['d[0]', '4', 4, 4],
                                         ['c', '3', 3, 3],
                                         ['ab', '12', 0, 2]])
    expect_equal(b.register_info(2)[4], [])

test(obj)
