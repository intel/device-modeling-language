# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

from simics import *
from stest import expect_equal

def expect_same_content(list1, list2):
    hashable_list1 = (tuple(e) if hasattr(e, '__iter__') else e
                      for e in list1)
    hashable_list2 = (tuple(e) if hasattr(e, '__iter__') else e
                      for e in list2)
    expect_equal(set(hashable_list1), set(hashable_list2))

def test(obj):
    b = SIM_get_port_interface(obj, 'register_view', 'b')

    expect_same_content(b.register_info(0)[4], [['all', '', 0, 31]])
    expect_same_content(b.register_info(1)[4], [['g', '7', 7, 7],
                                          ['d[1]', '4', 5, 5],
                                          ['d[0]', '4', 4, 4],
                                          ['c', '3', 3, 3],
                                          ['ab', '12', 0, 2]])
    expect_same_content(b.register_info(2)[4], [])
    expect_same_content(b.register_info(3)[4],
                        [['a', '', 31, 31]] +
                        [['b.b[%d][%d]' % (i, j), '',

                          30 - i * 2 - j, 30 - i * 2 - j]
                         for i in range(2) for j in range(2)] +
                        [['c[%d][%d].c[%d][%d]' % (i, j, k, l), '',
                          22 - i * 8 - j * 4 - k * 2 - l,
                          22 - i * 8 - j * 4 - k * 2 - l]
                         for i in range(2) for j in range(2)
                         for k in range(2) for l in range(2)])

test(obj)
