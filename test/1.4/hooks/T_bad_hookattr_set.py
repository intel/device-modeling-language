# © 2023 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import simics;
import stest;

after_to_m_elem = ['after', ["('g[%u].m', (None, None))", [2], [2, True],
                             [['dev', []]]]]
after_to_n_elem = ['after', ["('n', (None,))", [], [4], [['dev', []]]]]

# has a bool arg instead of an int arg
bad_elem = ['after', ["('n', (None,))", [], [True], [['dev', []]]]]

stest.expect_equal(obj.h, [after_to_m_elem, after_to_n_elem])

with stest.expect_exception_mgr(simics.SimExc_Type):
    obj.h = [after_to_n_elem, bad_elem]

# Failed deserialization of hook queue doesn't change it at all
stest.expect_equal(obj.h, [after_to_m_elem, after_to_n_elem])
