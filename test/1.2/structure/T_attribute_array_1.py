# © 2021-2024 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest
import simics

[footype] = [attr[4] for attr in VT_get_all_attributes("test")
             if attr[0] == 'foo']

stest.expect_equal(footype, "[[i{4}]{4}]")
stest.expect_equal(obj.foo, [[0, 1, 2, 3], [4, 5, 6, 7],
                             [8, 9, 10, 11], [12, 13, 14, 15]])

with stest.expect_exception_mgr(simics.SimExc_General):
    SIM_get_attribute(obj, 'bad')

with stest.expect_exception_mgr(simics.SimExc_General):
    obj.bad = [None] * 5
