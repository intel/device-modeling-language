# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest

# Needed so that we can check 'after'
cpu = SIM_create_object("clock", "clock", [["freq_mhz", 1]])
obj.queue = cpu

# check initial value through attribute
stest.expect_equal(obj.method_with_saved_with_init, 5)
stest.expect_equal([p.port_array_method_v for p in obj.port.p], [0, 0, 0, 0])
stest.expect_equal([b.r_f_nested_array_method_v for b in obj.bank.b],
                   [[0, 0, 0], [0, 0, 0], [0, 0, 0], [0, 0, 0]])
stest.expect_equal(obj.regular_context_v, 0)

# write changed values, then recheck
obj.set_saved = [-1, 2, 4, 2]

stest.expect_equal(obj.method_with_saved_with_init, -1)
stest.expect_equal(obj.method_with_saved_without_init, 2)
stest.expect_equal([p.port_array_method_v for p in obj.port.p], [0, 0, 6, 0])
stest.expect_equal([b.r_f_nested_array_method_v for b in obj.bank.b],
                   [[0, 0, 0], [0, 0, 7], [0, 0, 0], [0, 0, 0]])
stest.expect_equal(obj.regular_context_v, 1)
