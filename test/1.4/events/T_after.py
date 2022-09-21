# © 2021-2023 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest

cpu = SIM_create_object("clock", "clock", [["freq_mhz", 1]])
obj.queue = cpu


obj.single_operator = 0
obj.multi_operator = [[0, 0], [0, 0]]

obj.trigger = [[1, 1], [1, 1]]
obj.trigger_hook = [1, 1]
obj.operate = None
SIM_continue(99999)
stest.expect_equal(obj.trigger, [[0, 0], [0, 0]])
stest.expect_equal(obj.trigger_hook, [0, 0])
stest.expect_equal(obj.single_operator, 0)
stest.expect_equal(obj.multi_operator, [[0, 0], [0, 0]])
stest.expect_equal(obj.hook_operator, [[0, 0], [0, 0]])
SIM_continue(2)
stest.expect_equal(obj.trigger, [[0, 0], [0, 4]])
stest.expect_equal(obj.trigger_hook, [0, 2])
stest.expect_equal(obj.single_operator, 5)
stest.expect_equal(obj.multi_operator, [[0, 5], [3, 0]])
stest.expect_equal(obj.hook_operator, [[0, 5], [3, 0]])
SIM_continue(99998)
stest.expect_equal(obj.single_operator, 5)
SIM_continue(2)
stest.expect_equal(obj.single_operator, 3)

obj.trigger_constig = None
SIM_continue(99999)
stest.expect_equal(obj.constig_res, [0, 0])
SIM_continue(2)
stest.expect_equal(obj.constig_res, [4, 7 << 32 | 11])
