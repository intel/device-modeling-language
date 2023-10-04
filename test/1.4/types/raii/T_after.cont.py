# © 2023 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest

obj = conf.obj
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
