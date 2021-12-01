# © 2021-2022 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest

cpu = SIM_create_object("clock", "clock", [["freq_mhz", 1]])
obj.queue = cpu

def init():
    obj.single_operator = 1
    obj.multi_operator = [[1, 1], [1, 1]]

init()
obj.test_1 = None
SIM_continue(99999)
stest.expect_equal(obj.single_operator, 1)
stest.expect_equal(obj.multi_operator, [[1, 1], [1, 1]])
SIM_continue(2)
stest.expect_equal(obj.single_operator, 216)
stest.expect_equal(obj.multi_operator, [[1, 216], [1, 1]])
init()
obj.test_2 = None
SIM_continue(99999)
stest.expect_equal(obj.single_operator, 1)
stest.expect_equal(obj.multi_operator, [[1, 1], [1, 1]])
SIM_continue(2)
stest.expect_equal(obj.single_operator, 2)
stest.expect_equal(obj.multi_operator, [[1, 5], [1, 1]])
