# © 2021-2023 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest

# Every time we access the counter, it will increase by 1

stest.expect_equal(obj.count, 0)

cpu = SIM_create_object("clock", "clock", [["freq_mhz", 1]])
obj.queue = cpu

stest.expect_equal(obj.count, 1)

obj.a = None

stest.expect_equal(obj.count, 3)

local = obj.a

stest.expect_equal(obj.count, 5)

obj.ev = None

stest.expect_equal(obj.count, 7)

SIM_continue(100000)

stest.expect_equal(obj.count, 9)

obj.iface.signal.signal_raise()

stest.expect_equal(obj.count, 11)
