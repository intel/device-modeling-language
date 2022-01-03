# © 2021-2022 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest

stest.expect_equal(obj.count, 0)

cpu = SIM_create_object("clock", "clock", [["freq_mhz", 1]])
# TODO: is this the desired behaviour? attributes not controlled by DML do not
#       count for the context
obj.queue = cpu

stest.expect_equal(obj.count, 0)

obj.a = None

stest.expect_equal(obj.count, 1)

local = obj.a

stest.expect_equal(obj.count, 2)

obj.ev = None

stest.expect_equal(obj.count, 3)

SIM_continue(100000)

stest.expect_equal(obj.count, 4)

obj.iface.signal.signal_raise()

stest.expect_equal(obj.count, 5)
