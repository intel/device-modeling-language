# © 2021 Intel Corporation
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

SIM_notify(obj, SIM_notifier_type("exported-entry"))

stest.expect_equal(obj.count, 6)

SIM_notify(obj, SIM_notifier_type("statically-exported-entry"))

stest.expect_equal(obj.count, 7)

obj.immediate_after = None

stest.expect_equal(obj.count, 8)

SIM_process_pending_work()

stest.expect_equal(obj.count, 9)
