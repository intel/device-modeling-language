# © 2021-2024 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest

cpu = SIM_create_object("clock", "clock", [["freq_mhz", 1]])
obj.queue = cpu

# Just posting user data with an event works.
obj.post_all = 100
SIM_continue(1)
stest.expect_equal(obj.a, 100)
stest.expect_equal(obj.b, [[0, 0], [0, 100]])

# Read and re-write clock.time_queue to force the event data to be
# converted back and forth to an attribute.
obj.post_all = 100
cpu.time_queue = list(cpu.time_queue)
SIM_continue(1)
# 20 + 3 was added to the parameter because of translation via
# get_event_info() + set_event_info()
stest.expect_equal(obj.a, 123)
stest.expect_equal(obj.b, [[0, 0], [0, 123]])
