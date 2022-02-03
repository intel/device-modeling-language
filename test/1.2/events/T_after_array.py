# © 2021-2022 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest
cpu = SIM_create_object("clock", "clock", [["freq_mhz", 1]])
obj.queue = cpu
obj.alttest = 1
SIM_continue(100000)
stest.expect_true(obj.runtest)
