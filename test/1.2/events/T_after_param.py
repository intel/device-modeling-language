# © 2021-2022 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest
cpu = SIM_create_object("clock", "clock", [["freq_mhz", 1]])
obj.queue = cpu
obj.alttest = 30
SIM_continue(100000)
stest.expect_equal(obj.num, 30)
stest.expect_equal(obj.p_num, [0, 30])
