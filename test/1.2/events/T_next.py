# © 2021-2023 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest
cpu = SIM_create_object("clock", "clock", [["freq_mhz", 1]])
obj.queue = cpu
stest.expect_true(obj.runtest)
