# © 2021-2023 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest

clock = SIM_create_object('clock', 'clock', [['freq_mhz', 1]])
obj.queue = clock

obj.post = None
SIM_continue(1)

stest.expect_equal(obj.happened, 0xAFFFFFFFFFFFFFFF)
