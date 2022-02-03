# © 2021-2022 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest
stest.expect_equal(obj.b_r, [0, 1])
stest.expect_equal(obj.b_p, 0)
stest.expect_equal(obj.b_q, 0x123b)
stest.expect_equal(obj.b_g_r, 0)
obj.b_r = [4, 9]
obj.b_q = 0xffff
obj.ports.POWER.signal.signal_raise()
stest.expect_equal(obj.b_r, [0, 1])
stest.expect_equal(obj.b_p, 16)
stest.expect_equal(obj.b_q, 0x12cb)
stest.expect_equal(obj.b_g_r, 2)
obj.b_r = [4, 9]
obj.b_q = 0xffff
obj.ports.HRESET.signal.signal_raise()
stest.expect_equal(obj.b_r, [0, 1])
stest.expect_equal(obj.b_p, 17)
stest.expect_equal(obj.b_q, 0x12db)
stest.expect_equal(obj.b_g_r, 3)
obj.b_r = [4, 9]
obj.b_q = 0xffff
obj.ports.SRESET.signal.signal_raise()
stest.expect_equal(obj.b_r, [0, 1])
stest.expect_equal(obj.b_p, 18)
stest.expect_equal(obj.b_q, 0x6789)
stest.expect_equal(obj.b_g_r, 4)
