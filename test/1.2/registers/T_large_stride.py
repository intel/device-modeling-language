# © 2021-2024 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest, dev_util

regs = [dev_util.Register_LE(obj.bank.b, i*0x10000000, size=1) for i in range(16)]

obj.b_r = list(range(16))

# this will trigger a miss access if there is an overflow in
# offset calculations
stest.expect_equal(regs[8].read(), 8)
