# © 2021-2024 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest, dev_util

regs = [dev_util.Register_LE(obj.bank.b, i, size=1) for i in range(3)]

obj.b_r = [1, 2, 3]


stest.expect_equal(regs[0].read(), 1)
with stest.expect_log_mgr(obj.bank.b, 'spec-viol'):
    with stest.expect_exception_mgr(dev_util.MemoryError):
        regs[1].read()
stest.expect_equal(regs[2].read(), 3)
