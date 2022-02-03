# © 2021-2022 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest
import dev_util

regs = [[dev_util.Register((obj, 'b', i*2 + j*5), size = 1)
         for j in range(2)]
        for i in range(2)]

for i in range(2):
    for j in range(2):
        obj.b_g_r[i][j] = 43
        regs[i][j].write(5)
        stest.expect_equal(obj.b_g_r[i][j], 5)
