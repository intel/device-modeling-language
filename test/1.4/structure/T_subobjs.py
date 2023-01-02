# © 2021-2023 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest

with stest.expect_log_mgr(obj.port.p[1], 'info'):
    obj.port.p[1].pa = 3
with stest.expect_log_mgr(obj.d[1], 'spec-viol'):
    obj.d[1].da = 3
with stest.expect_log_mgr(obj.bank.b[1], 'unimpl'):
    obj.bank.b[1].ba = 3

def matrix(i):
    return [[0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0],
            [0, 0, 0, i, 0]]

stest.expect_true(obj.runtest)
stest.expect_equal(obj.bank.b[0].bb, matrix(0))
stest.expect_equal(obj.bank.b[1].bb, matrix(8))
obj.bank.b[0].bb = matrix(6)
stest.expect_equal(obj.bank.b[0].bb, matrix(6))
