# © 2021-2022 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest

SIM_continue(1000000)

stest.expect_equal(conf.obj.a_flag, True)
stest.expect_equal(conf.obj.b_flag, True)
stest.expect_equal(conf.obj.c_flag, [True, False])
stest.expect_equal(conf.obj.d_flag, [False, True])
