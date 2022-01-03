# © 2021-2022 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest

run_command("peq")
print("Running 2 s")
SIM_continue(2000000)

stest.expect_true(conf.obj.a_flag)
stest.expect_true(conf.obj.b_flag)
stest.expect_equal(conf.obj.c_flag, [True, True])
stest.expect_equal(conf.obj.d_flag, [True, True])
