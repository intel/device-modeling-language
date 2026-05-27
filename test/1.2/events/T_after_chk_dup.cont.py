# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import simics
import stest
import conf

simics.SIM_run_command("peq")
print("Running 2 s")
simics.SIM_continue(2000000)

stest.expect_true(conf.obj.a_flag)
stest.expect_true(conf.obj.b_flag)
stest.expect_equal(conf.obj.c_flag, [True, True])
stest.expect_equal(conf.obj.d_flag, [True, True])
