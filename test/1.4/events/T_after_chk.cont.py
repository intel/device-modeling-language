# © 2021-2023 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest

run_command("peq")
conf.obj.cancel_afters = None
print("Running 2 s")
SIM_continue(2000000)

stest.expect_equal(conf.obj.flag, True)
stest.expect_equal(conf.obj.g_flag, False)
stest.expect_equal(conf.obj.port.p[0][0].flag, False)
stest.expect_equal(conf.obj.port.p[1][0].flag, False)
stest.expect_equal(conf.obj.port.p[1][1].flag, True)

stest.expect_equal(conf.obj.storage, 1)
stest.expect_equal(conf.obj.g_storage, 0)
stest.expect_equal(conf.obj.port.p[0][0].storage, 0)
stest.expect_equal(conf.obj.port.p[1][0].storage, 0)
stest.expect_equal(conf.obj.port.p[1][1].storage, 4)
