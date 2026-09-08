# © 2022 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest
import simics
import testenv
obj = testenv.instantiate()

with stest.expect_exception_mgr(simics.SimExc_IllegalValue):
    obj.s = ["test", []]
