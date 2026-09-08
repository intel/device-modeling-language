# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import simics
import stest
import testenv
obj = testenv.instantiate()

with stest.expect_exception_mgr(simics.SimExc_IllegalValue):
    obj.a = None

with stest.expect_exception_mgr(simics.SimExc_General):
    simics.SIM_get_attribute(obj, "a")

with stest.expect_exception_mgr(simics.SimExc_IllegalValue):
    obj.b = None

with stest.expect_exception_mgr(simics.SimExc_General):
    simics.SIM_get_attribute(obj, "b")
