# © 2021-2023 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest

with stest.expect_exception_mgr(SimExc_IllegalValue):
    obj.a = None

with stest.expect_exception_mgr(SimExc_General):
    SIM_get_attribute(obj, "a")

with stest.expect_exception_mgr(SimExc_IllegalValue):
    obj.b = None

with stest.expect_exception_mgr(SimExc_General):
    SIM_get_attribute(obj, "b")
