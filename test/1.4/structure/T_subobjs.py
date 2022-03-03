# © 2021-2022 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest

with stest.expect_log_mgr(obj.port.p[1], 'info'):
    obj.port.p[1].pa = 3
with stest.expect_log_mgr(obj.d[1], 'spec-viol'):
    obj.d[1].da = 3
with stest.expect_log_mgr(obj.bank.b[1], 'unimpl'):
    obj.bank.b[1].ba = 3
