# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest

with stest.expect_log_mgr(obj, "error"):
    obj.after_test = 1

with stest.expect_log_mgr(obj, "error"):
    obj.post_test = 1
