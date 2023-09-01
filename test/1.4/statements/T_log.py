# © 2023 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest

try:
    with stest.expect_log_mgr(obj, 'info'):
        obj.test_nolog = None
except stest.TestFailure as e:
    if not str(e).startswith('Expected log'):
        raise
else:
    stest.fail('Unexpected logging occurred')
