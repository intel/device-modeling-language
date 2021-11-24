# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import dev_util
import simics
import stest

def expect_error(connection, access, handle, obj):
    with stest.expect_log_mgr(obj, 'error'):
        access.value(handle)

def test(obj, provider, log_obj):
    handle = provider.register_after_read(
        None, 0xc, 4, expect_error, obj.bank.b1)

    # It appears the dev_util register class throws an exception which we
    # need to suppress in addition to the spec violation itself
    r = dev_util.Register_LE(obj.bank.b1, 12, 4)
    with stest.expect_log_mgr(log_obj, 'spec-viol'):
        stest.expect_exception(r.read, [], dev_util.MemoryError)

    provider.remove_callback(handle)
