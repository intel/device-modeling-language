# © 2021-2023 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import dev_util
import stest

def callback(connection, access, handle, user_data):
    callback.called = True
callback.called = False

def test(obj, provider):
    maxuint = 0xffffffffffffffff
    max_address = maxuint - 3
    assert max_address % 4 == 0

    handle = provider.register_after_read(None, max_address, 4, callback, None)

    edge_register = dev_util.Register_LE(obj.bank.b1, max_address, 4)
    with stest.expect_log_mgr(obj.bank.b1, 'spec-viol'):
        stest.expect_exception(edge_register.read, [], dev_util.MemoryError)

    stest.expect_true(callback.called)
    provider.remove_callback(handle)
