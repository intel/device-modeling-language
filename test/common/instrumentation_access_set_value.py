# © 2021-2023 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import dev_util
import stest

def set_value_callback(value):
    def cb(connection, access, handle, user_data):
        access.set_value(handle, value)
    return cb

def test(obj, provider):
    handle_1 = provider.register_after_read(
        None, 0, 4, set_value_callback(4711), None)
    stest.expect_equal(dev_util.Register_LE(obj.bank.b1, 0, 4).read(), 4711)

    # An access miss is implicitly forgiven if the access value is set
    handle_2 = provider.register_after_read(
        None, 0xc, 4, set_value_callback(4711), None)

    # spec-viol is suppressed in 1.2 but not 1.4
    with stest.allow_log_mgr(obj, 'spec-viol'):
        stest.expect_equal(dev_util.Register_LE(obj.bank.b1, 0xc, 4).read(), 4711)

    provider.remove_callback(handle_1)
    provider.remove_callback(handle_2)
