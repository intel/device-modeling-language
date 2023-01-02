# © 2021-2023 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import dev_util
import stest

def suppress(connection, access, handle, user_data):
    access.suppress(handle)

def after_write(connection, access, handle, user_data):
    after_write.called = True
after_write.called = False

def test(obj, provider):
    r1 = dev_util.Register_LE(obj.bank.b1, 0, 4)
    stest.expect_equal(r1.read(), 1)

    handle_1 = provider.register_before_write(
        None, 0, 4, suppress, None)
    handle_2 = provider.register_after_write(
        None, 0, 4, after_write, None)

    r1.write(4711)
    stest.expect_true(after_write.called)
    stest.expect_equal(r1.read(), 1)

    provider.remove_callback(handle_1)
    provider.remove_callback(handle_2)
