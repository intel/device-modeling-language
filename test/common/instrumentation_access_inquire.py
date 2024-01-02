# © 2021-2024 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import dev_util
import stest

def inquire(connection, access, handle, user_data):
    access.inquire(handle)

def after_read(connection, access, handle, user_data):
    after_read.called = True

def test(obj, provider):
    after_read.called = False

    r4 = dev_util.Register_LE(obj.bank.b2, 0, 4)
    r5 = dev_util.Register_LE(obj.bank.b2, 4, 4)
    r6 = dev_util.Register_LE(obj.bank.b2, 8, 4)
    r7 = dev_util.Register_LE(obj.bank.b2, 12, 4)

    inquire.handle = provider.register_before_read(
        None, 12, 4, inquire, None)
    after_read.handle = provider.register_after_read(
        None, 12, 4, after_read, None)

    stest.expect_equal(r7.read(), 7)
    stest.expect_true(after_read.called)

    stest.expect_equal(r4.read(), 4)
    stest.expect_equal(r5.read(), 5)
    stest.expect_equal(r6.read(), 6)

    provider.remove_callback(inquire.handle)
    provider.remove_callback(after_read.handle)
