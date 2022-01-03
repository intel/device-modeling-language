# © 2021-2022 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import dev_util
import simics
import stest

def test_inquiry(register_callback, remove_callback,
                 inquiry_access, regular_access):
    def callback(connection, access, handle, user_data):
        callback.called = True
    callback.called = False

    handle = register_callback(None, 0, 4, callback, None)

    inquiry_access()
    stest.expect_false(callback.called)

    regular_access()
    stest.expect_true(callback.called)

    remove_callback(handle)

def test(obj, provider):
    r = dev_util.Register_LE(obj.bank.b1, 0, 4)
    r_inq = dev_util.Register_LE(obj.bank.b1, 0, 4)
    r_inq.read_transaction.inquiry = True
    r_inq.write_transaction.inquiry = True

    test_inquiry(provider.register_before_read, provider.remove_callback,
                 r_inq.read, r.read)
    test_inquiry(provider.register_after_read, provider.remove_callback,
                 r_inq.read, r.read)

    test_inquiry(provider.register_before_write, provider.remove_callback,
                 lambda: r_inq.write(0), lambda: r.write(0))
    test_inquiry(provider.register_after_write, provider.remove_callback,
                 lambda: r_inq.write(0), lambda: r.write(0))
