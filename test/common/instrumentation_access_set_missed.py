# © 2021-2023 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import dev_util
import stest

def forgive_miss(connection, access, handle, user_data):
    access.set_missed(handle, False)

def inject_miss(connection, access, handle, user_data):
    access.set_missed(handle, True)

def test(obj, provider):
    r = dev_util.Register_LE(obj.bank.b1, 12, 4)

    # It appears the dev_util register class throws an exception which we
    # need to suppress in addition to the spec violation itself
    with stest.expect_log_mgr(obj.bank.b1, 'spec-viol'):
        stest.expect_exception(r.read, [], dev_util.MemoryError)

    handle_1 = provider.register_after_read(None, 12, 4, forgive_miss, None)

    r.read()

    handle_2 = provider.register_after_read(None, 12, 4, inject_miss, None)

    with stest.expect_log_mgr(obj.bank.b1, 'spec-viol'):
        stest.expect_exception(r.read, [], dev_util.MemoryError)

    provider.remove_callback(handle_1)
    provider.remove_callback(handle_2)
