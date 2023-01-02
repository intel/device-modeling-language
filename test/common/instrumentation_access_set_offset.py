# © 2021-2023 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import dev_util
import stest

def bump_offset(connection, access, handle, user_data):
    access.set_offset(handle, access.offset(handle) + 4)

def test(obj, provider):
    handle = provider.register_before_read(
        None, 0, 4, bump_offset, None)

    r1 = dev_util.Register_LE(obj.bank.b1, 0, 4)
    stest.expect_equal(r1.read(), 2)

    provider.remove_callback(handle)
