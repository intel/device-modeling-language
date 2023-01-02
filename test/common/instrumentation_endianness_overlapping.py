# © 2021-2023 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import dev_util
import simics
import stest

def expect_value(value):
    def callback(connection, access, handle, user_data):
        stest.expect_equal(access.value(handle), value)
    return callback

def test(bank, expected):
    subscribe = bank.iface.bank_instrumentation_subscribe
    connection = subscribe.register_after_read(
        None, 0, 8, expect_value(expected), None)

    # endianness of register doesn't matter
    dev_util.Register_LE(bank, 0, 8).read()

    subscribe.remove_callback(connection)
