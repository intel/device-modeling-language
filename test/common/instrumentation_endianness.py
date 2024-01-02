# © 2021-2024 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import dev_util
import simics
import stest

def expect_value(value):
    def callback(connection, access, handle, user_data):
        stest.expect_equal(access.value(handle), value)
    return callback

def test(bank):
    subscribe = bank.iface.bank_instrumentation_subscribe

    c1 = subscribe.register_after_read(
        None, 0, 4, expect_value(0x01020304), None)

    c2 = subscribe.register_after_read(
        None, 4, 4, expect_value(0x0A0B0C0D), None)

    # endianness of these registers don't matter
    dev_util.Register_LE(bank, 0, 4).read()
    dev_util.Register_LE(bank, 4, 4).read()

    subscribe.remove_callback(c1)
    subscribe.remove_callback(c2)
