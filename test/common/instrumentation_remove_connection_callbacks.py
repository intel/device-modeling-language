# © 2021-2024 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import dev_util
import simics
import stest

import instrumentation_common as common

calls = []
def create_callback(key):
    def callback(connection, access, handle, user_data):
        global calls
        calls += [(key, connection)]
    return callback

def trigger_and_expect(register, expected):
    global calls
    register.write(register.read())
    stest.expect_equal(calls, expected)
    calls = []

def test(obj, provider):
    con1 = common.mock_object()
    con2 = common.mock_object()

    provider.register_before_read(
        con1, 0, 4, create_callback('before_read'), None)
    provider.register_after_read(
        con1, 0, 4, create_callback('after_read'), None)

    provider.register_before_write(
        con2, 0, 4, create_callback('before_write'), None)
    provider.register_after_write(
        con2, 0, 4, create_callback('after_write'), None)

    r1 = dev_util.Register_LE(obj.bank.b1, 0, 4)
    trigger_and_expect(r1, [('before_read', con1),
                            ('after_read', con1),
                            ('before_write', con2),
                            ('after_write', con2)])

    provider.remove_connection_callbacks(con1)
    trigger_and_expect(r1, [('before_write', con2),
                            ('after_write', con2)])

    provider.remove_connection_callbacks(con2)
    trigger_and_expect(r1, [])
