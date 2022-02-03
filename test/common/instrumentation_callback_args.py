# © 2021-2022 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import dev_util
import simics
import stest

import instrumentation_common as common

def expect_args(connection, user_data):
    def callback(got_connection, _handle, _access, got_user_data):
        stest.expect_equal(got_connection, connection)
        stest.expect_equal(got_user_data, user_data)
        expect_args.num_calls += 1
    expect_args.num_callbacks += 1
    return callback
expect_args.num_callbacks = 0
expect_args.num_calls = 0

def test(obj, provider):
    anon = provider.register_before_read(
        None, 0, 4, expect_args(None, 4711), 4711)

    connection = common.mock_object()
    provider.register_before_read(
        connection, 0, 4, expect_args(connection, 4711), 4711)
    provider.register_after_read(
        connection, 0, 4, expect_args(connection, 4711), 4711)

    provider.register_before_write(
        connection, 0, 4, expect_args(connection, 4711), 4711)
    provider.register_after_write(
        connection, 0, 4, expect_args(connection, 4711), 4711)

    r1 = dev_util.Register_LE(obj.bank.b1, 0, 4)
    r1.write(r1.read())

    stest.expect_equal(expect_args.num_calls, expect_args.num_callbacks)
    provider.remove_callback(anon)
    provider.remove_connection_callbacks(connection)
