# © 2026 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

# Test that a callback may remove its own registration, or all
# registrations of its connection, while it is being invoked. The
# current dispatch must complete, and later accesses must not invoke
# the removed callbacks.

import dev_util
import stest

import instrumentation_common as common

calls = []
def create_callback(key, on_call=None):
    def callback(connection, access, handle, user_data):
        global calls
        calls += [key]
        if on_call:
            on_call()
    return callback

def trigger_and_expect(register, expected):
    global calls
    calls = []
    register.read()
    stest.expect_equal(sorted(calls), sorted(expected))

def test_remove_own_callback(obj, provider):
    con = common.mock_object()
    handles = {}
    handles['self'] = provider.register_before_read(
        con, 0, 4, create_callback(
            'self', lambda: provider.remove_callback(handles['self'])), None)
    handles['other'] = provider.register_before_read(
        con, 0, 4, create_callback('other'), None)
    handles['after'] = provider.register_after_read(
        con, 0, 4, create_callback('after'), None)

    r1 = dev_util.Register_LE(obj.bank.b1, 0, 4)
    # All callbacks registered when the access started are invoked
    trigger_and_expect(r1, ['self', 'other', 'after'])
    # The callback that removed itself is gone
    trigger_and_expect(r1, ['other', 'after'])

    provider.remove_connection_callbacks(con)
    trigger_and_expect(r1, [])

def test_remove_own_connection(obj, provider):
    con1 = common.mock_object()
    con2 = common.mock_object()
    provider.register_before_read(
        con1, 0, 4, create_callback(
            'con1_remover',
            lambda: provider.remove_connection_callbacks(con1)), None)
    provider.register_before_read(
        con1, 0, 4, create_callback('con1_other'), None)
    provider.register_after_read(
        con1, 0, 4, create_callback('con1_after'), None)
    provider.register_before_read(
        con2, 0, 4, create_callback('con2'), None)

    r1 = dev_util.Register_LE(obj.bank.b1, 0, 4)
    # The remaining before_read callbacks of con1 were collected before
    # the connection was removed, but after_read is dispatched separately
    trigger_and_expect(r1, ['con1_remover', 'con1_other', 'con2'])
    # Only con2 remains
    trigger_and_expect(r1, ['con2'])

    provider.remove_connection_callbacks(con2)
    trigger_and_expect(r1, [])

def test_remove_other_connection(obj, provider):
    con1 = common.mock_object()
    con2 = common.mock_object()
    provider.register_before_read(
        con1, 0, 4, create_callback(
            'con1', lambda: provider.remove_connection_callbacks(con2)), None)
    provider.register_before_read(
        con2, 0, 4, create_callback('con2'), None)

    r1 = dev_util.Register_LE(obj.bank.b1, 0, 4)
    trigger_and_expect(r1, ['con1', 'con2'])
    trigger_and_expect(r1, ['con1'])

    provider.remove_connection_callbacks(con1)
    trigger_and_expect(r1, [])

def test(obj, provider):
    test_remove_own_callback(obj, provider)
    test_remove_own_connection(obj, provider)
    test_remove_other_connection(obj, provider)
