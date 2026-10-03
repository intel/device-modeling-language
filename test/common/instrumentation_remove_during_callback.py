# © 2026 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

# Test that removing a callback during dispatch prevents it from being called.

import dev_util
import stest

import instrumentation_common as common

calls = []
def create_callback(key, on_call=None):
    def callback(connection, access, handle, user_data):
        calls.append(key)
        if on_call:
            on_call()
    return callback

def trigger_and_expect(register, expected):
    assert not calls
    register.read()
    stest.expect_equal(sorted(calls), sorted(expected))
    del calls[:]

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
    trigger_and_expect(r1, ['self', 'other', 'after'])
    trigger_and_expect(r1, ['other', 'after'])

    provider.remove_connection_callbacks(con)
    trigger_and_expect(r1, [])

def test_remove_pending_callback(obj, provider):
    con = common.mock_object()
    handles = {}

    def remove_pending():
        provider.remove_callback(handles['pending'])

    provider.register_before_read(con, 0, 4,
                                  create_callback('remover', remove_pending), None)
    handles['pending'] = provider.register_before_read(
        con, 0, 4, create_callback('pending'), None)
    provider.register_before_read(con, 0, 4, create_callback('following'), None)

    r1 = dev_util.Register_LE(obj.bank.b1, 0, 4)
    trigger_and_expect(r1, ['remover', 'following'])

    provider.remove_connection_callbacks(con)
    trigger_and_expect(r1, [])

def test(obj, provider):
    test_remove_own_callback(obj, provider)
    test_remove_pending_callback(obj, provider)
