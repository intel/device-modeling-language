# © 2026 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

# Test that anonymous callbacks may be removed or registered during dispatch.
# A removed callback is not invoked, and a registered callback is not invoked
# until the next access.

import dev_util
import stest

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
    handles = {}
    handles['self'] = provider.register_before_read(
        None, 0, 4, create_callback(
            'self', lambda: provider.remove_callback(handles.pop('self'))),
        None)
    handles['other'] = provider.register_before_read(
        None, 0, 4, create_callback('other'), None)
    handles['after'] = provider.register_after_read(
        None, 0, 4, create_callback('after'), None)

    r1 = dev_util.Register_LE(obj.bank.b1, 0, 4)
    trigger_and_expect(r1, ['self', 'other', 'after'])
    trigger_and_expect(r1, ['other', 'after'])

    for handle in handles.values():
        provider.remove_callback(handle)
    trigger_and_expect(r1, [])

def test_remove_pending_callback(obj, provider):
    handles = {}

    def remove_pending():
        if 'pending' in handles:
            provider.remove_callback(handles.pop('pending'))

    handles['remover'] = provider.register_before_read(
        None, 0, 4, create_callback('remover', remove_pending), None)
    handles['pending'] = provider.register_before_read(
        None, 0, 4, create_callback('pending'), None)
    handles['following'] = provider.register_before_read(
        None, 0, 4, create_callback('following'), None)

    r1 = dev_util.Register_LE(obj.bank.b1, 0, 4)
    trigger_and_expect(r1, ['remover', 'following'])

    for handle in handles.values():
        provider.remove_callback(handle)
    trigger_and_expect(r1, [])

def test(obj, provider):
    test_remove_own_callback(obj, provider)
    test_remove_pending_callback(obj, provider)
