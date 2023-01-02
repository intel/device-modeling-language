# © 2021-2023 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import dev_util
import stest

import instrumentation_common as common

raised = []
def create_callback(idx):
    def callback(connection, access, handle, user_data):
        global raised
        raised += [idx,]
    return callback

def trigger_and_expect(register, expected):
    global raised
    raised = []
    register.write(register.read())
    stest.expect_equal(raised, expected)

def test_callback_order(provider, r1, subscribe):
    callbacks = [subscribe(create_callback(id_)) for id_ in (0, 1, 2, 3)]
    trigger_and_expect(r1, [0, 1, 2, 3])

    provider.remove_callback(callbacks[1])
    trigger_and_expect(r1, [0, 2, 3])

    provider.remove_callback(callbacks[0])
    trigger_and_expect(r1, [2, 3])

    provider.remove_callback(callbacks[3])
    trigger_and_expect(r1, [2,])

    provider.remove_callback(callbacks[2])
    trigger_and_expect(r1, [])

def test(obj, provider):
    r1 = dev_util.Register_LE(obj.bank.b1, 0, 4)

    register_anonymous = lambda cb: provider.register_before_read(
        None, 0, 4, cb, None)
    test_callback_order(provider, r1, register_anonymous)

    register_connection = lambda cb: provider.register_before_read(
        common.mock_object(), 0, 4, cb, None)
    test_callback_order(provider, r1, register_connection)
