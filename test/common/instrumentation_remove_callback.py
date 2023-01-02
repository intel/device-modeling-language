# © 2021-2023 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import dev_util
import stest

import instrumentation_common as common

raised = [False, False, False, False]
def create_callback(idx):
    def callback(connection, access, handle, user_data):
        global raised
        raised[idx] = True
    return callback

def trigger_and_expect(register, expected):
    global raised
    raised = [False, False, False, False]
    register.write(register.read())
    stest.expect_equal(raised, expected)

def test(obj, provider):
    con = common.mock_object()
    callbacks = {
        'before_read' : provider.register_before_read(
            con, 0, 4, create_callback(0), None),
        'after_read' : provider.register_after_read(
            con, 0, 4, create_callback(1), None),
        'before_write' : provider.register_before_write(
            con, 0, 4, create_callback(2), None),
        'after_write' : provider.register_after_write(
            con, 0, 4, create_callback(3), None)
        }

    r1 = dev_util.Register_LE(obj.bank.b1, 0, 4)
    trigger_and_expect(r1, [True, True, True, True])

    provider.remove_callback(callbacks['before_read'])
    trigger_and_expect(r1, [False, True, True, True])

    provider.remove_callback(callbacks['after_read'])
    trigger_and_expect(r1, [False, False, True, True])

    provider.remove_callback(callbacks['before_write'])
    trigger_and_expect(r1, [False, False, False, True])

    provider.remove_callback(callbacks['after_write'])
    trigger_and_expect(r1, [False, False, False, False])
