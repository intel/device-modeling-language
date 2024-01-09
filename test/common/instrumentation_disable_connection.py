# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import dev_util
import stest

import instrumentation_common as common

raised = False
def callback(connection, access, handle, user_data):
    global raised
    raised = True

def trigger_and_expect(register, expected):
    global raised
    raised = False
    register.read()
    stest.expect_equal(raised, expected)

def test(obj, provider):
    connection = common.mock_object()

    provider.register_before_read(connection, 0, 4, callback, None)

    register = dev_util.Register_LE(obj.bank.b1, 0, 4)
    trigger_and_expect(register, True)

    provider.disable_connection_callbacks(connection)
    trigger_and_expect(register, False)

    provider.enable_connection_callbacks(connection)
    trigger_and_expect(register, True)
