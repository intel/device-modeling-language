# © 2021-2023 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import dev_util
import stest

import instrumentation_common as common

def subscribe(provider, offset, size, connection):
    order = [0,]
    def callback(connection, access, handle, user_data):
        subscribe.counter += 1
        order[0] = subscribe.counter
    provider.register_before_read(connection, offset, size, callback, None)
    return order
subscribe.counter = -1

def test(obj, provider):
    r1_order = subscribe(provider, 0, 4, connection = None)
    r2_order = subscribe(provider, 4, 4, connection = None)
    r2_order_connection = subscribe(
        provider, 4, 4, connection = common.mock_object())
    r1_order_connection = subscribe(
        provider, 0, 4, connection = common.mock_object())

    dev_util.Register_LE(obj.bank.b1, 0, 8).read()

    stest.expect_equal(r1_order[0], 0)
    stest.expect_equal(r2_order[0], 1)
    stest.expect_equal(r2_order_connection[0], 2)
    stest.expect_equal(r1_order_connection[0], 3)
