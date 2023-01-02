# © 2021-2023 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import dev_util
import stest

import instrumentation_common as common

def cb(connection, access, handle, user_data):
    pass

def test(obj, subscribe, order):
    stest.expect_equal(len(order.get_connections()), 0)

    con0 = common.mock_object()
    con1 = common.mock_object()
    con2 = common.mock_object()
    subscribe.register_before_read(con0, 0, 0, cb, None)
    subscribe.register_before_read(con1, 0, 0, cb, None)
    subscribe.register_before_read(con2, 0, 0, cb, None)
    stest.expect_equal(order.get_connections(), [con0, con1, con2])

    order.move_before(con2, con0)
    stest.expect_equal(order.get_connections(), [con2, con0, con1])

    order.move_before(con1, con0)
    stest.expect_equal(order.get_connections(), [con2, con1, con0])

    # if anchor-id isn't provided connection is made last
    order.move_before(con2, None)
    stest.expect_equal(order.get_connections(), [con1, con0, con2])
