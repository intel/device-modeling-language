# © 2021-2023 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import dev_util
import stest

import instrumentation_common as common

called = {
    'b1_r1_before_read' : False,
    'b2_r1_before_read' : False
}

def callback(id_):
    def cb(connection, access, handle, user_data):
        global called
        called[id_] = True
    return cb

def clear():
    global called
    for (callback, _) in called.items():
        called[callback] = False

def test(obj, b1, b2):
    b1_r1 = dev_util.Register_LE(obj.bank.b1, 0, 4)
    b2_r1 = dev_util.Register_LE(obj.bank.b2, 0, 4)

    con = common.mock_object()
    b1.register_before_read(con, 0, 4, callback('b1_r1_before_read'), None)
    b2.register_before_read(con, 0, 4, callback('b2_r1_before_read'), None)

    b1_r1.read()
    stest.expect_equal(called['b1_r1_before_read'], True)
    stest.expect_equal(called['b2_r1_before_read'], False)

    clear()
    b2_r1.read()
    stest.expect_equal(called['b1_r1_before_read'], False)
    stest.expect_equal(called['b2_r1_before_read'], True)
