# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import simics
from stest import *

obj = simics.SIM_create_object('test', 'obj', [['req', 'banan']])

_ = obj.req
_ = obj.opt
_ = obj.pse

expect_equal(obj.req, "banan")
expect_equal(obj.opt, None)
expect_equal(obj.pse, None)

obj.req = "bazooka"
obj.opt = "apelsin"
obj.pse = "krutong"

expect_equal(obj.req, "bazooka")
expect_equal(obj.opt, "apelsin")
expect_equal(obj.pse, "krutong")

expect_exception(setattr, [obj, 'req', None], SimExc_Type)
obj.opt = None
#obj.pse = None
#
#expect_equal(obj.req, "bazooka")
#expect_equal(obj.opt, "apelsin")
#expect_equal(obj.pse, "krutong")

