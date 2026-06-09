# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import simics
import stest

obj = simics.SIM_create_object('test', 'obj', [['req', 'banan']])

_ = obj.req
_ = obj.opt
_ = obj.pse

stest.expect_equal(obj.req, "banan")
stest.expect_equal(obj.opt, None)
stest.expect_equal(obj.pse, None)

obj.req = "bazooka"
obj.opt = "apelsin"
obj.pse = "krutong"

stest.expect_equal(obj.req, "bazooka")
stest.expect_equal(obj.opt, "apelsin")
stest.expect_equal(obj.pse, "krutong")

stest.expect_exception(setattr, [obj, 'req', None], simics.SimExc_Type)
obj.opt = None
#obj.pse = None
#
#stest.expect_equal(obj.req, "bazooka")
#stest.expect_equal(obj.opt, "apelsin")
#stest.expect_equal(obj.pse, "krutong")

