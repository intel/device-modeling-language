# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest
import simics
from dev_util import Register_LE

stest.expect_equal(obj.y_value, 42)

obj.x_target = obj.y.target
stest.expect_equal(obj.x_value, 42)

obj.x_target = None
with stest.expect_log_mgr(obj):
    stest.expect_equal(obj.x_value, None)
with stest.expect_log_mgr(obj):
    with stest.expect_exception_mgr(simics.SimExc_Attribute):
        obj.x_value = 3

with stest.expect_exception_mgr(simics.SimExc_Attribute):
    obj.x_target = obj  # not a valid map target

with stest.expect_exception_mgr(simics.SimExc_Attribute):
    obj.x_target = [obj, "z"]  # port interface not allowed

r = Register_LE(obj.bank.z, 0x100, 8)

obj.x_target = obj.bank.z
obj.x_size = 8
obj.x_address = 0x100
obj.x_value = 0xaaaaaaaabbbbbbbb
stest.expect_equal(r.read(), 0xaaaaaaaabbbbbbbb)
stest.expect_equal(obj.x_value, 0xaaaaaaaabbbbbbbb)

data = tuple((0x12345678abcdef).to_bytes(8, 'little'))
obj.x_data = data
stest.expect_equal(r.read(), int.from_bytes(data, 'little'))
data = tuple(reversed(data))
r.write(int.from_bytes(data, 'little'))
stest.expect_equal(obj.x_data, data)
