# © 2021-2023 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest
import simics
from dev_util import Register_LE
import re

stest.expect_equal(obj.y_value, 42)

obj.x_target = obj.y.target
stest.expect_equal(obj.x_target, obj.y.target)
stest.expect_equal(obj.x_value, 42)

obj.x_target = None
stest.expect_equal(obj.x_target, None)
with stest.expect_log_mgr(obj, regex="not set"):
    stest.expect_equal(obj.x_value, None)
with stest.expect_log_mgr(obj, regex="not set"):
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

obj.log_level = 4
rex = re.compile("^read 8 bytes @ 0x100")
with stest.expect_log_mgr(obj, log_type="info", regex=rex):
    _ = obj.x_value

rex = re.compile("^wrote 8 bytes @ 0x100")
with stest.expect_log_mgr(obj, log_type="info", regex=rex):
    obj.x_value = 42

obj.x_address = 0
obj.log_level = 2
with stest.expect_log_mgr(obj, log_type="spec-viol"):
    rex = re.compile("^failed to read 8 bytes @ 0x0")
    with stest.expect_log_mgr(obj, log_type="info", regex=rex):
        _ = obj.x_value

    rex = re.compile("^failed to write 8 bytes @ 0x0")
    with stest.expect_exception_mgr(simics.SimExc_Attribute):
        with stest.expect_log_mgr(obj, log_type="info", regex=rex):
            obj.x_value = 42

