# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import simics
import stest
stest.expect_equal(obj.b_r0, 0x12345678)
stest.expect_equal(obj.b_r1, 0x12345678)

try:
    obj.b_r0 = 17
    stest.fail("register is writable")
except simics.SimExc_AttrNotWritable:
    pass

try:
    obj.b_r1 = 17
    stest.fail("register is writable")
except simics.SimExc_AttrNotWritable:
    pass
