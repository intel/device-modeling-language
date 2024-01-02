# © 2021-2024 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import dev_util
import stest
for name in ['r1', 'r2', 'r3', 'r4']:
    setattr(obj, 'b_' + name, 4)
    # set adds 1, get adds 2
    stest.expect_equal(getattr(obj, 'b_' + name), 7)

for bank in [obj.bank.le, obj.bank.be]:
    r = dev_util.Register_LE(bank, 0)
    r.write(0)
    # field with least significant bit (i==3) is accessed first
    stest.expect_equal(bank.r_write_order, 0x03000102)
    r.read()
    stest.expect_equal(bank.r_read_order, 0x03000102)
