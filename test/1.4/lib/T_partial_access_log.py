# © 2025 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import dev_util
import stest

for (order, bank) in [('little', obj.bank.le), ('big', obj.bank.be)]:
    bank.log_level = 4
    bank.r = 0x04030201
    if order == 'little':
        whole = dev_util.Register_LE(bank, 4, size=4)
        one = dev_util.Register_LE(bank, 5, size=1)
        three = dev_util.Register_LE(bank, 5, size=3)
    else:
        whole = dev_util.Register_BE(bank, 4, size=4)
        one = dev_util.Register_BE(bank, 6, size=1)
        three = dev_util.Register_BE(bank, 4, size=3)
    with stest.expect_log_mgr(
            bank, 'info',
            regex='Read from register [lb]e.r -> 0x04030201'):
        whole.read()
    with stest.expect_log_mgr(
            bank, 'info',
            regex='Partial read from register [lb]e.r[[]15:8[]] -> 0x02'):
        one.read()
    with stest.expect_log_mgr(
            bank, 'info',
            regex='Partial read from register [lb]e.r[[]31:8[]] -> 0x040302'):
        three.read()

    with stest.expect_log_mgr(
            bank, 'info',
            regex='Write to register [lb]e.r <- 0x01234567'):
        whole.write(0x01234567)
    with stest.expect_log_mgr(
            bank, 'info',
            regex='Partial write to register [lb]e.r[[]15:8[]] <- 0x03'):
        one.write(0x3)
    with stest.expect_log_mgr(
            bank, 'info',
            regex='Partial write to register [lb]e.r[[]31:8[]] <- 0x012345'):
        three.write(0x012345)
    with stest.expect_log_mgr(
            bank, 'info',
            regex='Partially setting register [lb]e.r[[]31:8[]] <- 0x012345'):
        simics.SIM_issue_transaction(
            bank, simics.transaction_t(write=True, size=3,
                                       data=(0x012345).to_bytes(3, order),
                                       inquiry=True),
            5 if order == 'little' else 4)
