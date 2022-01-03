# © 2021-2022 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

from functools import reduce
import operator
import stest, dev_util
run_command("log-level 4")

def check(bank, offset, size):
    data = reduce(operator.ior, (
        n << (n * 4) for n in range(size * 2)))
    reg = dev_util.Register_LE(bank, offset, size)
    print("Writing %r to %r" % (data, offset))
    reg.write(data)
    stest.expect_equal(reg.read(), data)

def check_bank(bank):
    check(bank, 0, 2)
    check(bank, 4, 4)
    check(bank, 8, 2)
    check(bank, 12, 4)

check_bank(obj.bank.b1)
check_bank(obj.bank.b2)
check_bank(obj.bank.b3)
check_bank(obj.bank.b4)
