# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

# Test that the read_only template works on registers with and without
# fields (bug 16355)

from stest import *
import dev_util as du

# Create register objects for accessing the DUT. R0 is a simple
# read-only register and R1 has a field. Both registers implement the
# read_only template.
r0 = du.Register_LE(obj.bank.b, 0)
r1 = du.Register_LE(obj.bank.b, 4)

# Registers should have 0 as reset value and ignore writes (since they
# are read_only). They should also emit 'spec-viol' on write access.
expect_equal(r0.read(), 0)
expect_log(r0.write, [17], obj.bank.b, 'spec-viol')
expect_equal(r0.read(), 0)

expect_equal(r1.read(), 0)
expect_log(r1.write, [17], obj.bank.b, 'spec-viol')
expect_equal(r1.read(), 0)

# read-only fields should be writable with the same value
r2 = du.Register_LE(obj.bank.b, 8)

expect_equal(r2.read(), 0)
r2.write(0)
expect_log(r2.write, [17], obj.bank.b, 'spec-viol')
expect_equal(r2.read(), 0)

# Testing no-alloc variants of read_only template
r3 = du.Register_LE(obj.bank.b, 12)
expect_equal(r3.read(), 0)
expect_log(r3.write, [17], obj.bank.b, 'spec-viol')
expect_equal(r3.read(), 0)

r4 = du.Register_LE(obj.bank.b, 16)
expect_equal(r4.read(), 0)
expect_log(r4.write, [17], obj.bank.b, 'spec-viol')
expect_equal(r4.read(), 0)

r5 = du.Register_LE(obj.bank.b, 20)
expect_equal(r5.read(), 0)
r5.write(0)
expect_log(r5.write, [17], obj.bank.b, 'spec-viol')
expect_equal(r5.read(), 0)

r6 = du.Register_LE(obj.bank.b, 24)
expect_equal(r6.read(), 7)
r6.write(7)
expect_log(r6.write, [11], obj.bank.b, 'spec-viol')
expect_equal(r6.read(), 7)

r7 = du.Register_LE(obj.bank.b, 28)
expect_equal(r7.read(), 7)
r7.write(7)
expect_log(r7.write, [11], obj.bank.b, 'spec-viol')
expect_equal(r7.read(), 7)
