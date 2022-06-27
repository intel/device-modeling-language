# © 2021-2022 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest
import dev_util

# bank_io_memory works
stest.expect_equal(dev_util.Register_LE((obj, 'bare', 0), size=1).read(), 0xaa)
# function_io_memory works ..
stest.expect_equal(dev_util.Register_LE((obj, 0xb, 0), size=1).read(), 0xbb)
stest.expect_equal(dev_util.Register_LE((obj, 0xc, 0), size=1).read(), 0xcc)
stest.expect_equal(dev_util.Register_LE((obj, 0x10, 0), size=1).read(), 0x0)
stest.expect_equal(dev_util.Register_LE((obj, 0x11, 0), size=1).read(), 0x1)
stest.expect_equal(dev_util.Register_LE((obj, 0x12, 0), size=1).read(), 0x10)
stest.expect_equal(dev_util.Register_LE((obj, 0x13, 0), size=1).read(), 0x11)
# .. and incorrect function numbers are handled somewhat gracefully
with stest.expect_log_mgr(obj, 'error'), stest.expect_exception_mgr(
        dev_util.MemoryError):
    dev_util.Register_LE((obj, 0xd, 0), size=1).read()

# io_memory_access
e = obj.bank.e
r = dev_util.Register_LE(e, 3, size=4)
miss = dev_util.Register_LE(e, 13, size=4)
stest.expect_equal((r.read(), e.read_offset, e.read_mask, e.read_aux),
                   (4711, 3, 0xffffffff, 1234))
with stest.expect_exception_mgr(dev_util.MemoryError):
    miss.read()

r.read_transaction.inquiry = True
stest.expect_equal((r.read(), e.get_offset, e.get_size), (4711, 3, 4))
miss.read_transaction.inquiry = True
with stest.expect_exception_mgr(dev_util.MemoryError):
    miss.read()

r.write(0xdeadbeef)
stest.expect_equal((e.write_offset, e.write_value, e.write_mask, e.write_aux),
                   (3, 0xdeadbeef, 0xffffffff, 1234))
with stest.expect_exception_mgr(dev_util.MemoryError):
    miss.write(5)

r.write_transaction.inquiry = True
r.write(0x4321)
stest.expect_equal((e.set_offset, e.set_value, e.set_size), (3, 0x4321, 4))
miss.write_transaction.inquiry = True
with stest.expect_exception_mgr(dev_util.MemoryError):
    miss.write()
