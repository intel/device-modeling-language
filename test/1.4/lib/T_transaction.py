# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

from simics import Sim_PE_No_Exception
import stest


for b, endian in ((obj.bank.b, 'little'), (obj.bank.be, 'big')):
    trans = transaction_t(read=True, size=4)
    stest.expect_equal((b.iface.transaction.issue(trans, 3), b.read_offset,
                        trans.value_be if endian == 'big' else trans.value_le,
                        b.read_mask, b.read_aux),
                       (Sim_PE_No_Exception, 3, 4711, 0xffffffff, 1234),
                       trans.data)
    with stest.expect_log_mgr(obj, 'spec-viol'):
        stest.expect_equal(b.iface.transaction.issue(trans, 13),
                           Sim_PE_IO_Not_Taken)

    trans = transaction_t(read=True, inquiry=True, size=4)
    stest.expect_equal((b.iface.transaction.issue(trans, 3), b.get_offset,
                        trans.value_be if endian == 'big' else trans.value_le,
                        b.get_size),
                       (Sim_PE_No_Exception, 3, 4711, 4,))
    stest.expect_equal(b.iface.transaction.issue(trans, 13),
                       Sim_PE_IO_Not_Taken)

    trans = transaction_t(write=True, data=int.to_bytes(4712, 3, endian))
    stest.expect_equal((b.iface.transaction.issue(trans, 5),
                        b.write_offset, b.write_value, b.write_mask,
                        b.write_aux),
                       (Sim_PE_No_Exception, 5, 4712, 0xffffff, 1234))
    with stest.expect_log_mgr(obj, 'spec-viol'):
        stest.expect_equal(b.iface.transaction.issue(trans, 13),
                           Sim_PE_IO_Not_Taken)

    trans = transaction_t(
        write=True, inquiry=True, data=int.to_bytes(4712, 3, endian))
    stest.expect_equal((b.iface.transaction.issue(trans, 5),
                        b.set_offset, b.set_value, b.set_size),
                       (Sim_PE_No_Exception, 5, 4712, 3))
    stest.expect_equal(b.iface.transaction.issue(trans, 13),
                       Sim_PE_No_Exception)
