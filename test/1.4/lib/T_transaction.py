# © 2021-2022 Intel Corporation
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
    stest.expect_equal(b.iface.transaction.issue(trans, 13),
                       Sim_PE_IO_Not_Taken)

    trans = transaction_t(
        write=True, inquiry=True, data=int.to_bytes(4712, 3, endian))
    stest.expect_equal((b.iface.transaction.issue(trans, 5),
                        b.set_offset, b.set_value, b.set_size),
                       (Sim_PE_No_Exception, 5, 4712, 3))
    stest.expect_equal(b.iface.transaction.issue(trans, 13),
                       Sim_PE_No_Exception)

    exp_offsets = [17, 24, 32, 0]
    exp_chunks = [(0, 7), (7, 15), (15, 20)]
    exp_multi_value = [0x6050403020100, 0xe0d0c0b0a090807, 0x131211100f, 0]
    data = b"".join(int.to_bytes(v, sz, endian)
                    for (v, sz) in zip(exp_multi_value, (7, 8, 5)))
    exp_value = exp_multi_value[2]

    trans = transaction_t(write=True, inquiry=True, data=data)
    stest.expect_equal((b.iface.transaction.issue(trans, 17),
                        b.set_offset, b.set_value, b.set_size),
                       (Sim_PE_No_Exception, 32, exp_value, 5))
    stest.expect_equal(b.set_multi_value, exp_multi_value)
    stest.expect_equal(b.set_multi_offset, exp_offsets)

    trans = transaction_t(write=True, data=data)
    stest.expect_equal((b.iface.transaction.issue(trans, 17),
                        b.write_offset, b.write_value),
                       (Sim_PE_No_Exception, 32, exp_value))
    stest.expect_equal(b.write_multi_value, exp_multi_value)
    stest.expect_equal(b.write_multi_offset, exp_offsets)

    trans = transaction_t(read=True, inquiry=True, size=20)
    stest.expect_equal((b.iface.transaction.issue(trans, 17),
                        b.get_offset, b.get_size),
                       (Sim_PE_No_Exception, 32, 5))
    stest.expect_equal(b.get_multi_offset, exp_offsets)
    for x, y in exp_chunks:
        stest.expect_equal(int.from_bytes(trans.data[x:y], endian), 4711)

    trans = transaction_t(read=True, size=20)
    stest.expect_equal((b.iface.transaction.issue(trans, 17), b.read_offset),
                       (Sim_PE_No_Exception, 32))
    stest.expect_equal(b.read_multi_offset, exp_offsets)
    for x, y in exp_chunks:
        stest.expect_equal(int.from_bytes(trans.data[x:y], endian), 4711)
