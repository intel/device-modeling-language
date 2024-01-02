# © 2021-2024 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import os
import pickle
import stest

def test_gfile(gfile):
    # test flags
    (t_device, t_attribute, t_method, t_event, t_group, t_register, t_field,
     t_port, t_implement, t_connect, t_interface) = (False,) * 11
    (t_session_type, t_bank_len, t_param_type, t_param_endian_int,
     t_param_traitref) = (None,)*5

    (_, _, _, dev) = [pickle.load(gfile) for _ in range(4)]

    (_, dev_name, _, subs) = dev
    t_device = dev_name == 'test'

    for i in subs:
        if i[1] == 'd':
            t_session_type = i[2]
        if i[1] == 'int_attr':
            t_attribute = True
            for j in i[3]:
                if j[1] == 'type':
                    t_param_type = j[2]
        if i[1] == 'p_irq':
            t_port = True
            for j in i[3]:
                if j[1] == 'sample':
                    t_implement = True
        if i[1] == 'reg':
            t_bank_len = i[2][0][0]
            for j in i[3]:
                if j[1] == 'x':
                    t_group = True
                if j[1] == 'r':
                    t_register = True
                    for k in j[3]:
                        if k[1] == 'EN':
                            t_field = True
        if i[1] == 'pic':
            t_connect = True
            for j in i[3]:
                if j[1] == 'sample':
                    t_interface = True
        if i[1] == 'ev_cycles':
            t_event = True
            for j in i[3]:
                if j[1] == 'event':
                    t_method = True
        if i[1] == 'endian_int':
            t_param_endian_int = i[2]
        if i[1] == 'traitref':
            t_param_traitref = i[2]

    for flag in (t_device, t_attribute, t_method, t_event, t_group, t_register,
                 t_field, t_port, t_implement, t_connect, t_interface):
        stest.expect_true(flag)

    stest.expect_equal(t_bank_len, 2)
    stest.expect_equal(t_session_type, 'double')
    stest.expect_equal(t_param_type, b'i')
    stest.expect_equal(t_param_endian_int, ('dml_store_int24_be_t(13)',))

    # Trait references are not implemented in dml-gdb (SIMICS-19506), so it's
    # ok for their encoded representation to be unusable.
    # What's critical is that the representation doesn't result in broken debug
    # files (SIMICS-20200).
    stest.expect_equal(t_param_traitref, ('reg[1].bank',))

filename = [f for f in os.listdir(os.getcwd()) if f.endswith('.g')][0]
test_gfile(open(filename, 'rb'))
