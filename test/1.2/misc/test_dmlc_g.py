# © 2021-2022 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import os
import pickle
import stest

def test_gfile(gfile):
    # test flags
    (t_device, t_data, t_attribute, t_parameter, 
     t_method, t_event, t_bank, t_group, t_register, 
     t_field, t_port, t_implement, t_connect, t_interface) = (False,) * 14

    (_, _, _, dev) = (pickle.load(gfile),
                      pickle.load(gfile),
                      pickle.load(gfile),
                      pickle.load(gfile))

    (_, dev_name, _, subs) = dev
    t_device = dev_name == 'test'

    for i in subs:
        if i[1] == 'd':
            t_data = True
            t_data_type = i[2]
        if i[1] == 'int_attr':
            t_attribute = True
            for j in i[3]:
                if j[1] == 'allocate_type':
                    t_parameter = True
                    t_parameter_val = j[2]
                if j[1] == 'after_set':
                    t_method = True
        if i[1] == 'p_irq':
            t_port = True
            for j in i[3]:
                if j[1] == 'sample':
                    t_implement = True
        if i[1] == 'reg':
            t_bank = True
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

    for flag in (t_device, t_data, t_attribute, t_parameter, 
                 t_method, t_event, t_bank, t_group, t_register, 
                 t_field, t_port, t_implement, t_connect, t_interface):
        stest.expect_true(flag)

    stest.expect_equal(t_data_type, 'float')
    stest.expect_equal(t_parameter_val, b'int32')
    stest.expect_equal(t_bank_len, 2)

filename = [f for f in os.listdir(os.getcwd()) if f.endswith('.g')][0]
test_gfile(open(filename, 'rb'))
print("Test done.")
