# © 2021-2022 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest

model_init_calls = {'dev': 0, 'dev.g': 0}

def on_model_init(name):
    global model_init_calls
    model_init_calls[name] += 1

stest.expect_equal(model_init_calls['dev'], 0)
stest.expect_equal(model_init_calls['dev.g'], 0)
SIM_load_module('dml-test-model_init')
stest.expect_equal(model_init_calls['dev'], 1)
stest.expect_equal(model_init_calls['dev.g'], 1)
obj = SIM_create_object('test', 'obj', [])
stest.expect_equal(model_init_calls['dev'], 1)
stest.expect_equal(model_init_calls['dev.g'], 1)
