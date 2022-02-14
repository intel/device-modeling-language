# © 2022 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest

startup_calls = {'a': 0, 'b': 0}

def on_startup(name):
    global startup_calls
    startup_calls[name] += 1

stest.expect_equal(startup_calls, {'a': 0, 'b': 0})
SIM_load_module('dml-test-startup')
stest.expect_equal(startup_calls, {'a': 1, 'b': 4})
obj = SIM_create_object('test', 'obj', [])
stest.expect_equal(startup_calls, {'a': 1, 'b': 4})
