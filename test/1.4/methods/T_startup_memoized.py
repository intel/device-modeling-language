# © 2022-2024 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import itertools
import stest

methods = [f'{prefix}memo_{suffix}' for (prefix, suffix)
           in itertools.product(['', 's_'],
                                ['0_t', '0_nt', '1', '2', '2_t', '2_nt'])]

startup_calls = {(node, meth): 0 for (node, meth)
                 in itertools.product(['test', 'c'], methods)}
expected_after_startup = {(node, meth): expected for ((node, expected), meth)
                          in itertools.product([('test', 1), ('c', 2*3*5)],
                                               methods)}
def on_startup(node, meth):
    global startup_calls
    startup_calls[(node, meth)] += 1

SIM_load_module('dml-test-startup_memoized')
stest.expect_equal(startup_calls, expected_after_startup)
obj = SIM_create_object('test', 'obj', [])
stest.expect_equal(startup_calls, expected_after_startup)
