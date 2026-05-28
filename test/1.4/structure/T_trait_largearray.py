# © 2022 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import simics
from simicsutils.host import is_windows
if is_windows():
    # resource module only available on linux
    simics.SIM_create_object('test', 'obj', [])
    exit(0)

import resource
import stest

before = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
simics.SIM_create_object('test', 'obj', [])
after = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
print(after - before)
stest.expect_true(after - before < 1024)
