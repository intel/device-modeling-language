# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

from simics_common import CriticalErrors
import stest
conf.sim.stop_on_error = False
try:
    SIM_load_module(f'dml-test-bad_subobj_connect')
except CriticalErrors as e:
    stest.expect_true('garbage' in str(e))
else:
    stest.fail('expected critical error')
