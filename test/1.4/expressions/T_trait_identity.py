# © 2023 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import simics
import stest
conf.sim.stop_on_error = False

try:
    obj.test_bad_eq = None
except simics.CriticalErrors as e:
    stest.expect_true('object identity equality check' in str(e))
else:
    stest.fail('expected critical error')
