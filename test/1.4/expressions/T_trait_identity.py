# © 2023 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import simics
import stest
conf.sim.fail_on_warnings = False

try:
    obj.test_bad_eq = None
except simics.CriticalErrors as e:
    stest.expect_true('object identity equality check' in str(e))
else:
    stest.fail('expected critical error')
