# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import simics
import testenv
import conf
obj = testenv.instantiate()
conf.sim.stop_on_error = False
try:
    obj.not_zero = 0
except simics.CriticalErrors as e:
    assert len(e.errors) == 4
assert obj.not_zero == 5
try:
    obj.not_negative = -1
except simics.CriticalErrors as e:
    assert len(e.errors) == 2
assert obj.not_negative == 5
