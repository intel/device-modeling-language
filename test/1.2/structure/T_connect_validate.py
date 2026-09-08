# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import simics
import testenv
import conf
obj = testenv.instantiate()
conf.sim.stop_on_error = False
obj.foo = conf.sim
try:
    obj.foo = obj
    print("*** Failed to detect wrong object")
    simics.SIM_quit(1)
except simics.SimExc_IllegalValue:
    pass
