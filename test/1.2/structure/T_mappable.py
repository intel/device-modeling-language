# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import simics
import stest
print(simics.SIM_get_port_interface(obj, "io_memory", "b1"))

try:
    print(simics.SIM_get_port_interface(obj, "io_memory", "b2"))
    print("b2 is mappable")
    simics.SIM_quit(1)
except simics.SimExc_Lookup:
    pass

print(simics.SIM_get_port_interface(obj, "io_memory", "b3"))
stest.expect_true(obj.runtest)
