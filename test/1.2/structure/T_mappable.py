# © 2021-2024 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest
print(SIM_get_port_interface(obj, "io_memory", "b1"))

try:
    print(SIM_get_port_interface(obj, "io_memory", "b2"))
    print("b2 is mappable")
    SIM_quit(1)
except SimExc_Lookup:
    pass

print(SIM_get_port_interface(obj, "io_memory", "b3"))
stest.expect_true(obj.runtest)
