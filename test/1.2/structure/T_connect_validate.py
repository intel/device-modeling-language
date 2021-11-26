# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

conf.sim.fatal_error_messages = False
obj.foo = conf.sim
try:
    obj.foo = obj
    print("*** Failed to detect wrong object")
    SIM_quit(1)
except SimExc_IllegalValue:
    pass
