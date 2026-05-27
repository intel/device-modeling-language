# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

# We need to create a second object, since run-test.py expects obj to
# remain after executing this script.

import simics
cpu = simics.SIM_create_object("clock", "clock", [["freq_mhz", 1]])

o = simics.SIM_create_object("test", "o", [["queue", cpu]])
print("Deleting")
simics.SIM_delete_object(o)
print("Done")
simics.SIM_continue(10000000)
print("Events not triggered")
