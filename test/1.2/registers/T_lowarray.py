# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import simics
import stest
stest.untrap_log("spec-viol")

mem = simics.SIM_create_object("memory-space", "mem",
                        [["map", [[0, obj, 0, 0, 0x10000000, None, 0, 8192]]]])

try:
    mem.iface.memory_space.read(None, 0, 4, 0)
    raise stest.TestFailure("no exception")
except simics.SimExc_Memory:
    pass

data = mem.iface.memory_space.read(None, 16, 4, 0)
stest.expect_equal(data, (0x22,0x22,0x22,0x22))

data = mem.iface.memory_space.read(None, 40, 4, 0)
stest.expect_equal(data, (0x22,0x22,0x22,0x22))
