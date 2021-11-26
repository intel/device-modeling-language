# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

from stest import *
untrap_log("spec-viol")

mem = SIM_create_object("memory-space", "mem",
                        [["map", [[0, obj, 0, 0, 0x10000000, None, 0, 8192]]]])

try:
    mem.iface.memory_space.read(None, 0, 4, 0)
    raise TestFailure("no exception")
except SimExc_Memory:
    pass

data = mem.iface.memory_space.read(None, 16, 4, 0)
expect_equal(data, (0x22,0x22,0x22,0x22))

data = mem.iface.memory_space.read(None, 40, 4, 0)
expect_equal(data, (0x22,0x22,0x22,0x22))
