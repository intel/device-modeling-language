# © 2021-2023 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

obj.log_level = 4
cpu = SIM_create_object("clock", "cpu", [["freq_mhz", 1]])
mem = SIM_create_object("memory-space", "mem",
                        [["map", [[0, obj, 0, 0, 0x10]]]])
try:
    mem.iface.memory_space.write(cpu, 0, (1,2,3,4), 0)
    raise Exception("expected nothing-is-mapped error")
except SimExc_Memory:
    pass
