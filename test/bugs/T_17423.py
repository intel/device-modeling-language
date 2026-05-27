# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import simics
obj.log_level = 4
print(obj)
cpu = simics.SIM_create_object("clock", "cpu", [["freq_mhz", 1]])
mem = simics.SIM_create_object("memory-space", "mem",
                        [["map", [[0, [obj, 'bank0'], 0, 0, 0x10]]]])

reg_vals = (0xfa1afe1, 0xbabe, 0xabba)
obj.bank0_REG1 = reg_vals[0]
obj.bank0_REG2 = reg_vals[1]
obj.bank0_REG3 = reg_vals[2]

try:
    mem.iface.memory_space.write(cpu, 0, (0 , 1), 0)
    raise Exception("expected nothing-is-mapped error")
except simics.SimExc_Memory:
    if (obj.bank0_REG1 != reg_vals[0] or obj.bank0_REG2 != reg_vals[1] 
        or obj.bank0_REG3 != reg_vals[2]):
        raise Exception("expect registers 1-3 to be unchanged")

