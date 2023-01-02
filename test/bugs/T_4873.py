# © 2021-2023 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

mem = SIM_create_object("memory-space", "mem",
                        [["map", [[0, obj, 0, 0, 0x10000, None, 0, 8192]]]])

obj.log_level = 4

# Fill the memory with some kind of pattern.
mem.iface.memory_space.write(None, 0, (1,2,3,4), 0)
mem.iface.memory_space.write(None, 4, (5,6,7,8), 0)
mem.iface.memory_space.write(None, 8, (9,10,11,12), 0)

# Read the registers
r1 = mem.iface.memory_space.read(None, 0, 4, 0)
r2 = mem.iface.memory_space.read(None, 4, 4, 0)
r3 = mem.iface.memory_space.read(None, 8, 4, 0)

print("r1 = %s" % (r1,))
print("r2 = %s" % (r2,))
print("r3 = %s" % (r3,))

# Not all parts of the FIR register are implemented
if r1 + r2 + r3 != (1,2,3,4,5,6,7,8,9,10,11,12):
    print("Wrong registers values")
    SIM_quit(1)

# Now read an overlapping range
try:
    r1r2 = mem.iface.memory_space.read(None, 2, 4, 0)
    print("Didn't get expected exception when reading r1r2")
    print("r1r2 = %s" % (r1r2,))
    SIM_quit(1)
except SimExc_Memory:
    print("Got expected exception when reading r1r2")

try:
    r2r3 = mem.iface.memory_space.read(None, 6, 4, 0)
    print("Didn't get expected exception when reading r2r3")
    print("r1r2 = %s" % (r2r3,))
    SIM_quit(1)
except SimExc_Memory:
    print("Got expected exception when reading r2r3")
