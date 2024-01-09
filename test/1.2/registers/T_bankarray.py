# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import dev_util
from stest import expect_equal

mem = SIM_create_object("memory-space", "mem",
                        [["map", 
                          [[0x0,   [obj, "regs[0]"], 0, 0, 0x100],
                           [0x100, [obj, "regs[1]"], 0, 0, 0x100],
                           [0x200, obj, 3, 0, 0x100],
                           [0x300, obj, 4, 0, 0x100]]]])

def rd(offset):
    return dev_util.tuple_to_value_le(
        mem.iface.memory_space.read(None, offset, 4, 0))

def wr(offset, value):
    mem.iface.memory_space.write(None, offset,
                                 dev_util.value_to_tuple_le(value, 4), 0)


expect_equal(rd(0), 0xbadc0ffe)
expect_equal(rd(0x100), 0xdeadbeef)
expect_equal(rd(0x200), 0xbadc0ffe)
expect_equal(rd(0x300), 0xdeadbeef)

obj = conf.obj
obj.regs_ra = [[[0x10, 0x14], [0x18, 0x1c]], [[0x110, 0x114], [0x118, 0x11c]]]
obj.regs_g_gr = [[0x40, 0x54], [0x140, 0x154]]
obj.regs_g_gra = [[[[0x44, 0x48], [0x4c, 0x50]],
                   [[0x58, 0x5c], [0x60, 0x64]]],
                  [[[0x144, 0x148], [0x14c, 0x150]],
                   [[0x158, 0x15c], [0x160, 0x164]]]]
obj.log_level = 4
for b in (0, 1):
    for o in (0x10, 0x14, 0x18, 0x1c, 0x40, 0x54,
              0x44, 0x48, 0x4c, 0x50, 0x58, 0x5c, 0x60, 0x64):
        r = o + b*0x100
        expect_equal(rd(r), r)
