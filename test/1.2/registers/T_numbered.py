# © 2021-2022 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest

def check_reg(bankname, name, regnum, val):
    iface = SIM_get_port_interface(obj, "int_register", bankname)

    if name:
        qname = bankname + "." + name
        i = iface.get_number(name)
        stest.expect_equal(i, regnum, "Index of register %s" % (qname,))

        n = iface.get_name(i)
        stest.expect_equal(n, name)
    else:
        i = regnum
        qname = bankname + "." + "<anon>"

    if bankname == "rw":
        iface.write(i, val)

    v = iface.read(i)
    stest.expect_equal(v, val, "Value of register %s" % (qname,))

check_reg("ro", "r1", 3, 1)
check_reg("ro", "r2", 5, 2)
check_reg("ro", "r3[0]", 9, 3)
check_reg("ro", "r3[1]", 8, 3)
check_reg("ro", "r3[2]", 7, 3)
check_reg("ro", "r3[3]", 6, 3)
check_reg("ro", "g1[0].r4", 10, 4)
check_reg("ro", "g1[1].r4", 11, 4)
check_reg("ro", "g1[2].r4", 12, 4)
check_reg("ro", "g1[2].r17[4]", 124, 19)
check_reg("ro", "g1[3].r4", 13, 4)
check_reg("ro", "g2.r1", 14, 5)
check_reg("ro", "r[0][0]", 16, 16)
check_reg("ro", "r[0][1]", 17, 16)
check_reg("ro", "r[1][0]", 18, 16)
check_reg("ro", "r[1][1]", 19, 16)
check_reg("ro", None, 7, 3)

check_reg("rw", "r1", 3, 1)
check_reg("rw", "r2", 5, 2)
check_reg("rw", "r3[0]", 6, 3)
check_reg("rw", "r3[1]", 7, 3)
check_reg("rw", "r3[2]", 8, 3)
check_reg("rw", "r3[3]", 9, 3)
check_reg("rw", "g1[0].r4", 10, 4)
check_reg("rw", "g1[1].r4", 11, 4)
check_reg("rw", "g1[2].r4", 12, 4)
check_reg("rw", "g1[3].r4", 13, 4)
check_reg("rw", "g1[2].h[1].r[1]", 27, 13)
check_reg("rw", "g2.r1", 14, 5)
check_reg("rw", "r[0][0]", 32, 15)
check_reg("rw", "r[0][1]", 33, 15)
check_reg("rw", "r[1][0]", 34, 15)
check_reg("rw", "r[1][1]", 35, 15)
check_reg("rw", None, 7, 3)

stest.expect_equal(obj.ports.hole.int_register.all_registers(), [0, 2])
