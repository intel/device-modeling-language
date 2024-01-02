# © 2021-2024 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest

cpu = SIM_create_object("clock", "clock", [["freq_mhz", 1]])
obj.queue = cpu

obj.post_all = None

[[ev_desc], [ev_no_desc], [ev_custom],
 [ev_array_0_0], [ev_array_0_1],
 [ev_array_1_0], [ev_array_1_1],
 [eva_0_0], [eva_0_1],
 [eva_1_0], [eva_1_1]] = [
    [desc for [_, _, cycle, desc] in cpu.iface.cycle.events()
     if cycle == i]
    for i in [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]]

stest.expect_equal(ev_desc, "from desc param")
stest.expect_equal(ev_no_desc, "ev_no_desc")
stest.expect_equal(ev_custom, "blurp")
stest.expect_equal(ev_array_0_0, "ev_array[0][0]")
stest.expect_equal(ev_array_0_1, "ev_array[0][1]")
stest.expect_equal(ev_array_1_0, "ev_array[1][0]")
stest.expect_equal(ev_array_1_1, "ev_array[1][1]")
stest.expect_equal(eva_0_0, "eva[0][0]")
stest.expect_equal(eva_0_1, "eva[0][1]")
stest.expect_equal(eva_1_0, "eva[1][0]")
stest.expect_equal(eva_1_1, "eva[1][1]")
