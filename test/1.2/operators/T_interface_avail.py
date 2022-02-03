# © 2021-2022 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

cpu = SIM_create_object("clock", "cpu", [["freq_mhz", 1]])

failures = 0

print("running test1")
if obj.test1:
    print("... success")
else:
    print("... failure")
    failures += 1

obj.o1 = cpu
obj.o2 = cpu

print("running test2")
if obj.test2:
    print("... success")
else:
    print("... failure")
    failures += 1

obj.o1 = cpu
obj.o2 = obj

print("running test3")
if obj.test3:
    print("... success")
else:
    print("... failure")
    failures += 1

if failures:
    SIM_quit(1)

