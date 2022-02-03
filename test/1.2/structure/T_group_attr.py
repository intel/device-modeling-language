# © 2021-2022 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

print(obj.b_ga_a)

if obj.b_ga_a == [[[i*100 + j*10 + k for k in range(3)]
                     for j in range(3)]
                    for i in range(4)]:
    print("OK")
else:
    print("Incorrect value")
    SIM_quit(1)
