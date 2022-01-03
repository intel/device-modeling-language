# © 2021-2022 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

obj.s = "teststring"
if obj.s != "teststring":
    print("Failed to set and get string attribute")
    SIM_quit(1)
