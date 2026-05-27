# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import simics
obj.s = "teststring"
if obj.s != "teststring":
    print("Failed to set and get string attribute")
    simics.SIM_quit(1)
