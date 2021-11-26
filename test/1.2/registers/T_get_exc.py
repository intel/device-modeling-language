# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

try:
    print(obj.b_r)
    print("attribute get didn't fail as it should have")
    SIM_quit(1)
except Exception as e:
    print(e)
