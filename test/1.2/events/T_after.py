# © 2021-2024 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

cpu = SIM_create_object("clock", "clock", [["freq_mhz", 1]])
obj.queue = cpu
obj.alttest = 1
