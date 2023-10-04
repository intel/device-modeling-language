# © 2023 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest

cpu = SIM_create_object("clock", "clock", [["freq_mhz", 1]])
obj.queue = cpu

obj.trigger = None
SIM_continue(99999)
SIM_continue(2)
