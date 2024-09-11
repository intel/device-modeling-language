# © 2024 Intel Corporation
# SPDX-License-Identifier: MPL-2.0
import stest

destroy_list = None
def on_destroyed(*invocations):
    global destroy_list
    stest.expect_equal(destroy_list, None)
    destroy_list = list(invocations)

cpu = SIM_create_object("clock", "clock", [["freq_mhz", 1]])
obj.queue = cpu
obj.post_ev = None

SIM_delete_object(obj)

stest.expect_equal(destroy_list, ["ev", "g2", "g1", "dev"])
