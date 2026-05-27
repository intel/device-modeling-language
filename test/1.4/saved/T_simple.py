# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import simics
import stest

# check the initial values where applicable
stest.expect_equal(obj.DML_saved_saved_initialized_int, 5)
stest.expect_equal(obj.DML_saved_p_saved_initialized_int, 6)

obj.modify_all_saved = 1
# sanity
obj.verify_all_saved = 1

# take checkpoint
simics.SIM_write_configuration_to_file("init.ckpt", 0)

# change values
obj.modify_all_saved = 3
# sanity
obj.verify_all_saved = 3

simics.SIM_delete_objects(simics.SIM_get_all_objects())

# load checkpoint, verify values reset
simics.SIM_read_configuration("init.ckpt")
# plain 'obj' reference is dead here
conf.obj.verify_all_saved = 1
