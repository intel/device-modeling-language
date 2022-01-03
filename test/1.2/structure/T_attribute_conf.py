# © 2021-2022 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest

obj = SIM_create_object('test', 'obj', [['b_a', 5]])

# Check that persistent attributes work

obj.persist = 17

CORE_write_configuration_persistent("persistent.conf", None, Sim_Save_Nobundle)

config = VT_get_configuration("persistent.conf")
print(config)

pobj = config.get('obj')
stest.expect_true(pobj, msg="No persistent state for obj")
stest.expect_equal(pobj.persist, 17, msg="Persistent value was wrong")
