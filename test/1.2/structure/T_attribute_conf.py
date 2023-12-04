# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest
import simicsutils

obj = SIM_create_object('test', 'obj', [['b1_a', 5], ['b2_a', [0,1,2,3]]])

stest.expect_equal(obj.bank.b1.a, 5)
stest.expect_equal([obj.bank.b2[i].a for i in range(4)], list(range(4)))

# Check that persistent attributes work

obj.persist = 17

if simicsutils.internal.get_simics_major() == "6":
    CORE_write_configuration_persistent("persistent.conf", None, Sim_Save_Nobundle)
else:
    SIM_write_persistent_state("persistent.conf", None, Sim_Save_Nobundle)

config = VT_get_configuration("persistent.conf")
print(config)

pobj = config.get('obj')
stest.expect_true(pobj, msg="No persistent state for obj")
stest.expect_equal(pobj.persist, 17, msg="Persistent value was wrong")
