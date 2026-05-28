# © 2026 Intel Corporation
# SPDX-License-Identifier: MPL-2.0
import simics
import stest
import conf

obj1 = simics.pre_conf_object('obj1', 'test')
obj2 = simics.pre_conf_object('obj2', 'test')
obj1.partner = obj2
obj2.partner = obj1

simics.SIM_add_configuration([obj1, obj2], None)

stest.expect_true(conf.obj1.objects_finalized_done)
stest.expect_true(conf.obj1.g_objects_finalized_done)
stest.expect_true(conf.obj1.s.objects_finalized_done)
stest.expect_true(conf.obj2.objects_finalized_done)
stest.expect_true(conf.obj2.g_objects_finalized_done)
stest.expect_true(conf.obj2.s.objects_finalized_done)
