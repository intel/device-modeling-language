# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import simics
import stest
import testenv
obj = testenv.instantiate()

stest.expect_equal(obj.class_desc, 'short desc 1')
stest.expect_equal(simics.VT_get_class_description(simics.SIM_object_class(obj)),
                   'short desc 1')
port = simics.SIM_object_descendant(obj, 'bank.descb')
stest.expect_equal(port.class_desc, 'short desc 2')
stest.expect_equal(simics.VT_get_class_description(simics.SIM_object_class(port)),
                   'short desc 2')
port = simics.SIM_object_descendant(obj, 'port.descp')
stest.expect_equal(port.class_desc, 'short desc 3')
stest.expect_equal(simics.VT_get_class_description(simics.SIM_object_class(port)),
                   'short desc 3')
for portname in ['port.nodescp', 'bank.nodescb']:
    port = simics.SIM_object_descendant(obj, portname)
    stest.expect_equal(port.class_desc, None)
    stest.expect_equal(simics.VT_get_class_description(simics.SIM_object_class(port)), None)
