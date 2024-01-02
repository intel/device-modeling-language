# © 2021-2024 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest

stest.expect_equal(obj.class_desc, 'short desc 1')
stest.expect_equal(VT_get_class_description(SIM_object_class(obj)),
                   'short desc 1')
port = SIM_object_descendant(obj, 'bank.descb')
stest.expect_equal(port.class_desc, 'short desc 2')
stest.expect_equal(VT_get_class_description(SIM_object_class(port)),
                   'short desc 2')
port = SIM_object_descendant(obj, 'port.descp')
stest.expect_equal(port.class_desc, 'short desc 3')
stest.expect_equal(VT_get_class_description(SIM_object_class(port)),
                   'short desc 3')
for portname in ['port.nodescp', 'bank.nodescb']:
    port = SIM_object_descendant(obj, portname)
    stest.expect_equal(port.class_desc, None)
    stest.expect_equal(VT_get_class_description(SIM_object_class(port)), None)
