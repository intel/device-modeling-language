# © 2021-2022 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest

stest.expect_equal(obj.class_desc, None)
stest.expect_equal(VT_get_class_description(SIM_object_class(obj)),
                                            'long description')
port = SIM_object_descendant(obj, 'port.doc_only')
stest.expect_equal(port.class_desc, None)
stest.expect_equal(VT_get_class_description(SIM_object_class(port)),
                                            'long description 2')
port = SIM_object_descendant(obj, 'bank.doc_desc')
stest.expect_equal(port.class_desc, 'short desc')
stest.expect_equal(VT_get_class_description(SIM_object_class(port)),
                                            'long description 3')
