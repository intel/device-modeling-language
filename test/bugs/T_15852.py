# © 2021-2024 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest
# Test that all banks get the int_register interface, no matter if
# they are mappable or not
for bank in ('b1', 'b2', 'b3'):
    print(SIM_get_port_interface(obj, 'int_register', bank))
stest.expect_true(obj.runtest)
