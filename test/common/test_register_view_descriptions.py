# © 2021-2023 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

# This test makes sure we always return the same documentation of DML
# objects through the register-view interface, irrespective of DML
# version and what documentation parameter is used

from simics import SIM_get_port_interface
from stest import expect_equal

def register_view(obj, bank):
    return SIM_get_port_interface(obj, 'register_view', bank)

def test(obj):
    def bank_desc(bank):
        return register_view(obj, bank).description()

    expect_equal(bank_desc('b_documentation'), '')
    expect_equal(bank_desc('b_desc_documentation'), '<i>desc</i>')

    def r_desc(bank):
        (_, desc, _, _) = register_view(obj, bank).register_info(0)[:4]
        return desc

    expect_equal(r_desc('r_documentation'), '')
    expect_equal(r_desc('r_desc_documentation'), '<i>desc</i>')

    def f_desc(bank):
        bitfields = register_view(obj, bank).register_info(0)[4]
        (_, desc, _, _) = bitfields[0]
        return desc

    expect_equal(f_desc('f_documentation'), '')
    expect_equal(f_desc('f_desc_documentation'), '<i>desc</i>')
