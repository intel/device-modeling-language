# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import unittest
import operator
import itertools

from dml.reginfo import RegInstance, RegInfo, explode_register
from dml.objects import Device, Bank, Group, Register
from dml.logging import SimpleSite
from dml.symtab import global_scope

class TestRegInfo(unittest.TestCase):
    def reg_array(self, bankdim, regdims):
        site = SimpleSite('nowhere')
        # the device's members are added to the global scope
        global_scope.symdict.clear()
        global_scope.symlist.clear()
        group = Bank('b', site, Device('d', site), bankdim, ('i'))
        for dimsizes in regdims:
            group = Group('g', site, group, dimsizes, 'i')
        return Register('r', site, group, (), ())

    def test_complete(self):
        '''Test RegInfo.node_instances() for a fully populated register array'''
        for bankdim in ((), (1,), (2,)):
            for regdims in ((), ((1,),), ((3,), (2,)), ((3, 2),)):
                flattened_dims = tuple(rd for regdim in regdims
                                       for rd in regdim)
                reg = self.reg_array(bankdim, regdims)
                instances = [RegInstance(coord, None, 1)
                             for coord in itertools.product(
                                     *list(map(range, flattened_dims)))]
                ri = RegInfo(reg, instances)
                self.assertEqual(
                    list(ri.node_instances()), [(reg, (), flattened_dims)])

    def test_sparse(self):
        '''Test RegInfo.node_instances() for a partially populated register
        array'''
        for bankdim in ((), (2,), (2, 3)):
            regdims = ((6, 7),)
            reg = self.reg_array(bankdim, regdims)
            ri = RegInfo(reg, [RegInstance(dims, None, 1)
                               for dims in [(3, 1), (4, 5)]]).node_instances()
            self.assertEqual(
                [(o, tuple(e.value for e in indices), dimsizes)
                 for (o, indices, dimsizes) in ri],
                [(reg, (3, 1), ()),
                 (reg, (4, 5), ())])
