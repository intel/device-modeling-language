# © 2021-2023 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import unittest
import operator
import itertools

from dml.reginfo import RegInstance, RegInfo, explode_register
from dml.objects import Device, Bank, Group, Register

class TestRegInfo(unittest.TestCase):
    def reg_array(self, bankdim, regdims):
        group = Bank('b', None, Device('d', None), bankdim, ('i'))
        for dimsizes in regdims:
            group = Group('g', None, group, dimsizes, 'i')
        return Register('r', None, group, (), ())

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
