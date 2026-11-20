# © 2026 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import unittest

from dml.logging import (SimpleSite, TemplateSite, InEachSite,
                         expansion_chains)
from dml.template import Rank, RankDesc

def lowest_rank():
    '''A rank with no inferior ranks; only its identity matters'''
    return Rank(set(), RankDesc('verbatim', 'dummy'))

class test_expansion_chains(unittest.TestCase):
    def setUp(self):
        self.dev_rank = lowest_rank()
        self.a_rank = lowest_rank()

    def test_unexpanded(self):
        self.assertEqual(expansion_chains(SimpleSite('x')), [[]])
        self.assertEqual(expansion_chains(SimpleSite('x'), SimpleSite('y')),
                         [[], []])

    def test_linear(self):
        is_a = SimpleSite('is a')
        is_b = TemplateSite(SimpleSite('is b'), is_a, 'a', self.dev_rank)
        site = TemplateSite(SimpleSite('m'), is_b, 'b', self.a_rank)
        self.assertEqual(expansion_chains(site), [[
            (is_b, 'template b instantiated by template a'),
            (is_a, 'template a instantiated here')]])

    def test_same_rank_grouped(self):
        # 'is (a, b)' in the device; conflicting definitions in a and b
        is_a = SimpleSite('is a')
        is_b = SimpleSite('is b')
        site_a = TemplateSite(SimpleSite('a.m'), is_a, 'a', self.dev_rank)
        site_b = TemplateSite(SimpleSite('b.m'), is_b, 'b', self.dev_rank)
        self.assertEqual(expansion_chains(site_a, site_b), [
            [(is_a, 'templates a and b instantiated here')],
            []])

    def test_shared_tail(self):
        # a instantiated in the device; b and c both instantiated by a
        is_a = SimpleSite('is a')
        is_b = TemplateSite(SimpleSite('is b'), is_a, 'a', self.dev_rank)
        is_c = TemplateSite(SimpleSite('is c'), is_a, 'a', self.dev_rank)
        site_b = TemplateSite(SimpleSite('b.m'), is_b, 'b', self.a_rank)
        site_c = TemplateSite(SimpleSite('c.m'), is_c, 'c', self.a_rank)
        self.assertEqual(expansion_chains(site_b, site_c), [
            [(is_b, 'templates b and c instantiated by template a'),
             (is_a, 'template a instantiated here')],
            []])

    def test_in_each(self):
        # 'in each (x, y) { is d; }', where x is instantiated in the
        # device and y is instantiated by template a
        is_a = SimpleSite('is a')
        is_x = SimpleSite('is x')
        is_y = TemplateSite(SimpleSite('is y'), is_a, 'a', self.dev_rank)
        in_each = SimpleSite('in each')
        is_d = InEachSite(SimpleSite('is d'), in_each, ['x', 'y'],
                          [(is_x, self.dev_rank, 'x'),
                           (is_y, self.a_rank, 'y')])
        in_each_rank = lowest_rank()
        site = TemplateSite(SimpleSite('d.m'), is_d, 'd', in_each_rank)
        self.assertEqual(expansion_chains(site), [[
            (is_d, "template d instantiated by 'in each (x, y)' block"),
            (is_x, 'templates x and a instantiated here'),
            (is_y, 'template y instantiated by template a')]])

    def test_imported_files(self):
        # Definitions directly in imported files are not expanded
        self.assertEqual(expansion_chains(SimpleSite('a.dml:2'),
                                          SimpleSite('b.dml:2')),
                         [[], []])
        # 'is ta' in a.dml and 'is tb' in b.dml have different ranks
        a_file_rank = lowest_rank()
        b_file_rank = lowest_rank()
        is_ta = SimpleSite('a.dml:4')
        is_tb = SimpleSite('b.dml:4')
        site_a = TemplateSite(SimpleSite('ta.m'), is_ta, 'ta', a_file_rank)
        site_b = TemplateSite(SimpleSite('tb.m'), is_tb, 'tb', b_file_rank)
        self.assertEqual(expansion_chains(site_a, site_b), [
            [(is_ta, 'template ta instantiated here')],
            [(is_tb, 'template tb instantiated here')]])
