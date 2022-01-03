# © 2021-2022 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

from dml import ast

import unittest

def apply(f):
    return f()

class TestAST(unittest.TestCase):
    def test(self):
        @apply
        class site(object): pass
        @apply
        class arg1(object): pass
        @apply
        class arg2(object): pass
        self.assertEqual(ast.if_(site).kind, 'if')
        self.assertEqual(ast.log(site).kind, 'log')
        log = ast.log(site, arg1, arg2)
        self.assertEqual(log.kind, 'log')
        self.assertEqual(log.site, site)
        self.assertEqual(log.args, (arg1, arg2))
        self.assertEqual(log[0], 'log')
        self.assertEqual(log[1], site)
        self.assertEqual(log[2], arg1)
        self.assertEqual(log[3], arg2)
        self.assertEqual(log[0:3], ['log', site, arg1])

class TestDispatch(unittest.TestCase):
    def setUp(self):
        self.disp = ast.astdispatcher('test_')
        @self.disp
        def test_after(tree, x):
            return x + tree.args[0]
        @self.disp
        def test_member(tree, x):
            return -x - tree.args[0]
        self.site = None
    def test_fnerr(self):
        def foo_bar(x):
            return x
        self.assertRaises(ast.ASTDispatchFnError,
                          self.disp, foo_bar)
    def test_miss(self):
        self.assertRaises(KeyError,
                          self.disp.dispatch, ast.throw(self.site))
    def test_hit(self):
        self.assertEqual(self.disp.dispatch(ast.after(self.site, 2), 17), 19)
        self.assertEqual(self.disp.dispatch(ast.member(self.site, 3), 17), -20)
