# © 2026 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

from dml import dmlparse, toplevel, logging
import itertools
import unittest
import re

def parse(contents):
    file_info = logging.FileInfo(
        '<unit test>', (1, 4), content_lines=contents.splitlines(
            keepends=True))
    ast = toplevel.parse(contents, file_info, file_info.name, (1, 4))
    assert ast.kind == 'dml', ast.kind
    return ast

class test_emptyprod_based_sites(unittest.TestCase):
    def test(self):
        # Test that sites are actually fixed up by fixup_emptyprod_lexpos.
        # Some sites would get ruined without it, like those of methods without
        # qualifiers and object declarations without the `in` syntax enabled by
        # `explicit_object_extensions`
        ast = parse('''
method m() {}
    group g;
'''.strip())
        self.assertEqual((ast.site.lineno, ast.site.colno), (1, 1))
        [_, stmts] = ast.args
        self.assertEqual([stmt.kind for stmt in stmts], ['method', 'object'])
        self.assertEqual((stmts[0].site.lineno, stmts[0].site.colno), (1, 1))
        self.assertEqual((stmts[1].site.lineno, stmts[1].site.colno), (2, 5))
