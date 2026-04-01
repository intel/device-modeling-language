# © 2026 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

from dml import dmlparse, toplevel, logging
import unittest
import re
import sys

empty_prod_re = re.compile(r'[:|]\s*(?:$|\|)')

bad_emptyprod_rules = set()
for rules in (dmlparse.production_rules_dml12,
              dmlparse.production_rules_dml14):
    for (name, rule) in rules.items():
        if (name != 'p_error' and empty_prod_re.search(rule.__doc__)
            and 'fixup_emptyprod_lexpos' not in rule.__code__.co_names):
            bad_emptyprod_rules.add(rule)

if bad_emptyprod_rules:
    sys.stderr.write(
        "empty production rule(s) without call to 'fixup_emptyprod_lexpos':\n")
    for rule in bad_emptyprod_rules:
        sys.stderr.write(f'    {rule.__name__}\n')

    sys.exit(1)

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
        # Without it, sites would be ruined by empty production rules.
        ast = parse('''
method m() {}
    group g;
'''.strip())
        self.assertEqual((ast.site.lineno, ast.site.colno), (1, 1))
        [_, stmts] = ast.args
        self.assertEqual([stmt.kind for stmt in stmts], ['method', 'object'])
        self.assertEqual((stmts[0].site.lineno, stmts[0].site.colno), (1, 1))
        self.assertEqual((stmts[1].site.lineno, stmts[1].site.colno), (2, 5))
