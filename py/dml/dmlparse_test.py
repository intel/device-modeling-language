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

class test_calls_from_empty_prod_rules(unittest.TestCase):
    def test(self):
        empty_prod_re = re.compile(r'[:|]\s*(?:$|\|)')
        assert empty_prod_re.search('foo : \n')
        assert not empty_prod_re.search('foo : something\n')

        bad_prod_rules = {}
        for rule in itertools.chain(dmlparse.production_rules_dml12.values(),
                                    dmlparse.production_rules_dml14.values()):
            if rule is not dmlparse.error:
                is_empty = empty_prod_re.search(rule.__doc__) is not None
                if is_empty != ('fixup_emptyprod_lexpos'
                                in rule.__code__.co_names):
                    bad_prod_rules[rule] = is_empty

        if bad_prod_rules:
            msg = "\n"
            for (rule, empty_prod) in bad_prod_rules.items():
                msg += ("empty production rule without call to "
                        if empty_prod else
                        "non-empty production rule with call to ")
                msg += f"'fixup_emptyprod_lexpos': {rule.__name__}\n"

            self.fail(msg)

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
