# © 2026 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import io
import unittest
from unittest.mock import patch

from dml import logging
from dml.logging import (ErrorContext, SimpleSite, TemplateSite, InEachSite,
                         ImportSite, expansion_chains)
from dml.objects import Device, Group

class test_expansion_chains(unittest.TestCase):
    def test_unexpanded(self):
        self.assertEqual(expansion_chains(SimpleSite('x.dml:1'),
                                          SimpleSite('y.dml:1')),
                         [[], []])

    def test_linear(self):
        is_a = SimpleSite('dev.dml:1')
        is_b = TemplateSite(SimpleSite('a.dml:2'), is_a, 'a', None)
        site = TemplateSite(SimpleSite('b.dml:1'), is_b, 'b', None)
        self.assertEqual(expansion_chains(site), [[
            (is_b, 'template b instantiated by template a', None),
            (is_a, 'template a instantiated here', None)]])

    def test_objects(self):
        # template a { group g is b; } group top is a;
        device = Device('test', SimpleSite('dev.dml:1'))
        top = Group('top', SimpleSite('dev.dml:2'), device)
        top_g = Group('g', SimpleSite('a.dml:2'), top)
        is_a = SimpleSite('dev.dml:1')
        is_b = TemplateSite(SimpleSite('a.dml:2'), is_a, 'a', top)
        site = TemplateSite(SimpleSite('b.dml:1'), is_b, 'b', top_g)
        self.assertEqual(expansion_chains(site), [[
            (is_b, 'template b instantiated by template a (group top.g)',
             top_g),
            (is_a, 'template a instantiated here (group top)', top)]])
        # within the same object, only the outermost link mentions it
        site = TemplateSite(SimpleSite('b.dml:1'), is_b, 'b', top)
        self.assertEqual(expansion_chains(site), [[
            (is_b, 'template b instantiated by template a', None),
            (is_a, 'template a instantiated here (group top)', top)]])

    def test_outer_object_ignores_error_context(self):
        device = Device('test', SimpleSite('dev.dml:1'))
        top = Group('top', SimpleSite('dev.dml:2'), device)
        child = Group('g', SimpleSite('dev.dml:3'), top)
        plain = SimpleSite('dev.dml:4')
        expanded = TemplateSite(SimpleSite('a.dml:1'), plain, 'a', child)
        with ErrorContext(top):
            self.assertIsNone(plain.outer_object())
            self.assertIs(expanded.outer_object(), child)

    def test_multiple_sites_keep_chains_separate(self):
        is_a = SimpleSite('dev.dml:1')
        is_b = SimpleSite('dev.dml:2')
        device = Device('test', SimpleSite('dev.dml:1'))
        x = Group('x', SimpleSite('dev.dml:2'), device)
        y = Group('y', SimpleSite('dev.dml:3'), device)
        site_a = TemplateSite(SimpleSite('a.dml:1'), is_a, 'a', x)
        site_b = TemplateSite(SimpleSite('b.dml:1'), is_b, 'b', y)
        self.assertEqual(expansion_chains(site_a, site_b), [
            [(is_a, 'template a instantiated here (group x)', x)],
            [(is_b, 'template b instantiated here (group y)', y)]])

    def test_shared_tail_logging(self):
        is_a = SimpleSite('dev.dml:1')
        is_b = TemplateSite(SimpleSite('a.dml:2'), is_a, 'a', None)
        is_c = TemplateSite(SimpleSite('a.dml:3'), is_a, 'a', None)
        site_b = TemplateSite(SimpleSite('b.dml:1'), is_b, 'b', None)
        site_c = TemplateSite(SimpleSite('c.dml:1'), is_c, 'c', None)
        other_a = SimpleSite('dev.dml:2')
        site_a = TemplateSite(SimpleSite('a.dml:1'), other_a, 'a', None)
        self.assertEqual(expansion_chains(site_b, site_c), [
            [(is_b, 'template b instantiated by template a', None),
             (is_a, 'template a instantiated here', None)],
            [(is_c, 'template c instantiated by template a', None),
             (is_a, 'template a instantiated here', None)]])

        class Message(logging.LogMessage):
            kind = 'error'
            fmt = 'context test'

            def hints(self):
                return ((site, 'via here') for site in
                        [site_b, site_c, site_a, site_b])

        output = io.StringIO()
        with patch.object(logging.LogMessage, 'outfile', output), \
             patch.dict(logging.last_reported_loc, {}, clear=True):
            with patch.object(logging, 'last_reported_context', None):
                logging.report(Message(SimpleSite('b.dml:1')))
        self.assertEqual(output.getvalue().splitlines()[-5:], [
            'a.dml:2: template b instantiated by template a',
            'dev.dml:1: template a instantiated here',
            'a.dml:3: template c instantiated by template a',
            'dev.dml:2: template a instantiated here',
            'dev.dml:1: template a instantiated here'])

    def test_porting_message_has_no_context(self):
        cause = SimpleSite('dev.dml:1')
        site = TemplateSite(SimpleSite('a.dml:2'), cause, 'a', None)
        output = io.StringIO()
        with patch.object(logging.LogMessage, 'outfile', output), \
             patch.dict(logging.last_reported_loc, {}, clear=True), \
             patch.object(logging, 'last_reported_context', None):
            logging.report(logging.PortingMessage(site))
        self.assertEqual(len(output.getvalue().splitlines()), 1)

    def test_error_context_is_reported_as_hint(self):
        class Node:
            site = SimpleSite('dev.dml:1')

            def identity(self):
                return 'top'

        class Message(logging.LogMessage):
            kind = 'error'
            fmt = 'context test'

            def hints(self):
                return [(SimpleSite('hint.dml:3'), 'additional detail')]

        output = io.StringIO()
        with patch.object(logging.LogMessage, 'outfile', output), \
             patch.dict(logging.last_reported_loc, {}, clear=True), \
             patch.object(logging, 'last_reported_context', None):
            with ErrorContext(Node(), SimpleSite('call.dml:2')):
                logging.report(Message(SimpleSite('a.dml:1')))
        self.assertEqual(output.getvalue().splitlines(), [
            'a.dml:1: error: context test',
            'hint.dml:3: additional detail',
            'call.dml:2: in top'])

    def test_in_each(self):
        # 'in each (x, y) { is d; }', where x is instantiated in the
        # device and y is instantiated by template a
        is_a = SimpleSite('dev.dml:1')
        is_x = SimpleSite('dev.dml:2')
        is_y = TemplateSite(SimpleSite('a.dml:2'), is_a, 'a', None)
        in_each = SimpleSite('dev.dml:3')
        is_d = InEachSite(SimpleSite('dev.dml:4'), in_each, ['x', 'y'],
                          [(is_x, 'x', None), (is_y, 'y', None)])
        site = TemplateSite(SimpleSite('d.dml:1'), is_d, 'd', None)
        self.assertEqual(expansion_chains(site), [[
            (is_d, "template d instantiated by 'in each (x, y)' block",
             None),
            (is_x, 'template x instantiated here', None),
            (is_y, 'template y instantiated by template a', None),
            (is_a, 'template a instantiated here', None)]])

    def test_imported_files(self):
        import_a = SimpleSite('dev.dml:2')
        import_b = ImportSite(SimpleSite('a.dml:2'), import_a, 'a.dml')
        definition = ImportSite(SimpleSite('b.dml:3'), import_b, 'b.dml')
        self.assertEqual(expansion_chains(definition), [[
            (import_b, 'file b.dml imported here', None),
            (import_a, 'file a.dml imported here', None)]])

        imported_template_site = ImportSite(
            SimpleSite('b.dml:4'), import_b, 'b.dml')
        method = TemplateSite(SimpleSite('b.dml:5'), imported_template_site,
                              'example', None)
        self.assertEqual(expansion_chains(method), [[
            (imported_template_site, 'template example instantiated here',
             None),
            (import_b, 'file b.dml imported here', None),
            (import_a, 'file a.dml imported here', None)]])
