# © 2021-2022 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import unittest

from dml.traits import (
    Trait, ObjTraits,
)
import dml.objects
import dml.ast
import dml.ctree
from dml import crep
from dml import types
from dml import traits

class Test_traits(unittest.TestCase):
    def setUp(self):
        self.site = dml.logging.SimpleSite('X')
        dev = dml.objects.Device('dev', self.site)
        dev.name = 'dev'
        self.prev_device = dml.globals.device
        dml.globals.device = dev
        self.dev = dev

    def tearDown(self):
        dml.globals.device = self.prev_device

    def test_empty(self):
        t = Trait(None, 't', set(), {}, {}, {}, {}, {}, {})
        ot = ObjTraits(self.dev, {t}, {}, {}, {})
        self.dev.set_traits(ot)
        self.assertEqual(
            dml.ctree.mkCast(
                self.site, dml.ctree.mkNodeRef(self.site, self.dev, ()),
                t.type()).read(),
            '((t) {(&(*_tr__dev__t)), '
            + '((_identity_t) {.id = 0, .encoded_index = 0})})'
            )

    def test_one_default_method(self):
        body = dml.ast.compound(self.site, [])
        t = Trait(self.site, 't', set(),
                  {'m': (self.site, [], [], False, False, False, True, body, None)},
                  {}, {}, {}, {}, {})
        ot = ObjTraits(self.dev, {t}, {'m': t}, {}, {})
        self.dev.set_traits(ot)
        [(m, typ)] = list(t.vtable())
        # don't worry about the type for now
        self.assertEqual(m, 'm')
        ref = ot.lookup_shared_method_impl(self.site, 'm', ())
        self.assertTrue(ref)
        # does not crash
        with crep.DeviceInstanceContext():
            ref.call_expr([], types.TVoid()).read()
