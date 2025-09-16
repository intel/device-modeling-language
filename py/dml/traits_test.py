# © 2021 Intel Corporation
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
from dml import serialize

class Test_traits(unittest.TestCase):
    def setUp(self):
        self.site = dml.logging.SimpleSite('X')
        dev = dml.objects.Device('dev', self.site)
        dev.name = 'dev'
        self.prev_device = dml.globals.device
        dml.globals.device = dev
        dml.globals.serialized_traits = serialize.SerializedTraits()
        self.dev = dev

    def tearDown(self):
        dml.globals.device = self.prev_device

    def test_empty(self):
        t = Trait(None, 't', set(), {}, {}, {}, {}, {}, {}, {})
        ot = ObjTraits(self.dev, {t}, {}, {}, {})
        self.dev.set_traits(ot)
        with self.dev.use_for_codegen():
            self.assertEqual(
                dml.ctree.mkCast(
                    self.site, dml.ctree.mkNodeRef(self.site, self.dev, ()),
                    t.type()).read(),
                '((t) {(&_tr__dev__t), '
                + '((_identity_t) {.id = 1, .encoded_index = 0})})'
                )

    def test_one_default_method(self):
        body = dml.ast.compound(self.site, [], self.site)
        t = Trait(self.site, 't', set(),
                  {'m': (self.site, [], [], False, False, False, False, True,
                         False, body, None)},
                  {}, {}, {}, {}, {}, {})
        ot = ObjTraits(self.dev, {t}, {'m': t}, {}, {})
        self.dev.set_traits(ot)
        ref = ot.lookup_shared_method_impl(self.site, 'm', ())
        self.assertTrue(ref)
        # does not crash
        with crep.DeviceInstanceContext(), self.dev.use_for_codegen():
            ref.call_expr([], types.TVoid()).read()
