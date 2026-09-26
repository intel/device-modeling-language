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
        t = Trait(None, 't', set(), {}, {}, {}, {}, {}, {}, {}, {}, {})
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
                  {}, {}, {}, {}, {}, {}, {}, {})
        ot = ObjTraits(self.dev, {t}, {'m': t}, {}, {})
        self.dev.set_traits(ot)
        ref = ot.lookup_shared_method_impl(self.site, 'm', ())
        self.assertTrue(ref)
        # does not crash
        with crep.DeviceInstanceContext(), self.dev.use_for_codegen():
            ref.call_expr([], types.TVoid()).read()

class FakeSubobjTrait:
    def __init__(self, *parents):
        self.ancestors = set(parents).union(*(p.ancestors for p in parents))
    def implements(self, other):
        return other is self or other in self.ancestors

class FakeTrait:
    def __init__(self, **subobj_traits):
        self.subobj_traits = subobj_traits

class Test_inherited_subobjs(unittest.TestCase):
    def setUp(self):
        # x.g <- y.g, and the diamond d1.g <- d2.g, d3.g, d5.g
        self.xg = FakeSubobjTrait()
        self.yg = FakeSubobjTrait(self.xg)
        self.d1g = FakeSubobjTrait()
        self.d2g = FakeSubobjTrait(self.d1g)
        self.d3g = FakeSubobjTrait(self.d1g)
        self.d5g = FakeSubobjTrait(self.d1g)

    def check(self, parents, expected):
        self.assertEqual(
            traits.inherited_subobjs([FakeTrait(**p) for p in parents]),
            expected)

    def test_none(self):
        self.check([], {})
        self.check([{}], {})

    def test_single(self):
        self.check([{'g': self.xg}], {'g': [self.xg]})

    def test_chain(self):
        # ancestors are filtered out, regardless of order
        self.check([{'g': self.xg}, {'g': self.yg}], {'g': [self.yg]})
        self.check([{'g': self.yg}, {'g': self.xg}], {'g': [self.yg]})

    def test_multiple_paths(self):
        self.check([{'g': self.xg}, {'g': self.yg}, {'g': self.xg}],
                   {'g': [self.yg]})

    def test_one_branch_overrides(self):
        self.check([{'g': self.d2g}, {'g': self.d1g}], {'g': [self.d2g]})

    def test_diamond(self):
        self.check([{'g': self.d2g}, {'g': self.d3g}, {'g': self.d1g}],
                   {'g': [self.d2g, self.d3g]})
        self.check([{'g': self.d2g}, {'g': self.d3g}, {'g': self.d5g}],
                   {'g': [self.d2g, self.d3g, self.d5g]})

    def test_unrelated(self):
        self.check([{'g': self.xg}, {'g': self.d1g}],
                   {'g': [self.xg, self.d1g]})

    def test_names_independent(self):
        self.check([{'g': self.d2g, 'h': self.xg},
                    {'g': self.d3g, 'h': self.yg}],
                   {'g': [self.d2g, self.d3g], 'h': [self.yg]})

    def test_diamond_mixed(self):
        # in a diamond over d1, g is overridden on both sides, h on
        # neither, and k is declared on one side only
        d1h = FakeSubobjTrait()
        d2k = FakeSubobjTrait()
        self.check([{'g': self.d2g, 'h': d1h, 'k': d2k},
                    {'g': self.d3g, 'h': d1h},
                    {'g': self.d1g, 'h': d1h}],
                   {'g': [self.d2g, self.d3g], 'h': [d1h], 'k': [d2k]})
