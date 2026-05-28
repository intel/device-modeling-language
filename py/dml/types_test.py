# © 2024 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import dml.types as tp
import dml.globals
from dml import ctree
from dml import expr
from dml import traits
from dml import logging
from contextlib import contextmanager

import unittest

class TestClone(unittest.TestCase):
    def test(self):
        # types which support clone
        typ0 = tp.Void()
        for typ in (tp.Void(),
                    tp.Named("int"),
                    tp.Bool(),
                    tp.Int(8, False, {}),
                    tp.EndianInt(8, False, 'big-endian', {}),
                    tp.Float("a"),
                    tp.Array(typ0, ctree.mkIntegerLiteral(0, 2)),
                    tp.Ptr(typ0),
                    tp.Vector(typ0),
                    tp.Trait(object()),
                    tp.Struct({"name": tp.Int(32, False)}),
                    tp.Layout("big-endian", []),
                    tp.Device("a")):
            typ_clone = typ.clone()
            self.assertTrue(
                tp.realtype(typ_clone).eq(tp.realtype(typ)))
            self.assertTrue(
                tp.realtype(typ).eq(tp.realtype(typ_clone)))
            typ_clone.const = True
            self.assertFalse(typ.const)
            typ = typ_clone.clone()
            self.assertTrue(typ.const)

        # special case for tp.TraitList, because tp.realtype requires global
        # state (typedefs)
        typ = tp.TraitList("a")
        typ_clone = typ.clone()
        self.assertTrue(typ.eq(typ_clone))
        self.assertTrue(typ_clone.eq(typ))
        dml.globals.dml_version = (1, 2)
        # types which do not support clone
        with self.assertRaises(logging.ICE):
            tp.Unknown().clone()

class Typ1(tp.DMLType):
    def describe(self):
        return 'Typ1'
    def clone(self):
        return Typ1(self.const)

class Typ2(tp.DMLType):
    def describe(self):
        return 'Typ2'
    def clone(self):
        return Typ1(self.const)

class TestEq(unittest.TestCase):
    def assert_eq(self, t1, t2):
        self.assertTrue(t1.eq(t2))
        self.assertTrue(t2.eq(t1))
        self.assertEqual(t1.hashed(), t2.hashed())
        try:
            self.assertEqual(t1.key(), t2.key())
        except tp.DMLUnkeyableType:
            pass

    def assert_neq(self, t1, t2, hash_collision=False):
        self.assertFalse(t1.eq(t2))
        self.assertFalse(t2.eq(t1))
        if not hash_collision:
            self.assertNotEqual(t1.hashed(), t2.hashed())
            try:
                self.assertNotEqual(t1.key(), t2.key())
            except tp.DMLUnkeyableType:
                pass

    def test_DMLType(self):
        self.assert_eq(Typ1(), Typ1())
        self.assert_neq(Typ1(True), Typ1())
        self.assert_neq(Typ1(), Typ2())

    def test_IntegerType(self):
        self.assert_eq(tp.Int(8, False), tp.Int(8, False))
        self.assert_neq(tp.Int(16, False), tp.Int(8, False))
        # Not equivalent even though the C representations of uint16 and uint13
        # are compatible
        self.assert_neq(tp.Int(16, False), tp.Int(13, False))
        # Signedness
        self.assert_neq(tp.Int(8, True), tp.Int(8, False))

        # bitfields
        self.assert_neq(tp.Int(8, False), tp.Int(8, False, members={}))

        def bitfields():
            return tp.Int(32, False, { 'a': (tp.Int(8, False), 13, 6),
                                     'b': (tp.Int(13, False), 27, 15) })

        self.assert_eq(tp.Int(8, False, members={}), tp.Int(8, False, members={}))
        self.assert_eq(bitfields(), bitfields())

        @contextmanager
        def neq_bitfields_testcase():
            t = bitfields()
            yield t
            self.assert_neq(bitfields(), t)

        # Presence of members matter
        with neq_bitfields_testcase() as t:
            del t.members['a']

        # type of members matters
        with neq_bitfields_testcase() as t:
            t.members['a'][0].signed = True

        # msb/lsb of members matters
        with neq_bitfields_testcase() as t:
            (mt, msb, lsb) = t.members['a']
            t.members['a'] = (mt, msb - 1, lsb - 1)

        # order matters
        with neq_bitfields_testcase() as t:
            t.members = dict(reversed(t.members.items()))

        # names matter
        with neq_bitfields_testcase() as t:
            t.members['c'] = t.members['b']
            del t.members['b']

    def test_TEndianInt(self):
        self.assert_eq(tp.EndianInt(8, False, 'big-endian'),
                       tp.EndianInt(8, False, 'big-endian'))
        self.assert_neq(tp.EndianInt(8, False, 'big-endian'),
                        tp.EndianInt(8, False, 'little-endian'))

    def test_TFloat(self):
        self.assert_eq(tp.Float('double'), tp.Float('double'))
        self.assert_neq(tp.Float('double'), tp.Float('float'))

    def mkTArray(self, size, signed=False):
        size = (ctree.mkIntegerConstant(None, size, signed) if size is not None
                else expr.mkLit(None, 'lit', tp.Int(32, False)))
        return tp.Array(tp.Int(32, False), size)

    def test_TArray(self):
        # Arrays of constant size need the sizes be equal to be equal.
        # The type of the size expression is irrelevant.
        self.assert_eq(self.mkTArray(4), self.mkTArray(4))
        self.assert_eq(self.mkTArray(4), self.mkTArray(4, True))
        self.assert_neq(self.mkTArray(4), self.mkTArray(5))

        # Arrays of non-constant size needs the underlying expression to be
        # identical in order to be equal.
        self.assert_neq(self.mkTArray(None), self.mkTArray(None))
        a = self.mkTArray(None)
        b = self.mkTArray(None)
        b.size = a.size
        self.assert_eq(a, b)

        @contextmanager
        def array_testcase(validate):
            a = self.mkTArray(4)
            b = self.mkTArray(4)
            yield (a, b)
            validate(a, b)

        def eq_array_testcase(): return array_testcase(self.assert_eq)
        def neq_array_testcase(): return array_testcase(self.assert_neq)

        # Base type matters
        with neq_array_testcase() as (a, _):
            a.base.signed = True

        # Constness: constness of base type and array are normalized
        with neq_array_testcase() as (a, _):
            a.const = True
        with eq_array_testcase() as (a, b):
            a.const = True
            b.const = True
        with eq_array_testcase() as (a, b):
            a.base.const = True
            b.const = True
        with eq_array_testcase() as (a, b):
            a.base = self.mkTArray(4)
            b.base = self.mkTArray(4)
            a.base.base.const = True
            b.const = True

        # Pointers are not equivalent to arrays
        self.assert_neq(self.mkTArray(4), tp.Ptr(tp.Int(32, False)))

    def test_TPtr(self):
        self.assert_eq(tp.Ptr(Typ1()), tp.Ptr(Typ1()))
        self.assert_neq(tp.Ptr(Typ1()), tp.Ptr(Typ2()))
        self.assert_neq(tp.Ptr(Typ1(), const=True), tp.Ptr(Typ1()))
        self.assert_neq(tp.Ptr(Typ1(const=True)), tp.Ptr(Typ1()))
        self.assert_neq(tp.Ptr(Typ1(), const=True),
                        tp.Ptr(Typ1(const=True)))

        # void pointers are not special
        self.assert_neq(tp.Ptr(Typ1()), tp.Ptr(tp.Void()))

    def test_TVector(self):
        v1 = tp.Vector(Typ1())
        self.assert_eq(v1, v1.clone())
        v2 = v1.clone()
        v2.const = True
        self.assert_neq(v1, v2)
        self.assert_neq(v1, tp.Vector(Typ1()))

    def test_TTrait(self):
        tr1 = traits.Trait(None, 't1', set(), {}, {}, {}, {}, {}, {}, {})
        tr2 = traits.Trait(None, 't2', set(), {}, {}, {}, {}, {}, {}, {})

        self.assert_eq(tp.Trait(tr1), tp.Trait(tr1))
        self.assert_neq(tp.Trait(tr1), tp.Trait(tr2))

    def test_TTraitList(self):
        self.assert_eq(tp.TraitList('t1'), tp.TraitList('t1'))
        self.assert_neq(tp.TraitList('t1'), tp.TraitList('t2'))


    def test_TStruct(self):
        members = {'a': Typ1()}
        t1 = tp.Struct(members, label="a_struct")
        t2 = tp.Struct(members)

        self.assert_eq(t1, tp.Struct(members, label="a_struct"))
        self.assert_neq(t1, t2)
        self.assert_eq(t2, t2.clone())
        self.assert_neq(t2, tp.Struct(members))

    def test_TExternStruct(self):
        self.assert_eq(tp.ExternStruct({}, 0), tp.ExternStruct({}, 0))
        self.assert_eq(tp.ExternStruct({}, "str"), tp.ExternStruct({}, "str"))
        self.assert_neq(tp.ExternStruct({}, 0), tp.ExternStruct({}, "str"))

    def test_TFunction(self):
        self.assert_eq(tp.Function((), tp.Void()), tp.Function((), tp.Void()))
        self.assert_neq(tp.Function((Typ1(),), tp.Void()), tp.Function((), tp.Void()))

        self.assert_eq(tp.Function((Typ1(), tp.Ptr(tp.Void())), tp.Ptr(Typ2())),
                       tp.Function((Typ1(), tp.Ptr(tp.Void())), tp.Ptr(Typ2())))
        self.assert_neq(tp.Function((Typ1(), tp.Ptr(tp.Void())), tp.Ptr(Typ1())),
                        tp.Function((Typ1(), tp.Ptr(tp.Void())), tp.Ptr(Typ2())))
        self.assert_neq(tp.Function((Typ2(), tp.Ptr(tp.Void())), tp.Ptr(Typ2())),
                        tp.Function((Typ1(), tp.Ptr(tp.Void())), tp.Ptr(Typ2())))
        self.assert_neq(tp.Function((tp.Ptr(tp.Void()), Typ1()), tp.Ptr(Typ2())),
                        tp.Function((Typ1(), tp.Ptr(tp.Void())), tp.Ptr(Typ2())))

        # Direct constness doesn't matter
        self.assert_eq(tp.Function((Typ1(True), tp.Ptr(tp.Void(), True)),
                                 tp.Ptr(Typ2(), True)),
                       tp.Function((Typ1(), tp.Ptr(tp.Void())), tp.Ptr(Typ2())))

        # Constness behind indirection does
        self.assert_neq(tp.Function((Typ1(), tp.Ptr(tp.Void(True))), tp.Ptr(Typ2())),
                        tp.Function((Typ1(), tp.Ptr(tp.Void())), tp.Ptr(Typ2())))
        self.assert_neq(tp.Function((Typ1(), tp.Ptr(tp.Void())), tp.Ptr(Typ2(True))),
                        tp.Function((Typ1(), tp.Ptr(tp.Void())), tp.Ptr(Typ2())))

        # Variadicity matters
        self.assert_eq(tp.Function((), tp.Void(), varargs=True),
                       tp.Function((), tp.Void(), varargs=True))
        self.assert_neq(tp.Function((), tp.Void()),
                        tp.Function((), tp.Void(), varargs=True))

    def test_THook(self):
        self.assert_eq(tp.Hook(()), tp.Hook(()))
        self.assert_neq(tp.Hook(()), tp.Hook((Typ1(),)))

        self.assert_eq(tp.Hook((Typ1(), Typ2())), tp.Hook((Typ1(), Typ2())))

        # Order matters
        self.assert_neq(tp.Hook((Typ1(), Typ2())), tp.Hook((Typ2(), Typ1())))

        # Direct constness matters (for now; might change with tuple types)
        self.assert_neq(tp.Hook((Typ1(True), Typ2())), tp.Hook((Typ1(), Typ2())))
