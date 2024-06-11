# © 2024 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

from dml.types import *
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
        typ0 = TVoid()
        for typ in (TVoid(),
                    TNamed("int"),
                    TBool(),
                    TInt(8, False, {}),
                    TEndianInt(8, False, 'big-endian', {}),
                    TFloat("a"),
                    TArray(typ0, ctree.mkIntegerLiteral(0, 2)),
                    TPtr(typ0),
                    TVector(typ0),
                    TTrait(object()),
                    TStruct({"name": TInt(32, False)}),
                    TLayout("big-endian", {}),
                    TDevice("a")):
            typ_clone = typ.clone()
            self.assertTrue(
                realtype(typ_clone).eq(realtype(typ)))
            self.assertTrue(
                realtype(typ).eq(realtype(typ_clone)))
            typ_clone.const = True
            self.assertFalse(typ.const)
            typ = typ_clone.clone()
            self.assertTrue(typ.const)

        # special case for TraitList, because realtype requires global
        # state (typedefs)
        typ = TTraitList("a")
        typ_clone = typ.clone()
        self.assertTrue(typ.eq(typ_clone))
        self.assertTrue(typ_clone.eq(typ))
        dml.globals.dml_version = (1, 2)
        # types which do not support clone
        with self.assertRaises(logging.ICE):
            TUnknown().clone()

class Typ1(DMLType):
    def describe(self):
        return 'Typ1'
    def clone(self):
        return Typ1(self.const)

class Typ2(DMLType):
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
        except DMLUnkeyableType:
            pass

    def assert_neq(self, t1, t2, hash_collision=False):
        self.assertFalse(t1.eq(t2))
        self.assertFalse(t2.eq(t1))
        if not hash_collision:
            self.assertNotEqual(t1.hashed(), t2.hashed())
            try:
                self.assertNotEqual(t1.key(), t2.key())
            except DMLUnkeyableType:
                pass

    def test_DMLType(self):
        self.assert_eq(Typ1(), Typ1())
        self.assert_neq(Typ1(True), Typ1())
        self.assert_neq(Typ1(), Typ2())

    def test_IntegerType(self):
        self.assert_eq(TInt(8, False), TInt(8, False))
        self.assert_neq(TInt(16, False), TInt(8, False))
        # Not equivalent even though the C representations of uint16 and uint13
        # are compatible
        self.assert_neq(TInt(16, False), TInt(13, False))
        # Signedness
        self.assert_neq(TInt(8, True), TInt(8, False))

        # bitfields
        self.assert_neq(TInt(8, False), TInt(8, False, members={}))

        def bitfields():
            return TInt(32, False, { 'a': (TInt(8, False), 13, 6),
                                     'b': (TInt(13, False), 27, 15) })

        self.assert_eq(TInt(8, False, members={}), TInt(8, False, members={}))
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
        self.assert_eq(TEndianInt(8, False, 'big-endian'),
                       TEndianInt(8, False, 'big-endian'))
        self.assert_neq(TEndianInt(8, False, 'big-endian'),
                        TEndianInt(8, False, 'little-endian'))

    def test_TFloat(self):
        self.assert_eq(TFloat('double'), TFloat('double'))
        self.assert_neq(TFloat('double'), TFloat('float'))

    def mkTArray(self, size, signed=False):
        size = (ctree.mkIntegerConstant(None, size, signed) if size is not None
                else expr.mkLit(None, 'lit', TInt(32, False)))
        return TArray(TInt(32, False), size)

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
        self.assert_neq(self.mkTArray(4), TPtr(TInt(32, False)))

    def test_TPtr(self):
        self.assert_eq(TPtr(Typ1()), TPtr(Typ1()))
        self.assert_neq(TPtr(Typ1()), TPtr(Typ2()))
        self.assert_neq(TPtr(Typ1(), const=True), TPtr(Typ1()))
        self.assert_neq(TPtr(Typ1(const=True)), TPtr(Typ1()))
        self.assert_neq(TPtr(Typ1(), const=True),
                        TPtr(Typ1(const=True)))

        # void pointers are not special
        self.assert_neq(TPtr(Typ1()), TPtr(TVoid()))

    def test_TVector(self):
        v1 = TVector(Typ1())
        self.assert_eq(v1, v1.clone())
        v2 = v1.clone()
        v2.const = True
        self.assert_neq(v1, v2)
        self.assert_neq(v1, TVector(Typ1()))

    def test_TTrait(self):
        tr1 = traits.Trait(None, 't1', set(), {}, {}, {}, {}, {}, {}, {})
        tr2 = traits.Trait(None, 't2', set(), {}, {}, {}, {}, {}, {}, {})

        self.assert_eq(TTrait(tr1), TTrait(tr1))
        self.assert_neq(TTrait(tr1), TTrait(tr2))

    def test_TTraitList(self):
        self.assert_eq(TTraitList('t1'), TTraitList('t1'))
        self.assert_neq(TTraitList('t1'), TTraitList('t2'))


    def test_TStruct(self):
        members = {'a': Typ1()}
        t1 = TStruct(members, label="a_struct")
        t2 = TStruct(members)

        self.assert_eq(t1, TStruct(members, label="a_struct"))
        self.assert_neq(t1, t2)
        self.assert_eq(t2, t2.clone())
        self.assert_neq(t2, TStruct(members))

    def test_TExternStruct(self):
        self.assert_eq(TExternStruct({}, 0), TExternStruct({}, 0))
        self.assert_eq(TExternStruct({}, "str"), TExternStruct({}, "str"))
        self.assert_neq(TExternStruct({}, 0), TExternStruct({}, "str"))

    def test_TFunction(self):
        self.assert_eq(TFunction((), TVoid()), TFunction((), TVoid()))
        self.assert_neq(TFunction((Typ1(),), TVoid()), TFunction((), TVoid()))

        self.assert_eq(TFunction((Typ1(), TPtr(TVoid())), TPtr(Typ2())),
                       TFunction((Typ1(), TPtr(TVoid())), TPtr(Typ2())))
        self.assert_neq(TFunction((Typ1(), TPtr(TVoid())), TPtr(Typ1())),
                        TFunction((Typ1(), TPtr(TVoid())), TPtr(Typ2())))
        self.assert_neq(TFunction((Typ2(), TPtr(TVoid())), TPtr(Typ2())),
                        TFunction((Typ1(), TPtr(TVoid())), TPtr(Typ2())))
        self.assert_neq(TFunction((TPtr(TVoid()), Typ1()), TPtr(Typ2())),
                        TFunction((Typ1(), TPtr(TVoid())), TPtr(Typ2())))

        # Direct constness doesn't matter
        self.assert_eq(TFunction((Typ1(True), TPtr(TVoid(), True)),
                                 TPtr(Typ2(), True)),
                       TFunction((Typ1(), TPtr(TVoid())), TPtr(Typ2())))

        # Constness behind indirection does
        self.assert_neq(TFunction((Typ1(), TPtr(TVoid(True))), TPtr(Typ2())),
                        TFunction((Typ1(), TPtr(TVoid())), TPtr(Typ2())))
        self.assert_neq(TFunction((Typ1(), TPtr(TVoid())), TPtr(Typ2(True))),
                        TFunction((Typ1(), TPtr(TVoid())), TPtr(Typ2())))

        # Variadicity matters
        self.assert_eq(TFunction((), TVoid(), varargs=True),
                       TFunction((), TVoid(), varargs=True))
        self.assert_neq(TFunction((), TVoid()),
                        TFunction((), TVoid(), varargs=True))

    def test_THook(self):
        self.assert_eq(THook(()), THook(()))
        self.assert_neq(THook(()), THook((Typ1(),)))

        self.assert_eq(THook((Typ1(), Typ2())), THook((Typ1(), Typ2())))

        # Order matters
        self.assert_neq(THook((Typ1(), Typ2())), THook((Typ2(), Typ1())))

        # Direct constness matters (for now; might change with tuple types)
        self.assert_neq(THook((Typ1(True), Typ2())), THook((Typ1(), Typ2())))
