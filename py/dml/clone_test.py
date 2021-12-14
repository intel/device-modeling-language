# © 2021-2022 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import dml.types as dt
import dml.globals
from dml import ctree
from dml import logging
from dml import types

import unittest

class TestClone(unittest.TestCase):
    def test(self):
        # types which support clone
        typ0 = dt.TVoid()
        for typ in (dt.TVoid(),
                    dt.TNamed("int"),
                    dt.TBool(),
                    dt.TInt(8, False, {}),
                    dt.TEndianInt(8, False, 'big-endian', {}),
                    dt.TFloat("a"),
                    dt.TArray(typ0, ctree.mkIntegerLiteral(0, 2)),
                    dt.TPtr(typ0),
                    dt.TVector(typ0),
                    dt.TTrait(object()),
                    dt.TStruct({"name": types.TInt(32, False)}),
                    dt.TLayout("big-endian", {}),
                    dt.TFunction([], dt.TVoid()),
                    dt.TDevice("a")):
            typ_clone = typ.clone()
            self.assertEqual(
                types.realtype(typ_clone).cmp(types.realtype(typ)), 0)
            self.assertEqual(
                types.realtype(typ).cmp(types.realtype(typ_clone)), 0)
            typ_clone.const = True
            self.assertEqual(typ.const, False)
        # special case for TraitList, because realtype requires global
        # state (typedefs) and always has const=True
        typ = dt.TTraitList("a")
        typ_clone = typ.clone()
        self.assertEqual(typ.cmp(typ_clone), 0)
        self.assertEqual(typ_clone.cmp(typ), 0)
        self.assertEqual(typ.const, True)
        dml.globals.dml_version = (1, 2)
        # types which do not support clone
        with self.assertRaises(logging.ICE):
            dt.TUnknown().clone()
