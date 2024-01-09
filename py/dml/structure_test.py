# © 2022 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import unittest

from dml.structure import param_linear_int
from dml import objects, logging, ctree, types

class test_param_linear_int(unittest.TestCase):
    def p(self, mkexpr_, dimsizes):
        nowhere = logging.SimpleSite("nowhere")
        class PE(objects.ParamExpr):
            __slots__ = ()
            site = nowhere
            def mkexpr(self, indices):
                return mkexpr_(indices)
        dev = objects.Device('dev', nowhere)
        group = objects.Group('g', nowhere, dev, arraylens=dimsizes,
                              idxvars=tuple(f'i{n}' for n in dimsizes))
        return objects.DMLParameter('p', nowhere, group, PE())

    def test(self):
        site = logging.SimpleSite("nowhere")
        _17 = ctree.mkIntegerConstant(site, 17, True)
        _4 = ctree.mkIntegerConstant(site, 4, True)
        maxuint = (1 << 64) - 1
        self.assertEqual(
            param_linear_int(self.p(
                lambda indices: _17, ())),
            (17,))
        self.assertEqual(
            param_linear_int(self.p(
                lambda indices: indices[0], (2,))),
            (1, 0))
        self.assertEqual(
            param_linear_int(self.p(
                lambda indices: ctree.mkAdd(site, indices[0], _17), (2,))),
            (1, 17))
        self.assertEqual(
            param_linear_int(self.p(
                lambda indices:
                ctree.mkAdd(site, ctree.mkAdd(site, _17, indices[0]), _4),
                (2,))),
            (1, 21))
        self.assertEqual(
            param_linear_int(self.p(
                lambda indices:
                ctree.mkSubtract(
                    site, ctree.mkSubtract(site, _17, indices[0]), _4), (2,))),
            (maxuint, 13))
        self.assertEqual(
            param_linear_int(self.p(
                lambda indices:
                ctree.mkMult(
                    site, ctree.mkMult(site, _17, indices[0]), _4), (2,))),
            (68, 0))
        self.assertEqual(
            param_linear_int(self.p(
                lambda indices:
                ctree.mkMult(site, _17,
                             ctree.mkAdd(site, indices[0], indices[1])),
                (2, 2))),
            (17, 17, 0))
        # bilinear
        self.assertEqual(
            param_linear_int(self.p(
                lambda indices:
                ctree.mkMult(site, indices[0], indices[1]), (2, 2))),
            None)
        # error in expression: error handling delegated to caller
        self.assertEqual(
            param_linear_int(self.p(
                lambda indices: ctree.mkDereference(site, _4), ())),
            None)
        # arithmetic is done modulo 2**64, so a 64-bit cast is a nop
        # whereas an upcast is nonlinear.
        for signed in True, False:
            self.assertEqual(
                param_linear_int(self.p(
                    lambda indices:
                    ctree.mkCast(site, indices[0], types.TInt(64, signed)),
                    (2,))),
                (1, 0))
            self.assertEqual(
                param_linear_int(self.p(
                    lambda indices:
                    ctree.mkCast(site, indices[0], types.TInt(63, signed)),
                    (2,))),
                None)
