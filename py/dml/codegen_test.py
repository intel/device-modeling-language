# © 2021-2022 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import unittest
import math
from dml.codegen import loss_on_truncation

class Test_loss_on_truncation(unittest.TestCase):
    def int_edge_testing(self, signed, max_bits=64, val_wrapper=lambda x: x):
        def has_loss(v, bits):
            self.assertTrue(loss_on_truncation(val_wrapper(v), bits, signed))
        def no_loss(v, bits):
            self.assertFalse(loss_on_truncation(val_wrapper(v), bits, signed))

        for bits in range(2, max_bits+1):
            for i in range(-1, 2):
                no_loss(i, bits)
            max_pos = 2**(bits-signed) - 1
            # min_neg of unsigned designed to accept ~max_pos.
            min_neg = -2**(bits-signed)
            no_loss(max_pos, bits)
            no_loss(min_neg, bits)
            has_loss(max_pos + 1, bits)
            has_loss(min_neg - 1, bits)

    # Assuming we would ever add them, (u)int0 types are logically integer
    # types with only one representable value: 0.
    # For uint0, ~0 == -1 should also be accepted.
    def test_int0(self):
        for signed in (False, True):
            self.assertFalse(loss_on_truncation(0, 0, signed))
            self.assertTrue(loss_on_truncation(1, 0, signed))
            self.assertTrue(loss_on_truncation(-2, 0, signed))

        self.assertFalse(loss_on_truncation(-1, 0, False))
        self.assertTrue(loss_on_truncation(-1, 0, True))

    # int1s are given special treatment to allow `int1 a = 1` without
    # indicating truncation loss
    def test_int1(self):
        for signed in (False, True):
            for i in range(-1, 2):
                self.assertFalse(loss_on_truncation(i, 1, signed))
            self.assertTrue(loss_on_truncation(2, 1, signed))
        self.assertTrue(loss_on_truncation(-2, 1, True))
        # ~1 == -2
        self.assertFalse(loss_on_truncation(~1, 1, False))
        self.assertTrue(loss_on_truncation(~1 - 1, 1, False))

    def test_edge(self):
        self.int_edge_testing(False)
        self.int_edge_testing(True)

    def test_finite_floats_signed(self):
        def has_loss(v, bits, signed):
            self.assertTrue(loss_on_truncation(v, bits, signed))
        def no_loss(v, bits, signed):
            self.assertFalse(loss_on_truncation(v, bits, signed))

        for bits in range(0, 65):
            # Any truncation -- whether the result is inside or outside
            # representable range of the integer type -- is considered lossy
            for signed in (False, True):
                for v in (0.4, 0.6, -0.4, -0.6):
                    for i in range(-1, 2):
                        has_loss(i + v, bits, signed)

                    # Doubles past 2**53 do not have integer granularity
                    if (bits < 53):
                        has_loss(2**(bits-signed) - 1 + v, bits, signed)
                        has_loss(2**(bits-signed) - 1 + v, bits, signed)
                        has_loss(-(2**(bits-signed) - (not signed) + v),
                                 bits, signed)

                # Negative 0 is treated exactly like 0
                no_loss(-0.0, bits, signed)

            # Behaves exactly like normal integers if no truncation is needed
            # and target is signed
            self.int_edge_testing(True, 53, float)

    def test_finite_floats_unsigned(self):
        def has_loss(v, bits, signed):
            self.assertTrue(loss_on_truncation(v, bits, signed))
        def no_loss(v, bits, signed):
            self.assertFalse(loss_on_truncation(v, bits, signed))

        for bits in range(0, 65):
            # Any truncation -- whether the result is inside or outside
            # representable range of the integer type -- is considered lossy
            for v in (0.4, 0.6, -0.4, -0.6):
                has_loss(1 + v, bits, False)

                # Doubles past 2**53 do not have integer granularity
                if (bits < 53):
                    has_loss(2**bits - 1 + v, bits, False)

            # All negative floats but 0 are considered lossy when cast to
            # unsigned, as that is UB
            for v in (1, 0.4, 0.6):
                has_loss(float(-v), bits, False)

            if bits > 0:
                has_loss(float(-(2**bits - 1)), bits, False)

            no_loss(-0.0, bits, False)

    def test_nonfinite_floats(self):
        def has_loss(v, bits, signed):
            self.assertTrue(loss_on_truncation(v, bits, signed))

        for bits in range(0, 65):
            for signed in (False, True):
                has_loss(math.nan, bits, signed)
                has_loss(math.inf, bits, signed)
                has_loss(-math.inf, bits, signed)
