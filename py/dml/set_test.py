# © 2024 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

from dml.set import Set

import unittest

class TestSet(unittest.TestCase):
    def test_contains(self):
        (o1, o2) = (object(), object())
        x = Set([0, 2, 'foo', o1])
        self.assertTrue(0 in x)
        self.assertFalse(1 in x)
        self.assertTrue('foo' in x)
        self.assertFalse('bar' in x)
        self.assertTrue(o1 in x)
        self.assertFalse(o2 in x)

    def test_eq(self):
        x = Set(range(10))
        self.assertEqual(x, Set(range(10)))
        self.assertNotEqual(x, Set(range(11)))
        self.assertEqual(x, set(range(10)))
        assert set(range(10)) == {e: e for e in range(10)}.keys()
        self.assertEqual(x, {e: e for e in range(10)}.keys())

    def test_str_repr(self):
        for f in [str, repr]:
            self.assertEqual(f(Set()), 'Set()')
            self.assertEqual(f(Set([1, 'one'])), "Set([1, 'one'])")

    def test_iter(self):
        els = [object() for _ in range(100)]
        x = Set(x for x in els)
        self.assertEqual(list(x), els)

    def test_len(self):
        self.assertEqual(len(Set()), 0)
        self.assertEqual(len(Set(range(10))), 10)
        # the bool function relies on __len__
        self.assertFalse(bool(Set()))
        self.assertTrue(bool(Set([1])))

    def test_add(self):
        x = Set()
        x.add(4)
        x.add(10)
        x.add(8)
        self.assertTrue(4 in x)
        self.assertTrue(8 in x)

    def test_update(self):
        x = Set([10, 4, 2])
        x.update(x for x in [4, 8, 10])
        self.assertEqual(list(x), [10, 4, 2, 8])

    def test_remove(self):
        x = Set(range(10))
        x.remove(4)
        x.remove(7)
        self.assertEqual(list(x), [e for e in range(10) if e not in {4, 7}])
        with self.assertRaises(KeyError):
            x.remove(None)

    def test_hash(self):
        for s in [Set(), set()]:
            with self.assertRaises(TypeError):
                # fisketur[useless-builtin-call]
                hash(s)

    def test_intersection(self):
        x = Set(range(10))
        y = x.intersection()
        self.assertIsNot(x, y)
        self.assertEqual(list(y), list(range(10)))
        self.assertEqual(
            list(Set(range(10)).intersection({5, 3, 10})),
            [3, 5])
        self.assertEqual(
            list(Set(range(10)).intersection({5: 1, 3: 7, 10: 3})),
            [3, 5])
        self.assertEqual(
            list(Set(range(10)).intersection(range(0, 20, 2), range(0, 20, 3))),
            [0, 6])

    def test_difference(self):
        self.assertEqual(list(Set([2, 1]).difference()), [2, 1])
        for other in [{2, 4}, {2: None, 4: None}, Set([2, 4]), [2, 4]]:
            self.assertEqual(list(Set([3, 2, 1]).difference(other)),
                             [3, 1])
        self.assertEqual(
            list(Set(range(10)).difference(
                range(0, 20, 2), range(0, 20, 3), range(0, 20, 5))),
            [1, 7])

    def test_union(self):
        self.assertEqual(list(Set([1, 2]).union()), [1, 2])
        self.assertEqual(list(Set([6, 1, 2]).union([5, 2], range(6, 0, -2))),
                         [6, 1, 2, 5, 4])
