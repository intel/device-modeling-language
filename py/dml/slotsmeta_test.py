# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import unittest

from dml.slotsmeta import *

# Silence 'unused' warnings from fisketur
def use(o): pass

class TestSlotsMeta(unittest.TestCase):
    def _test_slotsmeta(self):
        with self.assertRaises(AssertionError):
            # __init__ required
            class BadBase(metaclass=SlotsMeta): pass
            use(BadBase)
        class Base(metaclass=SlotsMeta):
            def __init__(self, x):
                self.x = x
        self.assertEquals(Base.init_args, ['self', 'x'])
        self.assertEquals(Base.__slots__, ('x',))
        class Sub1(Base):
            pass
        self.assertEquals(Sub1.init_args, ['self', 'x'])
        self.assertEquals(Sub1.__slots__, ())
        with self.assertRaises(AssertionError):
            class BadSub0(Base):
                # may not define __slots__
                # (defining it as a method ducks fisketur warnings)
                def __slots__(self): pass
            use(BadSub0)
        with self.assertRaises(AssertionError):
            # __init__ argnames must match superclass
            class BadSub1(Base):
                def __init__(self, y, z): pass
            use(BadSub1)
        with self.assertRaises(AssertionError):
            # varargs not allowed
            class BadSub2(Base):
                def __init__(self, x, *args): pass
            use(BadSub2)
        with self.assertRaises(AssertionError):
            # kwargs not allowed
            class BadSub3(Base):
                def __init__(self, x, **kwargs): pass
            use(BadSub3)
        with self.assertRaises(AssertionError):
            # args required by superclass must remain required
            class BadSub4(Base):
                def __init__(self, x=0): pass
            use(BadSub4)
        class Sub2(Base):
            def __init__(self, x, y):
                Base.__init__(self, x)
                self.y = y
        self.assertEquals(Sub2.init_args, ['self', 'x', 'y'])
        self.assertEquals(Sub2.__slots__, ('y',))
        with self.assertRaises(AssertionError):
            # all __init__ overrides must be on first parent
            class BadDiamond(Sub1, Sub2): pass
            use(BadDiamond)
        class Diamond(Sub2, Sub1):
            slots = ('w',)
            def __init__(self, x, y, z): pass

        self.assertEquals(Diamond.init_args, ['self', 'x', 'y', 'z'])
        self.assertEquals(Diamond.__slots__, ('z', 'w'))

class test_auto_init(unittest.TestCase):
    def test(self):
        with self.assertRaises(AssertionError):
            # auto_init does not work on base SlotsMeta class
            class BadBase(metaclass=SlotsMeta):
                @auto_init
                def __init__(self): pass
            use(BadBase)
        with self.assertRaises(AssertionError):
            class BadMeta(object):
                @auto_init
                def __init__(self): pass
            BadMeta()
        class Base(metaclass=SlotsMeta):
            def __init__(self, x):
                self.x = x * 2
        self.assertEqual(Base(4).x, 8)
        class Sub1(Base):
            @auto_init
            def __init__(self, x, y):
                self.x += self.y
        self.assertEqual(Sub1(4, 1).x, 9)

