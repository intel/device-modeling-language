# © 2024 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

from itertools import chain

class Set:
    '''Restricted clone of builtin.set where iteration preserves
    insertion order

    Performance is worse than that of builtins.set; for the slowest
    operations the slowdown is about 20% worse using pypy, and about
    100% using CPython

    '''
    __slots__ = ['_d', '__iter__', '__contains__', '__len__', 'remove']
    def __init__(self, els=()):
        d = {e: None for e in els}
        self._d = d
        self.__iter__ = self._d.__iter__
        self.__contains__ = self._d.__contains__
        self.__len__ = self._d.__len__
        self.remove = self._d.__delitem__

    def __repr__(self):
        if self._d:
            return f"Set([{', '.join(map(repr, self._d))}])"
        else:
            return 'Set()'

    def __str__(self):
        return self.__repr__()

    def __eq__(self, other):
        if isinstance(other, Set):
            return self._d.keys().__eq__(other._d.keys())
        return self._d.keys().__eq__(other)

    __hash__ = None

    def add(self, x):
        self._d[x] = None

    def pop(self):
        return self._d.popitem()[0]

    def update(self, xs):
        self._d.update((x, None) for x in xs)

    def intersection(self, *others):
        if not others:
            return Set(self)
        if len(others) == 1 and isinstance(others[0], (set, Set, dict)):
            # __contains__ known to be fast
            [i] = others
        else:
            i = set(others[0]).intersection(*others[1:])
        return Set(x for x in self._d if x in i)

    def difference(self, *others):
        if len(self._d) == 0:
            return Set()
        if len(others) == 1 and isinstance(others[0], (set, Set, dict)):
            [u] = others
        else:
            u = set().union(*others)
        return Set(x for x in self._d if x not in u)

    def union(self, *others):
        return Set(chain(self._d, *others))
