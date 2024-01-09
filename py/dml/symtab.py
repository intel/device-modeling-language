# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

# Symbol table management

__all__ = (
    'Symtab',
    'MethodParamScope',
    'LocalSymbol',
    'StaticSymbol',
    'Symbol',
    'global_scope',
    )

from .logging import report, ICE, SimpleSite, dbg
import dml.globals

class Symbol(object):
    "a symbol in a symbol table"
    pseudo = True
    __slots__ = ('name', 'value', 'site', '_refcount')
    def __init__(self, name, value = None, site = None):
        assert site
        self.name = name
        self.value = value
        self.site = site

        # Use assignment counting to avoid some errors with unused
        # variables not getting any inferred type
        self._refcount = 0

    def incref(self):
        self._refcount += 1
    def decref(self):
        self._refcount -= 1
        assert self._refcount >= 0
    def refcount(self):
        return self._refcount

    def __repr__(self):
        return '%s(%s, %s)' % (self.__class__.__name__, self.name,
                               repr(self.value))

    def expr(self, site):
        raise ICE(site, "can't evaluate %r" % (type(self),))

class LocalSymbol(Symbol):
    """A symbol that corresponds to a local variable.  The 'value' is the C
    variable name, which defaults to the symbol name"""
    pseudo = False
    __slots__ = ('type', 'init', 'stmt')

    def __init__(self, name, cname, type = None, site = None, init = None,
                 stmt = False):
        Symbol.__init__(self, name, value = cname, site = site)
        self.type = type
        self.init = init
        self.stmt = stmt

class StaticSymbol(Symbol):
    """A symbol that corresponds to a static variable.  The 'value' is
    the name of the field in the device struct"""
    __slots__ = ('type', 'init', 'stmt')
    def __init__(self, name, cname, type = None, site = None, init = None,
                 stmt = False):
        Symbol.__init__(self, name, value = cname, site = site)
        self.type = type
        self.init = init
        self.stmt = stmt

symtab_idx = 0

class Symtab(object):
    def __init__(self, parent=None, location=None):
        global symtab_idx
        symtab_idx += 1
        self.idx = symtab_idx

        self.symdict = {}
        self.symlist = []
        self.parent = parent
    def __repr__(self):
        return '%s(%s)' % (self.__class__.__name__, repr(self.parent))
    def lookup(self, name, local = False):
        sym = self.symdict.get(name)
        if sym:
            return sym
        if not local and self.parent:
            return self.parent.lookup(name)
        return None
    def add(self, sym):
        if not isinstance(sym, Symbol):
            raise TypeError(repr(sym) + " is not a Symbol")
        if sym.name in self.symdict:
            raise ICE(sym.site, "duplicate symbol %s" % sym.name)
        self.symdict[sym.name] = sym
        self.symlist.append(sym)
    def symbols(self):
        return self.symlist[:]

    def add_variable(self, name, type=None, init=None, site=None, stmt=False,
                     make_unique=True):
        if init:
            from . import ctree
            assert isinstance(init, ctree.Initializer)
        if make_unique:
            cname = self.unique_cname(name)
        else:
            cname = name
        if init:
            # We need to add to the reference count here, since it
            # will be decremented in declarations()
            init.incref()
        sym = LocalSymbol(name, cname, type=type, init=init,
                          site=site, stmt=stmt)
        self.add(sym)
        return sym

    def unique_cname(self, name):
        return f'v{self.idx}_{name}'

class MethodParamScope(Symtab):
    '''Scope for method parameters.  Used to identify when local
    variables shadow method parameters.'''
    pass

global_scope = Symtab()
