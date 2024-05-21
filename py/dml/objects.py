# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import itertools
import abc
import contextlib

from .logging import dollar
from .set import Set
import dml.globals

__all__ = (
    'Attribute',
    'Bank',
    'Connection',
    'DMLObject',
    'DMLParameter',
    'Device',
    'Event',
    'Field',
    'Group',
    'Implement',
    'Interface',
    'Method',
    'Port',
    'Register',
    'Saved',
    'Session',
    )

class DMLObject(object):
    __slots__ = ('ident', 'site', 'parent', 'nongroup_parent', 'object_parent',
                 'dimensions')
    def __init__(self, ident, site, parent):
        self.ident = ident
        self.site = site
        self.parent = parent
        self.nongroup_parent = (parent
                                and (parent if parent.objtype != 'group'
                                     else parent.nongroup_parent))
        self.object_parent = (parent
                              and (parent if parent.objtype in
                                   {'device', 'bank', 'subdevice', 'port'}
                                   else parent.object_parent))
        self.dimensions = parent.dimensions if parent else 0

    _component_dict = None
    _components = []
    _arraylens = ()
    traits = None

    @property
    def name(self):
        return self.ident

    @property
    def name_anonymized(self):
        return self.name

    def __repr__(self):
        if self.parent: parent = self.parent.ident
        else: parent = None
        return '%s(%r, %r)' % (self.__class__.__name__, self.ident, parent)

    def attrname(self):
        attr = self.parent.attrname()
        if attr:
            if self.name:
                return attr + '_' + self.name
            else:
                return attr
        return self.name

    def isindexed(self):
        return bool(self._arraylens)

    def arraylens(self):
        return self._arraylens

    def local_dimensions(self):
        return len(self._arraylens)

    def nonlocal_dimensions(self):
        return self.dimensions - self.local_dimensions()

    @property
    def dimsizes(self):
        dims = self.parent.dimsizes
        l = self.arraylens()
        if l:
            return dims + l
        else:
            return dims

    def all_indices(self):
        return itertools.product(*(list(range(i)) for i in self.dimsizes))

    _idxstr = None

    def logname(self, indices=(), relative='device'):
        """Return the qualified name of the object, relative to
        nearest ancestor of type 'relative'."""
        name = self.name
        if self.parent and self.parent.objtype != relative:
            pname = self.parent.logname(indices, relative=relative)
            if not pname:
                pname = ''
            if not name:
                name = pname
            elif pname:
                name = pname + '.' + name
        return name or ""

    def logname_anonymized(self, indices=(), relative='device'):
        """Return the qualified name of the object, relative to
        nearest ancestor of type 'relative', but where each confidential
        member in the hierarchy has its name anonymized (only applicable in
        1.2).
        This should be used instead of logname for any strings that may be
        present in DML model binaries."""
        if dml.globals.dml_version != (1, 2):
            return self.logname(indices, relative)

        name = self.name_anonymized
        if self.parent and self.parent.objtype != relative:
            pname = self.parent.logname_anonymized(indices, relative=relative)
            if not pname:
                pname = ''
            if not name:
                name = pname
            elif pname:
                name = pname + '.' + name
        return name or ""

    def identity(self, indices=(), relative='device'):
        """Return the qualified DML identity of the object, relative to
        nearest ancestor of type 'relative'

        The DML identity of an object is how it would be referred to
        within DML code.
        Roughly speaking, objectref(node.identity()) == node."""
        ident = self.ident
        if self.parent and self.parent.objtype != relative:
            pident = self.parent.identity(indices, relative=relative)
            if not pident:
                pident = ''
            if not ident:
                ident = pident
            elif pident:
                ident = pident + "." + ident
        return ident or ""

    # The number of references to this object in reachable code, or
    # None if reference counting is not applicable for the object type.
    @property
    def refcount(self):
        return None

    def get_component(self, ident, objtype = None):
        if not self._component_dict:
            return None
        comp = self._component_dict.get(ident)
        if comp and objtype and comp.objtype != objtype:
            comp = None
        # print 'GETCOMP', self, name, '=>', comp
        return comp
    def get_components(self, *objtypes):
        if objtypes:
            comps = [ n for n in self._components if n.objtype in objtypes ]
        else:
            comps = list(self._components)
        # print 'GETCOMPS', self, objtypes, '=>', comps
        return comps
    def get_recursive_components(self, *objtypes):
        comps = []
        for n in self._components:
            if n.objtype in objtypes:
                comps.append(n)
            comps.extend(n.get_recursive_components(*objtypes))
        # print 'GETRECCOMPS', self, objtypes, '=>', comps
        return comps

    def recursive_components_postorder(self):
        comps = []
        for n in self._components:
            comps.extend(n.recursive_components_postorder())
            comps.append(n)
        return comps

    @property
    def idxvars(self):
        return self.parent.idxvars

class CompositeObject(DMLObject):
    '''A DML object that may contain other DML objects'''
    __slots__ = ('name', '_components', '_component_dict', '_arraylens',
                 '_idxvars', 'traits', 'templates', 'template_method_impls',
                 '_confidential', 'uniq')

    def __init__(self, ident, site, parent, arraylens = (), idxvars = ()):
        super(CompositeObject, self).__init__(ident, site, parent)
        self._components = []
        # TODO: remove this property, instead use param_str to
        # retrieve name when needed
        self.name = None
        # Use None here rather than creating an empty dictionary. This
        # has a noticeable effect on memory usage.
        self._component_dict = None

        self._arraylens = arraylens
        self._idxvars = idxvars

        self.dimensions += self.local_dimensions()

        # set to an ObjTraits object after object creation
        self.traits = None

        # Template -> ObjectSpec (site-wrapped)
        # Set after object creation
        self.templates = None

        # (Template, string) -> Method
        # ... where the string is a method identifier
        # Populated after object creation
        # TODO: Preferably, the Method class architecture should be revamped
        # such that this information can be derived instead. Possibly: a
        # method component of an object is represented by a MethodNode that has
        # a dictionary from Rank to implementation.
        self.template_method_impls = {}

        self._confidential = False

        # during code generation, set to a unique positive integer,
        # used as object ID, e.g. in the id_infos array.  This integer
        # number is the index into object.Device.objects *plus one*;
        # the plus one is because object ID 0 is reserved in runtime
        # for an invalid reference.
        self.uniq = None

    @property
    def name_site(self):
        return (self._component_dict['name'].site if self.name != self.ident
                else self.site)

    def is_confidential(self):
        return self._confidential

    def accepts_child_type(self, comp_type):
        return comp_type in self.allowed_components

    def add_component(self, comp):
        # dbg('ADDCOMP %r %s %r' % (self, comp.objtype, comp.name))
        # verified by structure.merge_ast()
        if comp.ident:
            if not self._component_dict:
                self._component_dict = {}
            assert comp.ident not in self._component_dict
            self._component_dict[comp.ident] = comp
        self._components.append(comp)

    def idxvars(self):
        if self._idxvars:
            return self.parent.idxvars() + self._idxvars
        else:
            return self.parent.idxvars()

    def logname(self, indices=(), relative='device'):
        if self.isindexed():
            suff = "".join('[%s]' % i for i in
                           indices[-self.local_dimensions():])
            suff += "".join(f'[{dollar(self.site)}{idxvar}]'
                            for idxvar in self._idxvars[len(indices):])
            indices = indices[:-self.local_dimensions()]
        else:
            suff = ''
        name = DMLObject.logname(self, indices, relative)
        return name + suff

    def logname_anonymized(self, indices=(), relative='device'):
        if dml.globals.dml_version != (1, 2):
            return self.logname(indices, relative)

        if self.isindexed():
            suff = "".join('[%s]' % i for i in
                           indices[-self.local_dimensions():])
            suff += "".join('[$%s]' % idxvar for idxvar in
                            self._idxvars[len(indices):])
            indices = indices[:-self.local_dimensions()]
        else:
            suff = ''
        name = DMLObject.logname_anonymized(self, indices, relative)
        return name + suff

    def identity(self, indices=(), relative='device'):
        if self.isindexed():
            suff = "".join('[%s]' % i for i in
                           indices[-self.local_dimensions():])
            suff += "".join(f'[{dollar(self.site)}{idxvar} < {arrlen}]'
                            for (idxvar, arrlen) in
                            itertools.islice(
                                zip(self._idxvars, self._arraylens),
                                len(indices), None))
            indices = indices[:-self.local_dimensions()]
        else:
            suff = ''
        ident = DMLObject.identity(self, indices, relative)
        return ident + suff

    def set_traits(self, traits):
        self.traits = traits
        for t in itertools.chain(*([t] + list(t.ancestors)
                                   for t in traits.ancestors)):
            dml.globals.trait_instances.setdefault(t, Set()).add(self)
            if t in dml.globals.serialized_traits.traits:
                traits.mark_referenced(t)

class Device(CompositeObject):
    __slots__ = ('initdata',)
    objtype = 'device'
    allowed_components = [ 'parameter', 'method', 'saved', 'session', 'bank',
                           'connect', 'attribute', 'event', 'port', 'implement',
                           'group', 'subdevice', 'initdata']
    objects = None
    hooks = None

    def __init__(self, ident, site):
        super(Device, self).__init__(ident, site, None)
        self.initdata = []

    def attrname(self):
        return None

    dimsizes = ()

    def add_init_data(self, node):
        assert isinstance(node, Session)
        assert node.astinit
        self.initdata.append(node)

    def idxvars(self):
        return ()

    @contextlib.contextmanager
    def use_for_codegen(self):
        '''Reusable, but not reentrant, context manager, entered
        during code generation.'''
        assert Device.objects is None
        objs = [self]
        hooks = []
        self.uniq = 1
        for o in self.recursive_components_postorder():
            if isinstance(o, CompositeObject):
                o.uniq = len(objs) + 1
                objs.append(o)
            elif isinstance(o, Hook):
                o.uniq = len(hooks) + 1
                hooks.append(o)
        Device.objects = objs
        Device.hooks = hooks

        try:
            yield
        finally:
            for o in Device.objects:
                o.uniq = None
            Device.objects = None
            for o in Device.hooks:
                o.uniq = None
            Device.hooks = None

class Group(CompositeObject):
    __slots__ = ()
    objtype = 'group'
    groupable_objects = {'register',
                         'attribute',
                         'connect',
                         'event',
                         'field',
                         'subdevice',
                         'bank',
                         'port',
                         'group'}
    def accepts_child_type(self, comp):
        if comp not in self.groupable_objects:
            return False
        return self.parent.accepts_child_type(comp)

class Bank(CompositeObject):
    __slots__ = ('mapped_registers', 'unmapped_registers', 'numbered_registers')
    objtype = 'bank'
    allowed_components = [
        'parameter', 'method', 'saved', 'session', 'group',
        'register', 'event', 'attribute', 'connect', 'implement' ]

class Register(CompositeObject):
    __slots__ = ('wholefield', 'fields', 'readable', 'writable', 'bank')
    objtype = 'register'
    allowed_components = [ 'parameter', 'method', 'saved', 'session', 'field',
                           'event', 'group' ]
    def __init__(self, ident, site, parent, arraylens, idxvars):
        CompositeObject.__init__(self, ident, site, parent,
                                 arraylens, idxvars)
        self.wholefield = None
        self.fields = None
        self.readable = True
        self.writable = True
        bank = parent
        while bank.objtype != "bank":
            bank = bank.parent
        self.bank = bank

    @property
    def simple_storage(self):
        return self.wholefield and \
               not bool(self.get_recursive_components('session', 'saved',
                                                      'hook'))

    @property
    def name_anonymized(self):
        if self.is_confidential() and dml.globals.dml_version == (1, 2):
            # recursive import
            from dml import ctree
            return ctree.get_anonymized_name(self)
        else:
            return super(CompositeObject, self).name_anonymized


class Field(CompositeObject):
    __slots__ = ()
    objtype = 'field'
    allowed_components = [ 'parameter', 'method', 'saved', 'session', 'group' ]
    @property
    def simple_storage(self):
        """Return true if the allocated value is stored in struct member
        __DMLfield."""
        return not bool(self.get_recursive_components('session', 'saved',
                                                      'hook'))

    @property
    def name_anonymized(self):
        if self.is_confidential() and dml.globals.dml_version == (1, 2):
            # recursive import
            from dml import ctree
            return ctree.get_anonymized_name(self)
        else:
            return super(CompositeObject, self).name_anonymized

class Connection(CompositeObject):
    __slots__ = ()
    objtype = 'connect'
    allowed_components = [ 'parameter', 'method', 'saved', 'session',
                           'interface', 'event', 'group' ]
    readable = True
    writable = True

class Interface(CompositeObject):
    __slots__ = ()
    objtype = 'interface'
    allowed_components = [ 'parameter', 'method', 'saved', 'session', 'group' ]

class Attribute(CompositeObject):
    __slots__ = ['readable', 'writable', 'datamember']
    objtype = 'attribute'
    allowed_components = [
        'parameter', 'method', 'saved', 'session', 'event', 'group']
    def __init__(self, ident, site, parent, arraylens, idxvars):
        CompositeObject.__init__(self, ident, site, parent,
                                 arraylens, idxvars)
        self.readable = True
        self.writable = True
        # Used by the System C backend, I don't know what it means.
        self.datamember = False

class Event(CompositeObject):
    __slots__ = ()
    objtype = 'event'
    allowed_components = [ 'parameter', 'method', 'saved', 'session', 'group' ]

class Session(DMLObject):
    __slots__ = ('_type', 'astinit')
    objtype = 'session'
    allowed_components = [ ]
    def __init__(self, name, type, astinit, site, parent):
        DMLObject.__init__(self, name, site, parent)
        self._type = type
        self.astinit = astinit
    def __repr__(self):
        return 'Session(%s,%s)' % (self.name, repr(self._type))
    def copy(self, parent):
        return Session(self.name, self._type, self.astinit, self.site, parent)

class Saved(Session):
    __slots__ = ()
    objtype = 'saved'
    allowed_components = [ ]
    def __init__(self, name, type, astinit, site, parent):
        Session.__init__(self, name, type, astinit, site, parent)
    def __repr__(self):
        return 'Saved(%s,%s)' % (self.name, repr(self._type))
    def copy(self, parent):
        return Saved(self.name, self._type, self.astinit, self.site, parent)

class Hook(DMLObject):
    __slots__ = ('msg_types', '_arraylens', 'uniq')
    objtype = 'hook'
    allowed_components = []

    def __init__(self, name, site, parent, msg_types, arraylens = ()):
        DMLObject.__init__(self, name, site, parent)
        self.msg_types = msg_types
        self._arraylens = arraylens
        self.dimensions += self.local_dimensions()
        # during code generation, set to a unique positive integer
        # (index in object.Device.hooks plus one, see
        # CompositeObject.uniq)
        self.uniq = None

    def logname(self, indices=(), relative='device'):
        if self.isindexed():
            suff = "".join(f'[{i}]' for i in
                           indices[-self.local_dimensions():])
            suff += "".join('[]' for _ in
                            range(self.local_dimensions() - len(indices)))
            indices = indices[:-self.local_dimensions()]
        else:
            suff = ''
        name = DMLObject.logname(self, indices, relative)
        return name + suff

    def logname_anonymized(self, indices=(), relative='device'):
        if dml.globals.dml_version != (1, 2):
            return self.logname(indices, relative)

        if self.isindexed():
            suff = "".join(f'[{i}]' for i in
                           indices[-self.local_dimensions():])
            suff += "".join('[]' for _ in
                            range(self.local_dimensions() - len(indices)))
            indices = indices[:-self.local_dimensions()]
        else:
            suff = ''
        name = DMLObject.logname_anonymized(self, indices, relative)
        return name + suff

class Port(CompositeObject):
    __slots__ = ()
    objtype = 'port'
    allowed_components = [ 'parameter', 'method', 'saved', 'session', 'connect',
                           'event', 'implement', 'attribute', 'group' ]

class Subdevice(CompositeObject):
    __slots__ = ()
    objtype = 'subdevice'
    allowed_components = [ 'parameter', 'method', 'saved', 'session', 'connect',
                           'event', 'implement', 'attribute', 'group',
                           'port', 'bank', 'subdevice' ]

class Implement(CompositeObject):
    __slots__ = ()
    objtype = 'implement'
    allowed_components = [ 'parameter', 'method', 'saved', 'session',
                           'group', 'attribute' ]

class MethodDefault(metaclass=abc.ABCMeta):
    '''Represents the methods a method overrides'''
    slots = ()
    # This one is needed by the g backend, in order to find all
    # MethodFunc instances.
    @abc.abstractproperty
    def node(self):
        '''Method object, if there is a unique non-trait default method.
        None otherwise.'''
    @abc.abstractmethod
    def default_sym(self, site, indices):
        'Return the symtab.Symbol object to use for the "default" identifier'

class Method(DMLObject):
    objtype = 'method'
    __slots__ = ('inp', 'outp', 'throws', 'independent', 'startup', 'memoized',
                 'astcode', 'fully_typed', 'funcs', 'refcount',
                 'default_method', 'rbrace_site')
    def __init__(self, name, site, parent, inp, outp, throws, independent,
                 startup, memoized, astcode, default_method, rbrace_site):
        DMLObject.__init__(self, name, site, parent)
        self.inp = tuple(inp)
        self.outp = tuple(outp)
        self.throws = throws
        self.independent = independent
        self.startup = startup
        self.memoized = memoized
        self.astcode = astcode
        # A flag indicating whether all parameters have types
        self.fully_typed = all(t for p, t in self.inp + self.outp)
        # MethodDefault instance
        self.default_method = default_method
        self.rbrace_site = rbrace_site

        # A dictionary mapping signatures to implementations.  For
        # methods where fully_typed=True, the only key will be None.
        self.funcs = {}

        self.refcount = 0

class ParamExpr(metaclass=abc.ABCMeta):
    '''If a DMLParameter resides inside an object array, its value may
    depend on an index variable. Therefore, parameter values are
    stored as expression factories rather than expressions.
    '''
    __slots__ = ()
    @abc.abstractmethod
    def mkexpr(self, indices): pass
    @abc.abstractproperty
    def site(self): pass

class DMLParameter(DMLObject):
    objtype = 'parameter'
    __slots__ = ('_mkexpr',)

    def __init__(self, name, site, parent, mkexpr):
        DMLObject.__init__(self, name, site, parent)
        assert isinstance(mkexpr, ParamExpr)
        self._mkexpr = mkexpr

    def __repr__(self):
        parent = self.parent.name if self.parent else None
        return '%s(%r, %r, %r)' % (self.__class__.__name__, self.name, parent,
                                   self._mkexpr)

    def copy(self, newparent):
        return DMLParameter(self.name, self.site, newparent, self._mkexpr)

    def set_expr(self, mkexpr):
        assert isinstance(mkexpr, ParamExpr)
        self._mkexpr = mkexpr

    def get_expr(self, indices):
        '''Return the parameter's expression, evaluated at the given indices.'''
        return self._mkexpr.mkexpr(indices)

    def expr_site(self):
        return self._mkexpr.site or self.site
