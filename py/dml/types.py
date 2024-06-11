# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

# Types in DML

__all__ = (
    'DMLTypeError',
    'DMLUnknownType',
    'DMLUnkeyableType',
    'parse_type',
    'realtype_shallow',
    'realtype',
    'safe_realtype_shallow',
    'safe_realtype',
    'safe_realtype_unconst',
    'conv_const',
    'shallow_const',
    'deep_const',
    'type_union',
    'compatible_types',
    'compatible_types_fuzzy',
    'typedefs',
    'global_type_declaration_order',
    'global_anonymous_structs',
    'add_late_global_struct_defs',
    'TypeSequence',
    'DMLType',
    'TVoid',
    'TUnknown',
    'TDevice',
    'TNamed',
    'IntegerType',
    'TBool',
    'TInt',
    'TEndianInt',
    'TLong',
    'TSize',
    'TFloat',
    'TArray',
    'TPtr',
    'TVector',
    'TTrait',
    'TTraitList',
    'StructType',
    'TExternStruct',
    'TStruct',
    'TLayout',
    'TFunction',
    'THook',
    'cident',
    'void',
)

import sys
import re
from itertools import *

from .env import is_windows
from .output import out
from .messages import *
from .logging import *
from . import compat
from . import output
import dml .globals
import abc

class DMLTypeError(Exception): pass

class DMLUnknownType(DMLTypeError):
    def __init__(self, t):
        DMLTypeError.__init__(self)
        self.type = t
    def __str__(self):
        return "Unknown type: %s" % (self.type,)

class DMLUnkeyableType(DMLTypeError):
    def __init__(self, t, clarification=None):
        DMLTypeError.__init__(self)
        self.type = t
        self.clarification = clarification
    def __str__(self):
        return "Unkeyable type %s%s" % (self.type,
                                         (': ' + self.clarification)
                                         if self.clarification else '')

# Types that are defined in DML, as list of type names, in the order
# they need to be declared in C.
global_type_declaration_order = []
# anonymous structs used by global typedefs, label -> type
global_anonymous_structs = {}

# Convert named types to what they really are
def realtype_shallow(t):
    "Lookup a named type"
    #assert isinstance(t, DMLType)
    seen = set()
    while isinstance(t, TNamed):
        if t in seen:
            raise ICE(t.declaration_site,
                       "recursive type definition of %r" % t)
        seen.add(t)
        t2 = typedefs.get(t.c)

        if not t2:
            raise DMLUnknownType(t)
        if t.const and not t2.const:
            if isinstance(t2, TFunction):
                raise ECONSTFUN(t.declaration_site)
            t = t2.clone()
            t.const = True
        else:
            t = t2
    return t

def realtype(t):
    t = realtype_shallow(t)

    if isinstance(t, TPtr):
        t2 = realtype(t.base)
        if t2 != t:
            return TPtr(t2, t.const)
    if isinstance(t, TTraitList):
        # in 'sequence(t)' and 'each t in (...)', t refers to the
        # template name rather than the type; this is a misplaced check
        # that there is a template named t.
        # (it would be natural to check this in eval_type, but
        # we run eval_type on typed parameters before globals.templates is
        # populated
        if (t.traitname not in dml.globals.templates
            or not dml.globals.templates[t.traitname].trait):
            raise DMLUnknownType(t)
    elif isinstance(t, TArray):
        t2 = realtype(t.base)
        if t2 != t:
            return TArray(t2, t.size, t.const)
    elif isinstance(t, TVector):
        t2 = realtype(t.base)
        if t2 != t:
            return TVector(t2, t.const, t.uniq)
    elif isinstance(t, TFunction):
        input_types = tuple(realtype(sub) for sub in t.input_types)
        output_type = realtype(t.output_type)
        if input_types != t.input_types or output_type != t.output_type:
            return TFunction(input_types, output_type, t.varargs, t.const)
    elif isinstance(t, THook):
        msg_types = tuple(realtype(sub) for sub in t.msg_types)
        if msg_types != t.msg_types:
            return THook(msg_types, t.validated, t.const)

    return t

def safe_realtype(t):
    try:
        return realtype(t)
    except DMLUnknownType as e:
        raise ETYPE(e.type.declaration_site or None, e.type)

def safe_realtype_shallow(t):
    try:
        return realtype_shallow(t)
    except DMLUnknownType as e:
        raise ETYPE(e.type.declaration_site or None, e.type)

def conv_const(const, t):
    # Functions cannot be const. Usually function types cannot happen
    # where conv_const is called, but if they can, then that deserves
    # that the caller handles it explicitly.
    assert not isinstance(t, TFunction)
    if const and not t.const:
        t = t.clone()
        t.const = True
    return t

def safe_realtype_unconst(t0):
    def sub(t):
        if isinstance(t, (TArray, TVector)):
            base = sub(t.base)
            if t.const or base is not t.base:
                t = t.clone()
                t.const = False
                t.base = base
        elif t.const:
            t = t.clone()
            t.const = False
        return t
    return sub(safe_realtype(t0))

def shallow_const(t):
    t = safe_realtype_shallow(t)
    while not t.const and isinstance(t, (TArray, TVector)):
        t = safe_realtype_shallow(t.base)

    return t.const

def deep_const(origt):
    subtypes = [origt]
    while subtypes:
        st = safe_realtype_shallow(subtypes.pop())
        if st.const:
            return True
        if isinstance(st, (TArray, TVector)):
            subtypes.append(st.base)
        elif isinstance(st, StructType):
            subtypes.extend(st.members.values())
        # TODO This should be added once the types of bitfields member are
        # respected by subreferences to them (SIMICS-18394 and SIMICS-8857).
        # elif st.is_int and st.is_bitfields:
        #     subtypes.extend(typ for (typ, _, _) in st.members.values())

    return False

class TypeSequence:
    '''A sequence of types, meant to be used as keys for dictionaries.'''
    def __init__(self, types):
        self.types = tuple(safe_realtype(t) for t in types)

    def __eq__(self, other):
        if not isinstance(other, TypeSequence):
            return NotImplemented

        return (len(self.types) == len(other.types)
                and all(typ_self.cmp(typ_other) == 0
                        for (typ_self, typ_other)
                        in zip(self.types, other.types)))

    def __hash__(self):
        return hash(tuple(elem.hashed() for elem in self.types))

class DMLType(metaclass=abc.ABCMeta):
    '''The type of a value (expression or declaration) in DML. One DML
    type always corresponds to a C type in generated code, but
    different DML types can correspond to the same C type.'''
    __slots__ = ('const', 'declaration_site')
    void = False
    # shorthand for isinstance(x, IntegerType)
    is_int = False
    # shorthand for isinstance(x, TFloat)
    is_float = False
    # shorthand for (is_int or is_float)
    is_arith = False

    def __init__(self, const = False):
        self.const = const
        self.declaration_site = None

    @property
    def const_str(self):
        if self.const:
            return "const "
        else:
            return ""

    def sizeof(self):
        '''Return size, or None if not known'''
        return None

    def cmp(self, other):
        """Strict type compatibility/equality.

        Return 0 if the types are run-time compatible,
        Return NotImplemented otherwise

        "Run-time compatibility" has two minimal criteria:
        1. The C representations of the types MUST be compatible, in a C sense
        2. A value of one type can be treated by DMLC as though it were of the
        other type without any additional risk of undefined behavior or invalid
        generated C.
        For example, all trait reference types share the same C representation,
        and so satisfy (1), but trait reference types for different traits do
        not share vtables; trying to use a vtable for one trait with an
        incompatible reference would result in undefined behavior, and so do
        not satisfy (2).

        The method is used for three purposes:
        1. in TPtr.canstore(), to judge whether pointer target types
        are compatible.
        2. in ctree, to compare how large values different numerical
        types can hold
        3. when judging whether a method override is allowed

        See SIMICS-9504 for further discussions.
        """
        return (0 if type(self) is type(other) and self.const == other.const
                else NotImplemented)

    def cmp_fuzzy(self, other):
        """Compare this type to another.
        Return 0 if the types are pretty much equivalent,
        Return NotImplemented otherwise.
        As implied, the exact meaning of this is fuzzy. It mostly relaxes
        criteria (1) of 'cmp'); for example, TPtr(void).cmp(TPtr(TBool())) is
        allowed to return 0, as is TPtr(TBool()).cmp(TArray(TBool())).

        Most notably, cmp_fuzzy does not take const-qualification into account.

        Any usage of cmp_fuzzy should be considered a HACK
        """
        return safe_realtype_unconst(self).cmp(safe_realtype_unconst(other))

    def canstore(self, other):
        """Can a variable of this type store a value of another type.

        The return value is a tuple (ok,trunc,const) where 'ok' is the
        boolean result.  The 'trunc' is a (unused) flag that indicates a
        possible truncation, and the 'const' flag indicates that there
        would be a const violation.

        The correctness of the return value can not be trusted; see
        SIMICS-9504 for further discussions.
        """
        return (safe_realtype_unconst(self).cmp(safe_realtype_unconst(other))
                == 0, False, False)

    @abc.abstractmethod
    def clone(self):
        """Return a shallow copy of this type object."""
        pass

    def declaration(self, var):
        raise Exception("not implemented")
    def print_declaration(self, var, init = None, unused = False):
        out("%s%s%s;\n" % (self.declaration(var),
                           " UNUSED " if unused else "",
                           (" = " + init) if init else ""))

    def describe(self):
        raise Exception("%s.describe not implemented" % self.__class__.__name__)

    def hashed(self):
        '''Hash the DML type in a way compatible with cmp. I.e.
           a.cmp(b) == 0 implies a.hashed() == b.hashed()'''
        assert type(self).cmp is DMLType.cmp, \
               '.cmp() overridden without overriding .hashed()'
        return hash((type(self), self.const))

    def key(self):
        return self.const_str + self.describe()

    def describe_assign_types(self):
        """Describe the types that can be used to assign to a variable
        of this type."""
        return self.describe()

    def __str__(self):
        # return self.declaration('')
        return self.describe()

    def resolve(self):
        """Finish the definition by using information in other types and
           return self"""
        return self

class TVoid(DMLType):
    __slots__ = ()
    void = True
    def __init__(self):
        DMLType.__init__(self)
    def __repr__(self):
        return 'TVoid()'
    def describe(self):
        return 'void'
    def declaration(self, var):
        return 'void ' + self.const_str + ' ' + var
    def clone(self):
        return TVoid()

class TUnknown(DMLType):
    '''A type unknown to DML. Typically used for a generic C macro
    imported to DML with an untyped 'extern' declaration, where some
    argument or return value cannot be simply expressed as a C type.

    Available only in DML 1.2'''
    __slots__ = ()
    def __init__(self):
        assert dml.globals.dml_version == (1, 2)
        DMLType.__init__(self)
    def describe(self):
        return "<unknown type>"
    def clone(self):
        raise ICE(self.declaration_site, "cannot clone unknown type")

class TDevice(DMLType):
    """The type of the device object

    This is the type of $dev. No other values have this type.
    """
    __slots__ = ('name',)
    def __init__(self, name, const=False):
        DMLType.__init__(self, const)
        self.name = name
    def __repr__(self):
        return 'TDevice(%s)' % repr(self.name)
    def c_name(self):
        return f'{self.name} *{self.const_str}'
    def describe(self):
        return 'pointer to %s' % self.name
    def key(self):
        return 'device'
    def canstore(self, other):
        constviol = False
        if not self.const and other.const:
            constviol = True
        if isinstance(other, TDevice):
            return (True, False, constviol)
        return (False, False, constviol)
    def clone(self):
        return TDevice(self.name, self.const)
    def declaration(self, var):
        return f'{self.c_name()}{var}'

# Hack: some identifiers that are valid in DML are not allowed in C,
# either because they are reserved words or because they clash with
# predefined C identifiers. We need to rename these when generating code.

# The main problem is trait names. We currently apply renaming on the
# following identifiers:
# - type names
# - struct tags
# - parent fields in trait vtable structs
# - vtable argument names in vtable methods
cident_renames = {
    'connect': '_connect',
    # both read and _read are predefined on Windows
    'read': '__read',
    'register': '_register',
    'write': '__write',
    'hard_reset': '_hard_reset',
    'soft_reset': '_soft_reset',
    'interface': '_interface',
}

def cident(name):
    # One alternative would be to prefix a string to all identifiers.
    # Extern typedefs would have to be excluded, though.
    return cident_renames.get(name, name)

class TNamed(DMLType):
    __slots__ = ('c',)
    def __init__(self, name, const = False):
        DMLType.__init__(self, const)
        # Type name in DML
        self.c = name
        assert name != 'void'

    def __repr__(self):
        return '%s(%r,%r)' % (self.__class__.__name__, self.c, self.const)
    def describe(self):
        return self.c
    def key(self):
        raise ICE(self.declaration_site, 'need realtype before key')
    def cmp(self, other):
        assert False, 'need realtype before cmp'
    def cmp_fuzzy(self, other):
        assert False, 'need realtype before cmp_fuzzy'
    def hashed(self):
        assert False, 'need realtype before hashed'

    def clone(self):
        return TNamed(self.c, self.const)

    def declaration(self, var):
        return cident(self.c) + ' ' + self.const_str + var

class TBool(DMLType):
    __slots__ = ()
    def __init__(self):
        DMLType.__init__(self)

    def __repr__(self):
        return 'TBool(%r)' % self.const
    def describe(self):
        return 'bool'
    def declaration(self, var):
        return 'bool ' + self.const_str + var

    def canstore(self, other):
        constviol = False
        if type(other) is TBool:
            return (True, False, constviol)
        if (other.is_int
            and other.bits == 1 and not other.signed):
            return (True, False, constviol)
        return (False, False, constviol)
    def clone(self):
        return TBool()

class IntegerType(DMLType):
    '''Type that can contain an integer value
    bits is the bitsize of the type
    signed is whether the integer is signed
    members are bitfield accessors
    '''
    __slots__ = ('bits', 'signed', 'members')
    def __init__(self, bits, signed, members = None, const = False):
        DMLType.__init__(self, const)
        self.bits = bits
        assert isinstance(signed, bool)
        if members is not None:
            assert all(isinstance(m, TInt) for (m, _, _) in members.values())
            assert not signed
        self.signed = signed
        self.members = members

    is_int = True
    is_arith = True
    is_endian = False
    is_arch_dependent = False

    @property
    def is_bitfields(self):
        return self.members is not None

    @property
    def bytes(self):
        return (self.bits - 1) // 8 + 1
    def sizeof(self):
        return self.bytes

    @property
    def members_qualified(self):
        assert(self.is_bitfields)
        return ((name, conv_const(self.const, typ), msb, lsb)
                for (name, (typ, msb, lsb)) in self.members.items())

    def get_member_qualified(self, member):
        t = self.members.get(member)
        if t is not None:
            t = (conv_const(self.const, t[0]),) + t[1:]
        return t

    def cmp(self, other):
        if self.const != other.const:
            return NotImplemented
        if not other.is_int:
            return NotImplemented
        if ((self.is_arch_dependent or other.is_arch_dependent)
            and type(self) is not type(other)):
            return NotImplemented
        if self.is_endian:
            if not other.is_endian:
                return NotImplemented
            if self.byte_order != other.byte_order:
                return NotImplemented
        elif other.is_endian:
            return NotImplemented
        return (0 if (self.bits, self.signed) == (other.bits, other.signed)
                else NotImplemented)

    def cmp_fuzzy(self, other):
        if not other.is_int:
            return NotImplemented
        if ((self.is_arch_dependent or other.is_arch_dependent)
            and type(self) is not type(other)):
            return NotImplemented
        if self.is_endian:
            if not other.is_endian:
                return NotImplemented
            if self.byte_order != other.byte_order:
                return NotImplemented
        elif other.is_endian:
            return NotImplemented
        if (dml.globals.dml_version == (1, 2)
            and compat.dml12_int in dml.globals.enabled_compat):
            # Ignore signedness
            return 0 if self.bits == other.bits else NotImplemented
        else:
            return (0 if (self.bits, self.signed) == (other.bits, other.signed)
                    else NotImplemented)

    def hashed(self):
        cls = type(self) if self.is_arch_dependent else IntegerType
        byte_order = self.byte_order if self.is_endian else None
        return hash((cls, self.const, self.bits, self.signed, byte_order))

    # This is the most restrictive canstore definition for
    # IntegerTypes, if this is overridden then it should be
    # because we want to be less restrictive
    def canstore(self, other):
        if other.is_int:
            return (True, self.bits < other.bits, False)
        else:
            return (False, False, False)

class TInt(IntegerType):
    '''An integer. In a declaration, the corresponding C type is the
    smallest type with the same sign, in which the type fits.

    When looking at the types of rvalues, it is slightly unclear how
    TInt maps to C integers. It seems that the bitsize of the TInt of
    an expression maintains an approximate upper bound of the possible
    values of the expression, and that the corresponding C type is (often)
    the first of [int32, uint32, int64, uint64] in which the DML type fits.

    DMLC does not seem to maintain consistency between signedness of
    TInt and the corresponding C type. This seldom makes a difference,
    because it means that C's operational semantics is what's used in
    practice.
    '''
    __slots__ = ()
    def __init__(self, bits, signed, members = None, const = False):
        IntegerType.__init__(self, bits, signed, members, const)

    def describe(self):
        s = 'int%d' % self.bits
        if self.signed:
            return s
        else:
            return 'u' + s
    def __repr__(self):
        return 'TInt(%r,%r,%r,%r)' % (self.bits, self.signed,
                                      self.members, self.const)

    def apitype(self):
        "Return the smallest API type that can represent the type"
        if self.bits == 0:
            #raise(ICE(None, "zero-bit integer"))
            return 'void'
        elif self.bits > 64:
            if self.signed:
                raise DMLTypeError("Can't sign large registers")
            if self.bits % 8:
                raise DMLTypeError("Uneven number of self.bits: "
                                   + str(self.bits))
            return None
        else:
            t = "int"
            if self.bits > 32: t += '64'
            elif self.bits > 16: t += '32'
            elif self.bits > 8: t += '16'
            else: t += '8'
            if not self.signed: t = 'u' + t
            return t

    def canstore(self, other):
        constviol = False
        other = realtype(other)
        if other.is_int:
            trunc = (other.bits > self.bits)
            if (compat.dml12_misc in dml.globals.enabled_compat
                and isinstance(other, TBool)):
                return (False, False, constviol)
            return (True, trunc, constviol)
        if other.is_float and not self.is_bitfields:
            return (True, True, constviol)
        return (False, False, constviol)

    def clone(self):
        return TInt(self.bits, self.signed, self.members, self.const)

    def declaration(self, var):
        t = self.apitype()
        if t:
            # return '%s /* %s bits */ %s' % (t, self.bits, var)
            return '%s %s%s' % (t, self.const_str, var)
        else:
            return 'uint8 ' + self.const_str + var + '[' + str(self.bytes) + ']'

class ArchDependentIntegerType(IntegerType):
    '''Integer types whose definition and/or properties are architecture
       dependent.'''
    __slots__ = ()
    is_arch_dependent = True

class TLong(ArchDependentIntegerType):
    '''The 'long' type from C'''
    __slots__ = ()
    def __init__(self, signed, const=False):
        IntegerType.__init__(self, 32 if is_windows() else 64, signed,
                             const=const)

    def c_name(self):
        return self.const_str + ('long' if self.signed else 'unsigned long')

    def describe(self):
        return self.c_name()

    def __repr__(self):
        return 'TLong(%r, %r)' % (self.signed, self.const)

    def clone(self):
        return TLong(self.signed, self.const)

    def declaration(self, var):
        return f'{self.c_name()} {var}'

class TSize(ArchDependentIntegerType):
    '''The 'size_t' type from C'''
    __slots__ = ()
    def __init__(self, signed, const=False):
        IntegerType.__init__(self, 64, signed, const=const)

    def c_name(self):
        return self.const_str + ('ssize_t' if self.signed else 'size_t')

    def describe(self):
        return self.c_name()

    def __repr__(self):
        return 'TSize(%r, %r)' % (self.signed, self.const)

    def clone(self):
        return TSize(self.signed, self.const)

    def declaration(self, var):
        return f'{self.c_name()} {var}'

class TInt64_t(ArchDependentIntegerType):
    '''The '[u]int64_t' type from ISO C. For compatibility with C
    APIs, e.g., calling an externally defined C function that takes a
    `uint64_t *` arg. We find `uint64` a generally more useful type
    primarily because it works better with format strings; "%lld" works
    for `uint64` on both linux64 and win64.
    '''
    __slots__ = ()
    def __init__(self, signed, const=False):
        IntegerType.__init__(self, 64, signed, const=const)

    def c_name(self):
        name = 'int64_t' if self.signed else 'uint64_t'
        return f'const {name}' if self.const else name

    def describe(self):
        return self.c_name()

    def __repr__(self):
        return 'TInt64_t(%r, %r)' % (self.signed, self.const)

    def clone(self):
        return TInt64_t(self.signed, self.const)

    def declaration(self, var):
        return f'{self.c_name()} {var}'

class TEndianInt(IntegerType):
    '''An integer where the byte storage order is defined.
    Corresponds to the (u)?intX_[be|le] family of types defined in
    dmllib.h
    '''
    __slots__ = ('byte_order')
    def __init__(self, bits, signed, byte_order, members = None, const = False):
        IntegerType.__init__(self, bits, signed, members, const)
        if (bits % 8 != 0):
            raise DMLTypeError("Trying to create endian int without whole "
                               "byte size")
        assert(byte_order in ('big-endian', 'little-endian'))
        self.byte_order = byte_order
    is_endian = True

    @property
    def big_endian(self):
        return self.byte_order == 'big-endian'

    def c_name(self):
        return '%s%sint%d_%s_t' % (
            self.const_str, "" if self.signed else "u", self.bits,
            "be" if self.big_endian else "le")

    def describe(self):
        return self.c_name()

    def __repr__(self):
        return 'TEndianInt(%r,%r,%r,%r,%r)' % (
            self.bits, self.signed, self.byte_order, self.members, self.const)

    @property
    def access_type(self):
        """Integer type used for read/write access"""
        return TInt(64, self.signed or self.bits != 64)

    def dmllib_fun(self, fun):
        """translate a function name to the c dmllib function"""
        return "dml_%s_%sint%d_%s_t" % (fun, ("u", "")[self.signed],
                                        self.bits,
                                        ("le", "be")[self.big_endian])
    @property
    def dmllib_store(self):
        return self.dmllib_fun("store")
    @property
    def dmllib_load(self):
        return self.dmllib_fun("load")

    def get_store_fun(self):
        """function reference to dmllib function used to store values to an
        endianint"""
        return (self.dmllib_store,
                TFunction([self.access_type], self))

    def get_load_fun(self):
        """function reference to dmllib function used to load values from an
        endianint"""
        return (self.dmllib_load,
                TFunction([self], self.access_type))

    def clone(self):
        return TEndianInt(self.bits, self.signed,
                          self.byte_order, self.members, self.const)

    def declaration(self, var):
        return f'{self.c_name()} {var}'

class TFloat(DMLType):
    __slots__ = ('name',)
    is_float = True
    is_arith = True
    def __init__(self, name, const = False):
        DMLType.__init__(self, const)
        self.name = name
    def __repr__(self):
        return '%s(%r,%r)' % (self.__class__.__name__, self.name, self.const)
    def describe(self):
        return self.name
    def cmp(self, other):
        if (self.const == other.const
            and other.is_float and self.name == other.name):
            return 0
        return NotImplemented

    def hashed(self):
        return hash((TFloat, self.const, self.name))

    def canstore(self, other):
        constviol = False
        if other.is_float:
            return (True, False, constviol)
        if isinstance(other, TInt):
            return (True, True, constviol)
        return (False, False, constviol)

    def declaration(self, var):
        return self.name + ' ' + self.const_str + var
    def clone(self):
        return TFloat(self.name, self.const)

class TArray(DMLType):
    __slots__ = ('base', 'size')
    def __init__(self, base, size, const = False):
        DMLType.__init__(self, const)
        # size must be an expression
        assert hasattr(size, 'read')
        self.base = base
        self.size = size
    def __repr__(self):
        return "TArray(%r,%r,%r)" % (self.base, self.size, self.const)
    def key(self):
        if not self.size.constant:
            raise DMLUnkeyableType(self, "array of non-constant size")
        return ('array(%s)[%s]'
                % (conv_const(self.const, self.base).key(),
                   self.size.value))
    def describe(self):
        return 'array of size %s of %s' % (str(self.size),
                                           self.base.describe())
    def declaration(self, var):
        return self.base.declaration(self.const_str + var
                                     + '[' + self.size.read() + ']')

    def print_declaration(self, var, init = None, unused = False):
        # VLA:s may not have an initializer
        assert not init or self.size.constant
        DMLType.print_declaration(self, var, init, unused)

    def sizeof(self):
        if not self.size.constant:
            # variable-sized array, sizeof is not known
            return None
        elt_size = self.base.sizeof()
        if elt_size == None:
            return None
        return self.size.value * elt_size

    def cmp(self, other):
        if not isinstance(other, TArray):
            return NotImplemented
        if not (self.size is other.size
                or (self.size.constant and other.size.constant
                    and self.size.value == other.size.value)):
            return NotImplemented
        return conv_const(self.const, self.base).cmp(
            conv_const(other.const, other.base))

    def cmp_fuzzy(self, other):
        if isinstance(other, (TArray, TPtr)):
            if other.base.void:
                return 0
            return self.base.cmp_fuzzy(other.base)
        return NotImplemented

    def hashed(self):
        size = self.size.value if self.size.constant else self.size
        return hash((TArray,
                     size,
                     conv_const(self.const, self.base).hashed()))

    def canstore(self, other):
        return (False, False, False)
    def clone(self):
        return TArray(self.base, self.size, self.const)
    def resolve(self):
        self.base.resolve()
        return self

class TPtr(DMLType):
    __slots__ = ('base',)
    def __init__(self, base, const = False):
        DMLType.__init__(self, const)
        if not isinstance(base, DMLType):
            raise DMLTypeError("base is not a type: %r" % (base,))
        self.base = base
    def __repr__(self):
        return "TPtr(%r,%r)" % (self.base, self.const)
    def key(self):
        return f'{self.const_str}pointer({self.base.key()})'
    def describe(self):
        return 'pointer to %s' % (self.base.describe())
    def cmp(self, other):
        if DMLType.cmp(self, other) != 0:
            return NotImplemented
        return self.base.cmp(other.base)

    def cmp_fuzzy(self, other):
        if isinstance(other, (TPtr, TArray)):
            if self.base.void or other.base.void:
                return 0
            return self.base.cmp_fuzzy(other.base)
        return NotImplemented

    def hashed(self):
        return hash((TPtr, self.const, self.base.hashed()))

    def canstore(self, other):
        ok = False
        trunc = False
        constviol = False
        if isinstance(other, (TPtr, TArray)):
            constviol = (not shallow_const(self.base)
                         and shallow_const(other.base))
            if self.base.void or other.base.void:
                ok = True
            else:
                unconst_self_base = safe_realtype_unconst(self.base)
                unconst_other_base = safe_realtype_unconst(other.base)

                ok = ((unconst_self_base.cmp_fuzzy
                       if compat.dml12_int in dml.globals.enabled_compat
                       else unconst_self_base.cmp)(unconst_other_base)
                      == 0)
        elif isinstance(other, TFunction):
            ok = safe_realtype_unconst(self.base).cmp(other) == 0
        # TODO gate this behind dml.globals.dml_version == (1, 2) or
        # dml12_misc?
        if self.base.void and isinstance(other, TDevice):
            ok = True
        #dbg('TPtr.canstore %r %r => %r' % (self, other, ok))
        return (ok, trunc, constviol)

    def clone(self):
        return TPtr(self.base, self.const)

    def declaration(self, var):
        if isinstance(self.base, (TFunction, TArray)):
            var = f'(*{self.const_str}{var})'
        else:
            var = f'*{self.const_str}{var}'
        return self.base.declaration(var)
    def resolve(self):
        self.base.resolve()
        return self

class TVector(DMLType):
    count = 0
    __slots__ = ('base', 'uniq',)
    def __init__(self, base, const=False, uniq=None):
        DMLType.__init__(self, const)
        if uniq is None:
            uniq = TVector.count
            TVector.count += 1
        self.uniq = uniq
        if not base:
            raise DMLTypeError("Null base")
        self.base = base
    def __repr__(self):
        return "TVector(%r,%r)" % (self.base, self.const)
    def key(self):
        return f'{self.const_str}vector({self.base.key()})'
    def describe(self):
        return 'vector of %s' % self.base.describe()
    def cmp(self, other):
        return (0 if (DMLType.cmp(self, other) == 0
                      and self.uniq == other.uniq)
                else NotImplemented)
    def cmp_fuzzy(self, other):
        if isinstance(other, TVector):
            # Can only compare for voidness or equality
            if self.base.void or other.base.void:
                return 0
            return self.base.cmp_fuzzy(other.base)
        return NotImplemented
    def hashed(self):
        return hash((TVector, self.const, self.uniq))
    def clone(self):
        return TVector(self.base, self.const, self.uniq)
    def declaration(self, var):
        s = self.base.declaration('')
        return 'VECT(%s) %s%s' % (s, self.const_str, var)

class TTrait(DMLType):
    '''A run-time reference to a trait. Represented in C as a pointer
    to a trait vtable struct, together with an object identity'''
    __slots__ = ('trait',)

    def __init__(self, trait, const=False):
        DMLType.__init__(self, const)
        self.trait = trait

    def __repr__(self):
        return "TTrait(%s)" % (self.trait.name,)

    def clone(self):
        return TTrait(self.trait)

    def cmp(self, other):
        return (0 if (DMLType.cmp(self, other) == 0
                      and self.trait is other.trait)
                else NotImplemented)

    def key(self):
        return f'{self.const_str}trait({self.trait.name})'

    def hashed(self):
        return hash((TTrait, self.const, self.trait))

    def c_name(self):
        return f'{self.const_str}{cident(self.trait.name)}'

    def describe(self):
        return 'trait ' + self.trait.name

    def declaration(self, var):
        return f'{self.c_name()} {var}'

class TTraitList(DMLType):
    __slots__ = ('traitname')

    def __init__(self, traitname, const=False):
        DMLType.__init__(self, const)
        self.traitname = traitname

    def __repr__(self):
        return "TTraitList(%s)" % (self.traitname,)

    def clone(self):
        return TTraitList(self.traitname, self.const)

    def cmp(self, other):
        return (0 if (DMLType.cmp(self, other) == 0
                      and self.traitname == other.traitname)
                else NotImplemented)

    def key(self):
        return f'{self.const_str}sequence({self.traitname})'

    def hashed(self):
        return hash((TTraitList, self.const, self.traitname))

    def c_type(self):
        return f'{self.const_str}_each_in_t'

    def describe(self):
        return 'list of trait ' + self.traitname

    def declaration(self, var):
        # 'each in' expressions are currently the only way to produce
        # a trait list. The trait type is only visible to DML; in the
        # C representation, a void pointer is used and the type
        # information is discarded.
        return f'{self.c_type()} {var}'

class StructType(DMLType):
    '''common superclass for DML-defined structs and extern structs'''
    __slots__ = ('members',)
    def __init__(self, members, const):
        super(StructType, self).__init__(const)
        self.members = members

    @property
    def members_qualified(self):
        return ((name, conv_const(self.const, typ))
                for (name, typ) in self.members.items())

    def get_member_qualified(self, member):
        t = self.members.get(member)
        return t if t is None else conv_const(self.const, t)

class TExternStruct(StructType):
    '''A struct-like type defined by code outside DMLC's control.
    'members' is the potential right operands of binary '.',
    and 'label' is the typedef:ed type name.'''
    __slots__ = ('typename', 'id')
    count = 0

    def __init__(self, members, id, typename=None, const=False):
        super(TExternStruct, self).__init__(members, const)
        # unique object (wrt ==) representing this type in type comparisons
        # integer for anonymous structs, string for named types
        self.id = id
        # if not None, the struct can be instantiated using that name.
        self.typename = typename

    def __repr__(self):
        return 'TExternStruct(%r,%r,%r,%r)' % (
            self.members, self.id, self.typename, self.const)

    @staticmethod
    def unique_id():
        TExternStruct.count += 1
        return TExternStruct.count

    def key(self):
        if not self.typename:
            raise DMLUnkeyableType(self, 'anonymous struct')
        # TODO consider dropping the explicit 'externstruct'
        return self.const_str + f'externstruct {self.typename}'

    def describe(self):
        return 'extern struct' + (' ' + self.typename if self.typename else '')

    def declaration(self, var):
        if not self.typename:
            raise EANONEXT(self.declaration_site)
        return "%s %s%s" % (self.typename, self.const_str, var)

    def cmp(self, other):
        return (0 if (DMLType.cmp(self, other) == 0
                      and self.id == other.id)
                else NotImplemented)

    def hashed(self):
        return hash((TExternStruct, self.const, self.id))

    def clone(self):
        return TExternStruct(self.members, self.id, self.typename, self.const)

def add_late_global_struct_defs(decls):
    TStruct.late_global_struct_defs.extend((site, t.resolve())
                                           for (site, t) in decls)

class TStruct(StructType):
    __slots__ = ('label', 'anonymous')
    # Anonymous struct types defined in global scope, but outside typedef
    # declarations, e.g. 'session struct { int x; } y;'.
    # These types can depend on typedefs, but typedefs cannot depend on them.
    late_global_struct_defs = []
    num_anon_structs = 0
    def __init__(self, members, label=None, const=False):
        assert members is None or members
        self.anonymous = label is None
        if self.anonymous:
            label = '_anon_struct_%d' % (TStruct.num_anon_structs,)
            TStruct.num_anon_structs += 1
        self.label = label
        super().__init__(members, const)

    def __repr__(self):
        return 'TStruct(%r,%r,%r)' % (self.members, self.label, self.const)

    def key(self):
        if self.anonymous:
            raise DMLUnkeyableType(self, 'anonymous struct')
        # TODO consider dropping the explicit 'struct'
        return f"{self.const_str}struct {self.label}"

    def describe(self):
        return 'struct %s' % (self.label,)

    def declaration(self, var):
        assert self.members is not None
        return "struct %s %s%s" % (self.label,
                                   self.const_str,
                                   var)

    def print_struct_definition(self):
        output.site_linemark(self.declaration_site)
        out("struct %s {\n" % (cident(self.label),), postindent = 1)
        for (n, t) in self.members.items():
            output.site_linemark(t.declaration_site)
            t.print_declaration(n)
        output.site_linemark(self.declaration_site)
        out("};\n", preindent = -1)

    def cmp(self, other):
        return (0 if (DMLType.cmp(self, other) == 0
                      and self.label == other.label)
                else NotImplemented)

    def hashed(self):
        return hash((TStruct, self.const, self.label))

    def clone(self):
        return TStruct(self.members, self.label, self.const)

class TLayout(TStruct):
    __slots__= ('endian', 'member_decls', 'size')

    def __init__(self, endian, member_decls, label=None, const=False):
        # Intentionally wait with setting member types until
        # resolve is finished
        super(TLayout, self).__init__(None, label, const)
        self.member_decls = member_decls
        self.endian = endian
        self.size = None

    def __repr__(self):
        return 'TLayout(%r, %r, %r, %r)' % (self.endian, self.member_decls,
                                            self.label, self.const)
    def key(self):
        if self.anonymous:
            raise DMLUnkeyableType(self, 'anonymous layout')
        # TODO consider dropping the explicit 'layout'
        return f"{self.const_str}layout {self.label}"

    def describe(self):
        return 'layout'

    def resolve(self):
        #dbg('resolve %r' % self)
        if self.members != None:
            return self

        # Checks if t is a valid layout member type
        # returning a sometimes patched type representing t
        # and the real, resolved, type of t used for verifying sizeof
        def check_layout_member_type(site, t, name):
            rt = t
            # We cannot use non-shallow instead of this loop because we need
            # to keep track of when we move through arrays
            while isinstance(rt, TNamed):
                rt = safe_realtype_shallow(rt)
            rt.resolve()
            if isinstance(rt, TLayout):
                # In the case of a layout, we need to keep the member type as
                # the original declared type to prevent dis-aliasing a typedef.
                return t, rt
            if rt.is_int:
                if (rt.bits % 8) != 0:
                    raise ELAYOUT(site, "size of %s is not a whole byte" % name)
            if (isinstance(rt, TInt)
                or (dml.globals.compat_dml12_int(site)
                    and isinstance(rt, TSize))):
                toret = TEndianInt(rt.bits, rt.signed,
                                   self.endian, rt.members, rt.const)
                return (toret, toret)
            if isinstance(rt, TEndianInt):
                return (rt, rt)
            if isinstance(rt, TArray):
                # In the case of an array, we return one array that respects
                # the original declaration when necessary, and one array
                # that is the fully resolved type
                new_base, real_base = check_layout_member_type(
                    site, rt.base, name)
                return (TArray(new_base, rt.size, rt.const),
                        TArray(real_base, rt.size, rt.const),)
            raise ELAYOUT(site, "illegal layout member type: %s" % t)

        self.size = 0
        self.members = {}
        for (m, (site, t)) in self.member_decls.items():
            try:
                # t = the member type, rt = real, resolved, underlying type
                t, rt = check_layout_member_type(site, t, m)
                self.members[m] = t

                size = rt.sizeof()
                if size is None:
                    # variable-sized array
                    raise ELAYOUT(site, "unknown layout size")
                else:
                    self.size += size
            except DMLError as e:
                report(e)

        return self

    def sizeof(self):
        return self.size

    def clone(self):
        cloned = TLayout(self.endian, self.member_decls, self.label,
                         self.const)
        if self.members is not None:
            cloned.members = self.members
            cloned.size = self.size
        return cloned



class TFunction(DMLType):
    __slots__ = ('input_types', 'output_type', 'varargs')
    def __init__(self, input_types, output_type,
                 varargs = False, const = False):
        assert isinstance(output_type, DMLType)
        DMLType.__init__(self, const)
        self.input_types = tuple(input_types)
        self.output_type = output_type
        self.varargs = varargs
    def __repr__(self):
        return "TFunction(%r,%r)" % (self.input_types, self.output_type)

    @property
    def const(self): return False

    @const.setter
    def const(self, value):
        assert not value

    def key(self):
        return ('%sfunction(%s%s)->(%s)'
                % (self.const_str,
                   ','.join(t.key() for t in self.input_types),
                   ',...' * self.varargs,
                   self.output_type.key()))

    def describe(self):
        inparams = ",".join([t.describe() if t else "?"
                             for t in self.input_types])
        if self.varargs:
            inparams += ",..."
        return ('function(%s) returning %s'
                % (inparams, self.output_type.describe()))

    def cmp(self, other):
        if (isinstance(other, TFunction)
            and len(self.input_types) == len(other.input_types)
            and all(safe_realtype_unconst(arg1).cmp(
                        safe_realtype_unconst(arg2)) == 0
                    for (arg1, arg2)
                    in zip(self.input_types, other.input_types))
            and safe_realtype_unconst(self.output_type).cmp(
                safe_realtype_unconst(other.output_type)) == 0
            and self.varargs == other.varargs):
            return 0
        return NotImplemented

    def cmp_fuzzy(self, other):
        if (isinstance(other, TFunction)
            and len(self.input_types) == len(other.input_types)
            and all(arg1.cmp_fuzzy(arg2) == 0
                    for (arg1, arg2)
                    in zip(self.input_types, other.input_types))
            and self.output_type.cmp_fuzzy(other.output_type) == 0
            and self.varargs == other.varargs):
            return 0
        return NotImplemented

    def hashed(self):
        return hash((TFunction,
                     tuple(safe_realtype_unconst(typ).hashed()
                           for typ in self.input_types),
                     safe_realtype_unconst(self.output_type).hashed(),
                     self.varargs))

    def canstore(self, other):
        return (False, False, False)

    def clone(self):
        return TFunction(self.input_types, self.output_type, self.varargs,
                         self.const)

    def declaration(self, var):
        arglist = ', '.join(t.declaration("arg%d" % n)
                            for (n, t) in enumerate(self.input_types))
        if self.varargs:
            arglist += ", ..."
        return self.output_type.declaration(f'{var}({arglist})')

class THook(DMLType):
    __slots__ = ('msg_types', 'validated')

    def __init__(self, msg_types, validated=False, const=False):
        DMLType.__init__(self, const)
        self.msg_types = msg_types
        self.validated = validated

    def __repr__(self):
        return 'THook(%s)' % (', '.join(repr(typ) for typ in self.msg_types),)

    def clone(self):
        return THook(self.msg_types, self.validated, self.const)

    def cmp(self, other):
        if (DMLType.cmp(self, other) == 0
            and len(self.msg_types) == len(other.msg_types)
            and all(own_comp.cmp(other_comp) == 0
                    for (own_comp, other_comp) in zip(self.msg_types,
                                                      other.msg_types))):
            return 0
        else:
            return NotImplemented

    def hashed(self):
        return hash((THook,
                     self.const,
                     tuple(comp.hashed() for comp in self.msg_types)))

    def key(self):
        return ('%shook(%s)'
                % (self.const_str, ','.join(t.key() for t in self.msg_types)))

    def describe(self):
        return ('hook with message component types: (%s)'
                % (', '.join(typ.describe() for typ in self.msg_types),))

    def declaration(self, var):
        return f'{self.const_str}_hookref_t {var}'

    def validate(self, fallback_site):
        if not self.validated:
            self.validated = True
            for typ in self.msg_types:
                try:
                    safe_realtype(typ).key()
                except DMLUnkeyableType as e:
                    raise EHOOKTYPE(self.declaration_site or fallback_site,
                                    typ, e.clarification) from e


intre = re.compile('(u?)int([1-5][0-9]?|6[0-4]?|[789])(_be_t|_le_t)?$')
def parse_type(typename):
    "Parse a simple C type name and return a DMLType"
    m = intre.match(typename)
    if m:
        signed = m.group(1) != 'u'
        bits = int(m.group(2))
        byte_order = m.group(3)
        if byte_order:
            return TEndianInt(
                bits, signed,
                "big-endian" if byte_order == "_be_t" else "little-endian")
        else:
            return TInt(bits, signed)
    elif typename in ('double', 'float'):
        return TFloat(typename)
    elif typename == 'bool':
        return TBool()
    elif typename == 'void':
        return TVoid()
    elif typename == 'integer_t' and dml.globals.api_version < compat.api_7:
        return TInt(64, True)
    elif typename == 'uinteger_t' and dml.globals.api_version < compat.api_7:
        return TInt(64, False)
    else:
        return TNamed(typename)

def type_union(type1, type2):
    "Return the greater of two types"
    if (type1.is_float or isinstance(type1, TUnknown)
        or (type1.is_int and type2.is_int
            and type1.bits > type2.bits)):
        return type1
    return type2

def compatible_types(type1, type2):
    # This function intends to verify that two DML types are
    # compatible in the sense defined by the C spec, possibly with
    # some DML-specific restrictions added.
    return type1.cmp(type2) == 0

# TODO We should look into getting rid of this and cmp_fuzzy, and replace
# their usages with usage-specific checks.
def compatible_types_fuzzy(type1, type2):
    # This function intends to verify that two DML types are
    # compatible in the sense defined by the C spec, possibly with
    # some DML-specific restrictions added.
    # DMLType.cmp_fuzzy is only a very rough approximation of this,
    # meant to suite usages such as type-checking the ternary
    # operator.
    # Any use of .cmp_fuzzy or compatible_type_fuzzy should be considered
    # a HACK.
    return type1.cmp_fuzzy(type2) == 0

void = TVoid()
# These are the named types used.  This includes both "imported"
# typedefs for types declared in C header files, and types defined in
# the DML file.
typedefs = {}
for (name, typ) in [
        ('void', void),
        ('int', TInt(32, True)),
        ('char', TInt(8, True)),
        ('long', TLong(True)),
        ('ulong', TLong(False)),
        ('ssize_t', TSize(True)),
        ('size_t', TSize(False)),
        ('int64_t', TInt64_t(True)),
        ('uint64_t', TInt64_t(False))]:
    typedefs[name] = typ

for sym in __all__:
    cls = globals()[sym]
    if isinstance(cls, type) and issubclass(cls, DMLType):
        assert cls.is_arith == (cls.is_int or cls.is_float)
