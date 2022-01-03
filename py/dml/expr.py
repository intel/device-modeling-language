# © 2021-2022 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import abc

import dml.globals
from .logging import *
from .messages import *
from .output import out, quote_filename
from .types import *
from .slotsmeta import *

__all__ = (
    'Code',
    'Expression',
    'NonValue',
    'mkLit', 'Lit',
    'mkApply', 'Apply',
    'StaticIndex',
    'typecheck_inargs',
)

# Base type for code blocks
class Code(object, metaclass=SlotsMeta):
    def __init__(self, site):
        self.site = site

    def __repr__(self):
        return '%s(%s)' % (type(self).__name__,
                           ','.join(repr(getattr(self, name))
                                    for name in self.init_args[2:]))

    def linemark(self):
        if dml.globals.linemarks and self.site:
            # out("/* %s */\n" % repr(self))
            out('#line %d "%s"\n' % (self.site.lineno,
                                     quote_filename(self.site.filename())))

class Expression(Code):
    '''An Expression can represent either:
    (1) a C expression (returned by .read()) with a DML type attached to it, or
    (2) a DML expression that is not a stand-alone value

    An expression in DML will yield either (1) or (2). (1) is also
    used internally by the compiler for code generation, and there are
    Expression objects that cannot be represented as DML expressions.

    An expression is always internally consistent; i.e., any compile
    error in a DML expression must be reported before the Expression
    is instantiated.

    Expressions can be composed into a composite expression, given
    that their DML types (returned by .ctype()) obey the type rules of
    the composite expression.'''
    slots = ('context',)

    # An instance of DMLType
    #type = None

    # If true, this is a constant, with the value stored in .value, as
    # a Python value: either a string, number, bool, a Python
    # list of Expression objects, or the value types.undefval.
    constant = False

    # true if this is an undefined expression
    undefined = False

    # Operator priority of the generated C expression, sometimes used
    # by parent expression to decide whether to enclose expression in
    # a parenthesis.
    #
    # Priorities are based on by Harbison & Steele, "C - A Reference
    # Manual", 5th ed., Section 7.2, although using multiples of 10.
    #
    # Some priorities have been adjusted to force parentheses in the
    # output, in order to keep gcc from emitting warnings for the
    # generated code. (GCC does not trust that you know what you are
    # doing, so you have to insert more parentheses than strictly
    # necessary to make it shut up.)
    #
    # operator                  priority    adjusted
    # ----------------------------------------------
    # a[k] f(...) . -> i++ i--  160           =
    # i++ i--                   160           =
    # ++i --i                   150           =
    # sizeof                    150         n/a
    # ~ ! -i +i & *p            150           =
    # (type)                    140         n/a
    # * / %                     130           =
    # + -                       120         110
    # << >>                     110           =
    # < <= > >=                 100          70
    # == !=                     90           70
    # &                         80           70
    # ^                         70           70
    # |                         60           70
    # &&                        50           40
    # ||                        40            =
    # ?:                        30            =
    # = += -= *= /= %=          20            =
    # <<= >>= &= ^= |=          20            =
    # ,                         10           10
    priority = 0

    # This is true for integer expressions whose bitsize are explicitly
    # defined, such as local variables. Required for big-endian
    # bitslicing.
    explicit_type = False

    # Can the expression be assigned to?
    # If writable is True, there is a method write() which returns a C
    # expression to make the assignment.
    writable = False

    def __init__(self, site):
        assert not site or isinstance(site, Site)
        self.site = site
        self.context = ErrorContext.current()

    def __str__(self):
        "Format the expression in DML form"
        return '<unknown expression '+repr(self)+'>'

    # Produce a C expression that has the value of this expression.
    def read(self):
        raise ICE(self.site, "can't read %r" % self)

    # Produce a C expression but don't worry about the value.
    def discard(self):
        return self.read()

    def ctype(self):
        '''The corresponding DML type of this expression'''
        return self.type

    def apply(self, args):
        'Apply this expression as a function'
        return mkApply(self.site, self, args)

    def incref(self):
        pass

    def decref(self):
        pass

    def copy(self, site):
        "Create an identical expression, but with a different site."
        return type(self)(
            site, *(getattr(self, name) for name in self.init_args[2:]))

class NonValue(Expression):
    '''An expression that is not really a value, but which may validly
    appear as a subexpression of certain expressions.
    This will be caught by codegen_expression, which raises ENVAL.'''
    @property
    def type(self):
        raise ICE(self.site, 'not a value: %r' % (self,))
    def ctype(self):
        raise ICE(self.site, 'not a value: %r' % (self,))
    def read(self):
        raise ICE(self.site, 'cannot read non-value %r' % (self,))
    def apply(self, args):
        raise self.exc()
    def exc(self):
        '''Exception to raise when expression appears in an incorrect context'''
        return ENVAL(self.site, self)

class Lit(Expression):
    "A literal C expression"
    slots = ('safe', 'cexpr', 'type', 'str')
    def __init__(self, site, cexpr, type, safe = False, str = None):
        Expression.__init__(self, site)
        self.cexpr = cexpr
        self.type = type
        self.safe = safe
        self.str = cexpr if str is None else str
    @property
    def priority(self):
        return 1000 if self.safe else 0
    def __str__(self):
        return self.str or self.cexpr
    def read(self):
        return self.cexpr
    def write(self, source):
        assert self.writable
        return "%s = %s" % (self.cexpr, source.read())
    @property
    def writable(self):
        return self.type is not None

mkLit = Lit

def typecheck_inargs(site, args, inp, kind="function", known_arglen=None):
    arglen = len(args) if known_arglen is None else known_arglen
    if arglen != len(inp):
        raise EARG(site, kind)

    for (i, (arg, (pname, ptype))) in enumerate(zip(args, inp)):
        argtype = safe_realtype(arg.ctype())
        if not argtype:
            raise ICE(site, "unknown expression type")

        rtype = safe_realtype(ptype)
        assert rtype
        (ok, trunc, constviol) = rtype.canstore(argtype)

        if ok:
            if constviol:
                raise ECONSTP(site, pname, kind + " call")
        else:
            raise EPTYPE(site, arg, rtype, pname, kind)

class Apply(Expression):
    priority = 160
    explicit_type = True
    @auto_init
    def __init__(self, site, fun, args, funtype):
        pass
    def ctype(self):
        return self.funtype.output_type
    def __str__(self):
        return (str(self.fun) +
                '(' + ", ".join(str(e) for e in self.args) + ')')
    def read(self):
        return (self.fun.read() +
                '(' + ", ".join(e.read() for e in self.args) + ')')

def mkApply(site, fun, args):
    '''Apply a C function'''
    funtype = fun.ctype()

    if not funtype:
        raise EAPPLY(fun)

    try:
        funtype = realtype(funtype)
        if isinstance(funtype, TPtr) and isinstance(funtype.base, TFunction):
            # Pointers to functions are the same as the functions
            funtype = realtype(funtype.base)
    except DMLUnknownType:
        raise ETYPE(site, funtype)

    if not isinstance(funtype, TFunction):
        raise EAPPLY(fun)

    if funtype.varargs and len(args) > len(funtype.input_types):
        known_arglen = len(funtype.input_types)
    else:
        known_arglen = len(args)

    typecheck_inargs(
        site, args,
        [(str(i + 1), t) for (i, t) in enumerate(funtype.input_types)],
        'function', known_arglen)

    # Coerce vararg endian integers to integers
    from .expr_util import coerce_if_eint
    coerced_varargs = [coerce_if_eint(arg) for arg in args[known_arglen:]]

    # varargs are not source-for-assignment-ed
    if (not dml.globals.compat_dml12_int(site)
        or not dml.globals.compat_dml12_int(fun.site)):
        from .ctree import source_for_assignment
        args = [source_for_assignment(site, in_type, arg)
                for (in_type, arg) in zip(funtype.input_types, args)]
    else:
        args = [coerce_if_eint(arg) for arg in args[:known_arglen]]
    args.extend(coerced_varargs)
    return Apply(site, fun, args, funtype)

class StaticIndex(NonValue):
    """A reference to the index variable of a containing object array,
    from a static context. For instance, $i in the following:

    register r[4] { if ($i == 3) { field f; } }
    """
    @auto_init
    def __init__(self, site, var):
        pass
    def str(self):
        return dollar(self.site) + self.var
    def exc(self):
        return EIDXVAR(self.site, dollar(self.site) + self.var)
