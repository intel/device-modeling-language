# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import abc

import dml.globals
from .logging import *
from .messages import *
from .output import *
from .types import *
from .slotsmeta import *

from . import output

__all__ = (
    'Code',
    'Expression',
    'NonValue',
    'NonValueArrayRef',
    'mkLit', 'Lit',
    'mkApply', 'mkApplyInits', 'Apply',
    'mkNullConstant', 'NullConstant',
    'StaticIndex',
    'typecheck_inargs',
    'typecheck_inarg_inits',
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
        site_linemark(self.site)

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

    def apply(self, inits, location, scope):
        'Apply this expression as a function'
        return mkApplyInits(self.site, self, inits, location, scope)

    @property
    def is_pointer_to_stack_allocation(self):
        '''Returns True only if it's known that the expression is a pointer
           to stack-allocated data'''
        return False

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
    def apply(self, inits, location, scope):
        raise self.exc()
    def exc(self):
        '''Exception to raise when expression appears in an incorrect context'''
        return ENVAL(self.site, self)

class NonValueArrayRef(NonValue):
    '''Reference to an array node before it's indexed. Indexing is the
    only supported operation.'''

    @abc.abstractproperty
    def local_indices(self): pass

    @abc.abstractproperty
    def local_dimsizes(self): pass

    def exc(self):
        return EARRAY(self.site, self)

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

class NullConstant(Expression):
    """The NULL expression in DML 1.4.
    This class must be used for any pointer-typed expression of value NULL
    considered constant by DMLC."""
    constant = True
    value = None
    priority = 1000
    type = TPtr(void, const=True)
    def __str__(self):
        return 'NULL'
    def read(self):
        return 'NULL'
    def copy(self, site):
        return NullConstant(site)

mkNullConstant = NullConstant

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

# Typecheck a DML method application, where the arguments are given as a list
# where each element is either an AST of an initializer, or an initializer
# directly.
# Returns a list of expressions corresponding to the provided initializers
def typecheck_inarg_inits(site, inits, inp, location, scope,
                          kind="function", variadic=False,
                          allow_undefined_args=False,
                          on_ptr_to_stack=None):
    if (not variadic and len(inits) != len(inp)) or len(inits) < len(inp):
        raise EARG(site, kind)

    from .expr_util import coerce_if_eint
    from .codegen import eval_initializer, codegen_expression, \
                         codegen_expression_maybe_nonvalue
    from .ctree import Initializer, ExpressionInitializer

    args = []
    for (init, (pname, ptype)) in zip(inits, inp):
        if isinstance(init, Initializer):
            if ptype is None:
                assert isinstance(init, ExpressionInitializer)
                arg = init.expr
            else:
                try:
                    arg = init.as_expr(ptype)
                except EASTYPE as e:
                    if e.site is init.site:
                        raise EPTYPE(site, e.source, e.target_type, pname,
                                     kind) from e
                    raise
                # better error message
                except EDISCONST as e:
                    if e.site is init.site:
                        raise ECONSTP(site, pname, kind + " call") from e
                    raise
        elif ptype is None:
            if init.kind != 'initializer_scalar':
                raise ESYNTAX(init.site, '{',
                              'the argument for an untyped parameter must be '
                              + 'a simple expression')
            arg = codegen_expression_maybe_nonvalue(init.args[0], location,
                                                    scope)
            if (isinstance(arg, NonValue)
                and not (allow_undefined_args and arg.undefined)):
                raise arg.exc()
        elif (dml.globals.compat_dml12_int(site)
            and dml.globals.compat_dml12_int(init.site)
            and init.kind == 'initializer_scalar'):
            arg = coerce_if_eint(codegen_expression(init.args[0],
                                                    location, scope))
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
        else:
            try:
                arg = eval_initializer(init.site, ptype, init, location,
                                       scope, False).as_expr(ptype)
            except EASTYPE as e:
                if e.site is init.site:
                    raise EPTYPE(site, e.source, e.target_type, pname,
                                 kind) from e
                raise
            # better error message
            except EDISCONST as e:
                if e.site is init.site:
                    raise ECONSTP(site, pname, kind + " call") from e
                raise
        if (on_ptr_to_stack
            and isinstance(safe_realtype_shallow(ptype), TPtr)
            and arg.is_pointer_to_stack_allocation):
            on_ptr_to_stack(arg)
        args.append(arg)

    if variadic and len(inits) > len(inp):
        for init in inits[len(inp):]:
            if init.kind != 'initializer_scalar':
                raise ESYNTAX(init.site, '{',
                              'variadic arguments must be simple expressions')
            args.append(coerce_if_eint(codegen_expression(init.args[0],
                                                          location, scope)))

    return args

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

def mkApplyInits(site, fun, inits, location, scope):
    '''Apply a C function to initializers'''
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

    args = typecheck_inarg_inits(
        site, inits,
        [(str(i + 1), t) for (i, t) in enumerate(funtype.input_types)],
        location, scope, 'function', funtype.varargs)

    return Apply(site, fun, args, funtype)

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
    def __str__(self):
        return dollar(self.site) + self.var
    def exc(self):
        return EIDXVAR(self.site, dollar(self.site) + self.var)
