# © 2021-2022 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

# Intermediate representation of the device model

import os.path
import re
import abc
import collections
import operator
import itertools
import math
from functools import reduce

from dml import objects, symtab, logging, crep, serialize
from .logging import *
from .messages import *
from .output import out, quote_filename
from .types import *
from .expr import *
from .expr_util import *
from .slotsmeta import auto_init
from . import dmlparse
import dml.globals
# set from codegen.py
codegen_call_expr = None

__all__ = (
    'endian_convert_expr',
    'source_for_assignment',

    'Location',

    'ExpressionSymbol',
    'LiteralSymbol',

    'mkCompound',
    'mkNull', 'Null',
    'mkLabel',
    'mkUnrolledLoop',
    'mkGoto',
    'mkReturnFromInline',
    'mkThrow',
    'mkUnrolledBreak',
    'mkTryCatch',
    'mkInline',
    'mkInlinedMethod',
    'mkComment',
    'mkAssert',
    'mkReturn',
    'mkDelete',
    'mkExpressionStatement',
    'mkAfter',
    'mkIf',
    'mkWhile',
    'mkDoWhile',
    'mkFor',
    'mkSwitch',
    'mkCase',
    'mkDefault',
    'mkVectorForeach',
    'mkBreak',
    'mkContinue',
    'mkAssignStatement',
    'mkCopyData',
    'mkIfExpr',
    #'BinOp',
    #'Test',
    #'Flag',
    #'Logical',
    'mkAnd',
    'mkOr',
    #'Compare',
    'mkLessThan',
    'mkLessThanOrEquals',
    'mkGreaterThan',
    'mkGreaterThanOrEquals',
    'mkEquals',
    'mkNotEquals',
    #'BitBinOp',
    'mkBitAnd',
    'mkBitOr',
    'mkBitXOr',
    'BitShift',
    'mkShL',
    'mkShR',
    #'ArithBinOp',
    'mkMult',
    'mkDiv',
    'mkMod',
    'mkAdd',
    'mkSubtract',
    'mkAssignOp',
    'mkAddressOf',
    'mkDereference',
    'mkNot',
    'mkBitNot',
    'mkUnaryMinus',
    'mkUnaryPlus',
    'mkPreInc',
    'mkPreDec',
    'mkPostInc',
    'mkPostDec',
    'mkMethodPresent',
    'mkBitSlice',
    'InterfaceMethodRef',
    'mkNew',
    #'Constant',
    'mkIntegerConstant', 'IntegerConstant',
    'mkIntegerLiteral',
    'mkFloatConstant', 'FloatConstant',
    'mkStringConstant', 'StringConstant',
    'AbstractList',
    'mkList', 'List',
    'mkSequenceLength',
    'mkObjectList',
    'mkEachIn', 'EachIn',
    'mkBoolConstant',
    'mkUndefined', 'Undefined',
    'TraitParameter',
    'TraitSessionRef',
    'TraitMethodRef',
    'TraitMethodIndirect',
    'TraitMethodDirect',
    'mkTraitUpcast',
    'TraitUpcast',
    'try_convert_identity',
    'ObjIdentity',
    'TraitObjIdentity',
    'ObjTraitRef',
    'NodeArrayRef',
    'SessionVariableRef',
    'mkNodeRef', 'NodeRef',
    'Variable',
    'mkLocalVariable',
    'mkStaticVariable',
    'mkSubRef',
    'mkIndex',
    'mkCast',
    'Cast',
    'mkInlinedParam',
    'mkQName', 'QName',
    'mkDeviceObject',
    'mkStructDefinition',
    'mkDeclaration',
    'mkCText',
    'Initializer', 'ExpressionInitializer', 'CompoundInitializer',
    'DesignatedStructInitializer', 'MemsetInitializer',

    'as_bool',
    'as_int',
    'sym_declaration',
    'lookup_var',
    'log_statement',

    'all_index_exprs',

    'get_anonymized_name',
    'mkHiddenName', 'HiddenName',
    'mkHiddenQName', 'HiddenQName',
   )

def assert_type(site, expr, type):
    if not isinstance(expr, type):
        raise ICE(site, repr(expr)+" is not a "+repr(type))


def comparable_types(expr1, expr2, equality):
    "Check if two expressions can be compared"
    typ1 = realtype(expr1.ctype())
    typ2 = realtype(expr2.ctype())

    if typ1.is_arith and typ2.is_arith:
        return True
    if isinstance(typ1, (TPtr, TArray)) and isinstance(typ2, (TPtr, TArray)):
        return True
    if equality and isinstance(typ1, TBool) and isinstance(typ2, TBool):
        return True

    return False

def assert_comparable_types(site, expr1, expr2, equality):
    "Assert that two expressions can be compared"
    if not comparable_types(expr1, expr2, equality):
        typ1 = realtype(expr1.ctype())
        typ2 = realtype(expr2.ctype())
        raise EILLCOMP(site, expr1, typ1, expr2, typ2)

class ExpressionSymbol(symtab.Symbol):
    """A symbol that corresponds to an expression."""
    def __init__(self, name, expr, site):
        super(ExpressionSymbol, self).__init__(name, value = expr, site = site)
    def expr(self, site):
        expr = self.value.copy(site)
        expr.incref()
        return expr

class LiteralSymbol(symtab.Symbol):
    "A symbol with corresponds directly to an opaque C identifier"
    def __init__(self, name, type, site, crep=None):
        super(LiteralSymbol, self).__init__(name, value = crep or name,
                                            site = site)
        self.type = type
    def expr(self, site):
        return mkLit(site, self.value, self.type)

class Location(object):
    __slots__ = ('node', 'indices')
    def __init__(self, node, indices):
        self.node = node
        # Many functions take an 'indices' arg derived from
        # Location.indices; we generally allow such functions to assume
        # that indices are either TInt or StaticIndex.
        assert all(isinstance(e, StaticIndex)
                   or isinstance(realtype(e.ctype()), TInt)
                   for e in indices)
        self.indices = indices
    def __repr__(self):
        return 'Location(%r, %r)' % (self.node, self.indices)
    def method(self):
        if isinstance(self.node, objects.Method):
            return self.node
        else:
            return None

#
# Statements
#

class ControlFlow(object):
    '''Represents the set of possible ways execution can proceed, within
    the same method, after execution of a statement:
    - fallthrough: may proceed to the following statement.
    - throw: may throw
    - br: may break an enclosing switch or loop

    Note that returning from the method, crashing, or entering an
    infinite loop count as equivalent here.

    Note also that 'continue' statements are not represented for in a
    control flow. This is because their jump destination is already is
    known to be reachable; since a 'continue' cannot fall through and don't
    add any new path for execution to proceed, it can be
    considered equivalent to a false assertion for the purposes of
    our control flow analysis.'''
    __slots__ = ('fallthrough', 'throw', 'br')
    def __init__(self, **args):
        for prop in self.__slots__:
            setattr(self, prop, False)
        for prop in args:
            # will trigger error if a kw arg is misspelt
            setattr(self, prop, args[prop])
    def __repr__(self):
        return 'ControlFlow(%s)' % (
            ', '.join('%s=%r' % (prop, getattr(self, prop))
                      for prop in self.__slots__))
    def union(self, other, **args):
        '''A control flow with the union of exit paths from two control flows.
        Keyword args provide explicit overrides.'''
        for prop in self.__slots__:
            args.setdefault(prop, getattr(self, prop) or getattr(other, prop))
        return ControlFlow(**args)
    def replace(self, **args):
        '''Return a copy of this flow, with selected properties overridden'''
        for prop in self.__slots__:
            args.setdefault(prop, getattr(self, prop))
        return ControlFlow(**args)

class Statement(Code):
    slots = ('context',)
    # True if the statement declares a symbol in the containing scope
    is_declaration = False
    is_empty = False
    def __init__(self, site):
        self.site = site
        self.context = ErrorContext.current()
    @abc.abstractmethod
    def toc(self): pass
    def toc_inline(self): return self.toc()

    def control_flow(self):
        '''Rudimentary control flow analysis: Return a ControlFlow object
        indicating how this statement affects the control flow.

        The analysis is used to prove that control cannot reach the
        end of the method, i.e., that either a return statement is
        reached, an uncaught exception is reached, or an infinite loop
        is entered.

        The analysis is conjectured to be sound, i.e., if our analysis
        deduces that control cannot reach the end of a method, then
        this can be trusted.

        The analysis is also rather simple and limited: one can
        construct code where a proper analysis of the full control
        flow graph would prove that control cannot reach the end of
        the method, but where our analysis fails to prove this. For an
        example, see test/1.4/errors/T_ENORET_throw_after_break.dml.'''
        return ControlFlow(fallthrough=True)

class Compound(Statement):
    @auto_init
    def __init__(self, site, substatements):
        assert isinstance(substatements, list)

    def toc(self):
        out('{\n', postindent = 1)
        self.toc_inline()
        out('}\n', preindent = -1)

    def toc_inline(self):
        for substatement in self.substatements:
            substatement.toc()

    def control_flow(self):
        acc = ControlFlow(fallthrough=True)
        for sub in self.substatements:
            flow = sub.control_flow()
            acc = acc.union(flow, fallthrough=flow.fallthrough)
            if not acc.fallthrough:
                return acc
        return acc

def mkCompound(site, statements):
    "Create a simplified Compound() from a list of ctree.Statement"
    collapsed = []
    for stmt in statements:
        if (isinstance(stmt, Compound)
            and not any(subsub.is_declaration
                        for subsub in stmt.substatements)):
            collapsed.extend(stmt.substatements)
        elif not stmt.is_empty:
            collapsed.append(stmt)
    if len(collapsed) == 1 and not collapsed[0].is_declaration:
        return collapsed[0]
    elif collapsed:
        return Compound(site, collapsed)
    else:
        return mkNull(site)

class Null(Statement):
    is_empty = True
    def toc_inline(self):
        pass
    def toc(self):
        if self.site:
            self.linemark()
        out(';\n')

mkNull = Null

class Label(Statement):
    def __init__(self, site, label):
        Statement.__init__(self, site)
        self.label = label
    def toc(self):
        out('%s: ;\n' % self.label, preindent = -1, postindent = 1)

mkLabel = Label

class UnrolledLoop(Statement):
    @auto_init
    def __init__(self, site, substatements, break_label):
        assert isinstance(substatements, list)

    def toc(self):
        out('{\n', postindent = 1)
        self.toc_inline()
        out('}\n', preindent = -1)

    def toc_inline(self):
        for substatement in self.substatements:
            substatement.toc()
        if self.break_label is not None:
            out(f'{self.break_label}: UNUSED;\n')

    def control_flow(self):
        bodyflow = mkCompound(self.site, self.substatements).control_flow()
        return bodyflow.replace(fallthrough=(bodyflow.fallthrough
                                             or bodyflow.br), br=False)

mkUnrolledLoop = UnrolledLoop

class Goto(Statement):
    @auto_init
    def __init__(self, site, label): pass
    def toc(self):
        out('goto %s;\n' % (self.label,))

    def control_flow(self):
        raise ICE(self.site, 'goto is 1.2 only, control_flow() is 1.4+ only')

mkGoto = Goto

class ReturnFromInline(Goto):
    def control_flow(self):
        return ControlFlow()

mkReturnFromInline = ReturnFromInline

class Throw(Goto):
    def control_flow(self):
        return ControlFlow(throw=True)

mkThrow = Throw

class UnrolledBreak(Goto):
    def control_flow(self):
        return ControlFlow(br=True)

mkUnrolledBreak = UnrolledBreak

class TryCatch(Statement):
    '''A DML try/catch statement. Catch block is represented as an if (false)
    block with a catch label, to which Throw statements inside will go.'''
    @auto_init
    def __init__(self, site, label, tryblock, catchblock): pass
    def toc_inline(self):
        self.tryblock.toc()
        out('if (false) {\n')
        out('%s: ;\n' % (self.label,), postindent=1)
        self.catchblock.toc_inline()
        out('}\n', preindent=-1)
    def toc(self):
        out('{\n', postindent = 1)
        self.toc_inline()
        out('}\n', preindent = -1)
    def control_flow(self):
        tryflow = self.tryblock.control_flow()
        if not tryflow.throw:
            # catch block is dead
            return tryflow
        catchflow = self.catchblock.control_flow()
        return tryflow.union(catchflow, throw=catchflow.throw)

def mkTryCatch(site, label, tryblock, catchblock):
    if not label:
        return tryblock
    return TryCatch(site, label, tryblock, catchblock)

class Inline(Statement):
    @auto_init
    def __init__(self, site, str): pass
    def toc(self):
        out(self.str + '\n')

mkInline = Inline

class InlinedMethod(Statement):
    '''Wraps the body of an inlined method, to protect it from analysis'''
    @auto_init
    def __init__(self, site, method, body): pass
    def toc(self):
        self.body.toc()
    def toc_inline(self):
        self.body.toc_inline()
    def control_flow(self):
        return ControlFlow(fallthrough=True, throw=self.method.throws)

mkInlinedMethod = InlinedMethod

class Comment(Statement):
    @auto_init
    def __init__(self, site, str): pass
    def toc(self):
        # self.linemark()
        out('/* %s */\n' % self.str)

mkComment = Comment

class Assert(Statement):
    @auto_init
    def __init__(self, site, expr): pass
    def toc(self):
        out('DML_ASSERT("%s", %d, %s);\n'
            % (quote_filename(self.site.filename()),
               self.site.lineno, self.expr.read()))
    def control_flow(self):
        return ControlFlow(
            fallthrough=bool(not self.expr.constant or self.expr.value))

def mkAssert(site, expr):
    return Assert(site, expr)

class Return(Statement):
    @auto_init
    def __init__(self, site, expr): pass
    def toc(self):
        self.linemark()
        if self.expr is None:
            out('return;\n')
        else:
            out('return %s;\n' % self.expr.read())
    def control_flow(self):
        return ControlFlow()

def mkReturn(site, expr, rettype=None):
    if rettype and not dml.globals.compat_dml12_int(site):
        expr = source_for_assignment(site, rettype, expr)
    return Return(site, expr)

class Delete(Statement):
    @auto_init
    def __init__(self, site, expr): pass
    def toc(self):
        out('MM_FREE(%s);\n' % self.expr.read())

def mkDelete(site, expr):
    return Delete(site, expr)

class ExpressionStatement(Statement):
    @auto_init
    def __init__(self, site, expr): pass
    def toc(self):
        #if not self.site:
        #    print 'NOSITE', str(self), repr(self)
        self.linemark()
        # out('/* %s */\n' % repr(self))
        s = self.expr.discard()

        out(s+';\n')

def mkExpressionStatement(site, expr):
    if isinstance(expr, Constant):
        return mkNull(site)
    return ExpressionStatement(site, expr)

class After(Statement):
    @auto_init
    def __init__(self, site, unit, delay, method, fun, indices, inargs): pass
    def toc(self):
        self.linemark()
        objarg = '&_dev->obj'
        out(f'if (SIM_object_clock({objarg}) == NULL)\n', postindent=1)
        out(f'''SIM_log_error({objarg}, 0, "Attribute 'queue' is '''
            + '''not set, ignoring delayed call to method '''
            + f''''{self.method.logname_anonymized([])}'");\n''')
        out('else {\n', preindent=-1, postindent=1)
        if self.indices or self.inargs:
            if self.inargs:
                # _in_p_attrs deliberately named to avoid possible name
                # conflicts with _in_param_*
                out(f'attr_value_t _in_p_attrs[{len(self.inargs)}];\n')
                for (i, (arg, (pname, ptype))) in enumerate(
                        zip(self.inargs, self.method.inp)):
                    ptype = safe_realtype(ptype)
                    out(f'{ptype} _in_param_{pname} = {arg.read()};\n')
                    out(f'{serialize.lookup_serialize(ptype)}'
                        + f'(&_in_param_{pname}, &_in_p_attrs[{i}]);\n')
            out('attr_value_t *_data = MM_MALLOC(1, attr_value_t);\n')

            index_attr_list = [f'SIM_make_attr_uint64({i.read()})'
                               for i in self.indices]
            param_attr_list = [f'_in_p_attrs[{i}]'
                               for i in range(len(self.inargs))]

            # If both indices and params are present, serialized representation
            # is '[index_attr_list, param_attr_list]'
            #
            # If only indices or params are present, then the representation is
            # flattened to a single list.
            if self.indices and self.inargs:
                out('*_data = SIM_make_attr_list(2, '
                    + f'SIM_make_attr_list({len(self.indices)}, '
                    + f'{", ".join(index_attr_list)}), '
                    + f'SIM_make_attr_list({len(self.inargs)}, '
                    + f'{", ".join(param_attr_list)}));\n')
            else:
                out('*_data = SIM_make_attr_list'
                    + f'({len(self.indices) + len(self.inargs)}, '
                    + f'{", ".join(index_attr_list + param_attr_list)});\n')
            data = '_data'
        else:
            data = 'NULL'
        out(f'SIM_event_post_{self.unit}(SIM_object_clock({objarg}), '
            + f'{crep.get_evclass(self.method)}, {objarg}, '
            + f'{self.delay.read()}, {data});\n')
        out("}\n", preindent = -1)

def mkAfter(site, unit, delayexpr, method, fun, indices, inargs):
    return After(site, unit, delayexpr, method, fun, indices, inargs)

class If(Statement):
    @auto_init
    def __init__(self, site, cond, truebranch, falsebranch):
        assert_type(site, cond.ctype(), TBool)
        assert_type(site, truebranch, Statement)
        assert_type(site, falsebranch, (Statement, type(None)))
    def toc(self):
        self.linemark()
        # out('/* %s */\n' % repr(self))
        out('if ('+self.cond.read()+') {\n', postindent = 1)
        self.truebranch.toc_inline()
        if isinstance(self.falsebranch, If):
            out('} else ', preindent = -1)
            if dml.globals.linemarks:
                out('\n')
            self.falsebranch.toc()
        elif self.falsebranch:
            out('} else {\n', preindent = -1, postindent = 1)
            self.falsebranch.toc_inline()
            out('}\n', preindent = -1)
        else:
            out('}\n', preindent = -1)

    def control_flow(self):
        a = self.truebranch.control_flow()
        b = (self.falsebranch.control_flow() if self.falsebranch
             else ControlFlow(fallthrough=True))
        return a.union(b)

def mkIf(site, cond, truebranch, falsebranch = None):
    assert isinstance(cond.ctype(), TBool)
    if cond.constant:
        if cond.value:
            return truebranch
        elif falsebranch:
            return falsebranch
        else:
            return mkNull(site)
    return If(site, cond, truebranch, falsebranch)

class While(Statement):
    @auto_init
    def __init__(self, site, cond, stmt):
        assert_type(site, cond.ctype(), TBool)
        assert_type(site, stmt, Statement)
    def toc(self):
        self.linemark()
        # out('/* %s */\n' % repr(self))
        out('while ('+self.cond.read()+') {\n', postindent = 1)
        self.stmt.toc_inline()
        out('}\n', preindent = -1)

    def control_flow(self):
        bodyflow = self.stmt.control_flow()
        if self.cond.constant:
            if self.cond.value:
                # infinite loop
                return bodyflow.replace(fallthrough=bodyflow.br, br=False)
            else:
                # dead body
                return ControlFlow(fallthrough=True)
        else:
            # fallthrough is possible if condition is initially false
            return bodyflow.replace(fallthrough=True, br=False)

def mkWhile(site, expr, stmt):
    return While(site, expr, stmt)

class DoWhile(Statement):
    @auto_init
    def __init__(self, site, cond, stmt):
        assert_type(site, cond.ctype(), TBool)
        assert_type(site, stmt, Statement)
    def toc(self):
        self.linemark()
        # out('/* %s */\n' % repr(self))
        out('do {\n', postindent = 1)
        self.stmt.toc_inline()
        out('} while ('+self.cond.read()+');\n', preindent = -1)
    def control_flow(self):
        bodyflow = self.stmt.control_flow()
        if self.cond.constant and self.cond.value:
            # infinite loop
            return bodyflow.replace(fallthrough=bodyflow.br, br=False)
        else:
            return bodyflow.replace(
                fallthrough=bodyflow.fallthrough or bodyflow.br, br=False)

def mkDoWhile(site, expr, stmt):
    return DoWhile(site, expr, stmt)

class For(Statement):
    @auto_init
    def __init__(self, site, pres, cond, posts, stmt):
        assert_type(site, cond.ctype(), TBool)
        assert_type(site, stmt, Statement)
    def toc(self):
        self.linemark()

        out('for (%s; %s; ' % (", ".join(pre.discard()
                                         for pre in self.pres),
                                      self.cond.read()))
        if all(isinstance(post, ExpressionStatement) for post in self.posts):
            # common case: all post statements are expressions, so
            # traditional for loop can be produced
            out(', '.join(post.expr.discard() for post in self.posts))
        else:
            # general case: arbitrary statements in post code;
            # encapsulate in a statement expression
            out('({\n', postindent = 1)
            for post in self.posts:
                post.toc()
            out(' })', preindent = -1)

        out(') {\n', postindent = 1)
        self.stmt.toc_inline()
        out('}\n', preindent = -1)

    def control_flow(self):
        bodyflow = self.stmt.control_flow()
        if self.cond.constant:
            if self.cond.value:
                # infinite loop
                return bodyflow.replace(fallthrough=bodyflow.br, br=False)
            else:
                # dead body
                return ControlFlow(fallthrough=True)
        else:
            # fallthrough is possible if condition is initially false
            return bodyflow.replace(fallthrough=True, br=False)

def mkFor(site, pres, expr, posts, stmt):
    return For(site, pres, expr, posts, stmt)

class Switch(Statement):
    @auto_init
    def __init__(self, site, expr, stmt):
        assert_type(site, expr, Expression)
        assert_type(site, stmt, Statement)
    def toc(self):
        self.linemark()
        # out('/* %s */\n' % repr(self))
        out('switch ('+self.expr.read()+') {\n', postindent = 1)
        self.stmt.toc_inline()
        out('}\n', preindent = -1)

    def control_flow(self):
        assert self.site.dml_version() != (1, 2)
        # guaranteed by grammar in DML 1.4.
        assert isinstance(self.stmt, Compound)
        found_default = False
        # The possible exit paths from the sequence of statements
        # processed so far
        flow = ControlFlow(fallthrough=True)
        for stmt in self.stmt.substatements:
            if isinstance(stmt, Default):
                found_default = True
            if isinstance(stmt, (Default, Case)):
                flow = flow.replace(fallthrough=True)
            elif flow.fallthrough:
                f = stmt.control_flow()
                flow = flow.union(f, fallthrough=f.fallthrough)
        return flow.replace(
            fallthrough=flow.fallthrough or flow.br or not found_default,
            br=False)

def mkSwitch(site, expr, stmt):
    return Switch(site, as_int(expr), stmt)

class Case(Statement):
    @auto_init
    def __init__(self, site, expr): pass
    def toc(self):
        self.linemark()
        out('case %s: ;\n' % self.expr.read(), preindent = -1, postindent = 1)

mkCase = Case

class Default(Statement):
    @auto_init
    def __init__(self, site): pass
    def toc(self):
        self.linemark()
        out('default: ;\n', preindent = -1, postindent = 1)

mkDefault = Default

class VectorForeach(Statement):
    @auto_init
    def __init__(self, site, vect, var, stmt): pass

    def toc(self):
        out('VFOREACH(%s, %s) {\n' % (self.vect.read(), self.var.read()),
            postindent = 1)
        self.stmt.toc_inline()
        out('}\n', preindent = -1)

    def control_flow(self):
        flow = self.stmt.control_flow()
        return flow.replace(fallthrough=flow.fallthrough or flow.br, br=False)

def mkVectorForeach(site, vect, var, stmt):
    return VectorForeach(site, vect, var, stmt)

class Break(Statement):
    def toc(self):
        out('break;\n')
    def control_flow(self):
        return ControlFlow(br=True)

mkBreak = Break

class Continue(Statement):
    def toc(self):
        out('continue;\n')
    def control_flow(self):
        return ControlFlow()

mkContinue = Continue

class AssignStatement(Statement):
    @auto_init
    def __init__(self, site, target, initializer):
        assert isinstance(initializer, Initializer)
    def toc(self):
        out('{\n', postindent=1)
        self.toc_inline()
        out('}\n', preindent=-1)
    def toc_inline(self):
        self.initializer.assign_to(self.target, self.target.ctype())

mkAssignStatement = AssignStatement

def mkCopyData(site, source, target):
    "Convert a copy statement to intermediate representation"
    assignexpr = mkAssignOp(site, target, source)
    return mkExpressionStatement(site, assignexpr)

#
# Expressions
#

def as_bool(e):
    "Change this expression to a boolean expression, if possible"
    t = e.ctype()
    if isinstance(t, TBool):
        return e
    elif t.is_int and t.bits == 1:
        if logging.show_porting and (isinstance(e, NodeRef)
                                     or isinstance(e, LocalVariable)):
            report(PBITNEQ(dmlparse.start_site(e.site),
                           dmlparse.end_site(e.site)))
        return mkFlag(e.site, e)
    elif isinstance(t, TPtr):
        return mkNotEquals(e.site, e,
                           Lit(None, 'NULL', TPtr(TVoid()), 1))
    else:
        report(ENBOOL(e))
        return mkBoolConstant(e.site, False)

def as_int(e):
    """Change this expression to a TInt type, if possible

    In dml 1.2-compat, TInt typed expressions are returned as-is
    Otherwise: Returns an unsigned 64-bit integer if the integer type of the
    expression is also unsigned 64-bit, for smaller or signed integer types
    returns a signed 64-bit integer
    """
    t = realtype(e.ctype())
    if isinstance(t, TInt) and dml.globals.compat_dml12_int(e.site):
        return e
    if not t.is_int:
        raise EBTYPE(e.site, e.ctype(), "integer type")
    if t.bits == 64 and not t.signed:
        target_type = TInt(64, False)
    else:
        target_type = TInt(64, True)
    if t.is_endian:
        (fun, funtype) = t.get_load_fun()
        e = dml.expr.Apply(e.site, mkLit(e.site, fun, funtype), (e,), funtype)
        if not compatible_types(realtype(e.ctype()), target_type):
            e = mkCast(e.site, e, target_type)
        return e
    else:
        return mkCast(e.site, e, target_type)

def truncate_int_bits(value, signed, bits=64):
    sign_bit = (1 << (bits - 1))
    mask = (1 << bits) - 1
    if signed:
        return ((value + sign_bit) & mask) - sign_bit
    else:
        return value & mask

class LValue(Expression):
    "Somewhere to read or write data"
    writable = True

    def write(self, source):
        rt = realtype(self.ctype())
        if isinstance(rt, TEndianInt):
            return (f'{rt.dmllib_fun("copy")}(&{self.read()},'
                    + f' {source.read()})\n')
        return '%s = %s' % (self.read(), source.read())

class IfExpr(Expression):
    priority = 30
    @auto_init
    def __init__(self, site, cond, texpr, fexpr, type): pass
    def __str__(self):
        return '%s ? %s : %s' % (self.cond, self.texpr, self.fexpr)
    def read(self):
        cond = self.cond.read()
        texpr = self.texpr.read()
        fexpr = self.fexpr.read()
        if self.cond.priority <= self.priority:
            cond = '(' + cond + ')'
        if self.texpr.priority <= self.priority:
            texpr = '(' + texpr + ')'
        if self.fexpr.priority <= self.priority:
            fexpr = '(' + fexpr + ')'
        return cond + ' ? ' + texpr + ' : ' + fexpr

def mkIfExpr(site, cond, texpr, fexpr):
    if dml.globals.compat_dml12_int(site):
        if logging.show_porting:
            # triggers PBITNEQ
            as_bool(cond)
        ttype = texpr.ctype()
        ftype = fexpr.ctype()
        # Coerce endianints
        if ttype.is_int and ttype.is_endian:
            texpr = as_int(texpr)
            ttype = texpr.ctype()
        if ftype.is_int and ftype.is_endian:
            fexpr = as_int(fexpr)
            ftype = texpr.ctype()
        utype = type_union(ttype, ftype)
        if cond.constant:
            # Normally handled by expr_conditional; this only happens
            # in DMLC-internal mkIfExpr calls
            (result, rtype) = (texpr, ttype) if cond.value else (fexpr, ftype)
            assert rtype.cmp(utype) == 0
            return result
        return IfExpr(site, cond, texpr, fexpr, utype)
    else:
        cond = as_bool(cond)
        ttype = safe_realtype(texpr.ctype())
        ftype = safe_realtype(fexpr.ctype())
        if ttype.is_arith or ftype.is_arith:
            ttype, texpr = arith_argument_conv(texpr)
            ftype, fexpr = arith_argument_conv(fexpr)

            if ttype.is_float or ftype.is_float:
                texpr = promote_float(texpr, ttype)
                fexpr = promote_float(fexpr, ftype)
                utype = TFloat('double')
            else:
                (texpr, fexpr, utype) = usual_int_conv(
                    texpr, ttype, fexpr, ftype)
        else:
            if not compatible_types(ttype, ftype):
                raise EBINOP(site, ':', texpr, fexpr)
            # TODO: in C, the rules are more complex,
            # but our type system is too primitive to cover that
            if (isinstance(ttype, (TPtr, TArray))
                and isinstance(ftype, (TPtr, TArray))):
                # if any branch is void, then the union is too
                base = (ftype if ftype.base.void else ttype).base.clone()
                # if any branch is const *, then the union is too
                base.const = ttype.base.const or ftype.base.const
                utype = TPtr(base)
            else:
                utype = ttype
        if cond.constant:
            # should be safe: texpr and fexpr now have compatible types
            return texpr if cond.value else fexpr
        return IfExpr(site, cond, texpr, fexpr, utype)

class BinOp(Expression):
    # op = a string
    @auto_init
    def __init__(self, site, lh, rh):
        assert_type(site, lh, Expression)
        assert_type(site, rh, Expression)
        assert_type(site, self.op, str)
    def __str__(self):
        lh = str(self.lh)
        rh = str(self.rh)
        if self.lh.priority <= self.priority:
            lh = '('+lh+')'
        if self.rh.priority <= self.priority:
            rh = '('+rh+')'
        return lh + ' ' + self.op + ' ' + rh
    def read(self):
        lh = self.lh.read()
        rh = self.rh.read()
        if self.lh.priority <= self.priority:
            lh = '('+lh+')'
        if self.rh.priority <= self.priority:
            rh = '('+rh+')'
        return lh + ' ' + self.op + ' ' + rh

    @classmethod
    def make(cls, site, lh, rh):
        # dbg('%s(%r, %r)' % (cls.__name__, lh, rh))

        lhtype = lh.ctype()
        rhtype = rh.ctype()
        if isinstance(lhtype, TUnknown) or isinstance(rhtype, TUnknown):
            return cls(site, lh, rh)

        return cls.make_simple(site, lh, rh)

class Test(Expression):
    "a boolean expression"
    type = TBool()

class Flag(Test):
    "a bit"
    @auto_init
    def __init__(self, site, expr): pass
    @property
    def priority(self):
        return self.expr.priority
    def __str__(self):
        return str(self.expr)
    def read(self):
        return self.expr.read()

def mkFlag(site, expr):
    if expr.constant:
        return BoolConstant(site, expr.value)
    else:
        return Flag(site, as_int(expr))

class Logical(BinOp):
    type = TBool()

    @auto_init
    def __init__(self, site, lh, rh):
        assert_type(site, lh.ctype(), TBool)
        assert_type(site, rh.ctype(), TBool)

class And(Logical):
    # gcc warns for priority = 50
    priority = 40
    op = '&&'

def mkAnd(site, lh, rh):
    if lh.constant:
        if lh.value:
            return rh
        else:
            return lh

    # Can't optimize away lh if it might have side-effects
    if rh.constant and rh.value:
        return lh

    return And(site, lh, rh)

class Or(Logical):
    priority = 40
    op = '||'

def mkOr(site, lh, rh):
    if lh.constant:
        if lh.value:
            return lh
        else:
            return rh

    # Can't optimize away lh if it might have side-effects
    if rh.constant and not rh.value:
        return lh

    return Or(site, lh, rh)

class Compare(BinOp):
    type = TBool()

    @abc.abstractproperty
    def cmp_functions(self):
        '''pair of dmllib.h functions for comparison between signed and
        unsigned integer, with (int, uint) and (uint, int) args,
        respectively'''

    @abc.abstractmethod
    def eval_const(self, lh, rh):
        pass

    @classmethod
    def make(cls, site, lh, rh):
        lhtype = realtype(lh.ctype())
        rhtype = realtype(rh.ctype())

        if (lhtype.is_arith and rhtype.is_arith
            and lh.constant and rh.constant):
            return mkBoolConstant(site, cls.eval_const(lh.value, rh.value))
        if lhtype.is_int:
            lh = as_int(lh)
            lhtype = realtype(lh.ctype())
        if rhtype.is_int:
            rh = as_int(rh)
            rhtype = realtype(rh.ctype())
        if (isinstance(lhtype, TInt)
            and isinstance(rhtype, TInt)
            and lhtype.signed != rhtype.signed):
            (signed_expr, unsigned_expr) = ((lh, rh) if lhtype.signed
                                            else (rh, lh))
            if signed_expr.constant and signed_expr.value < 0:
                report(WNEGCONSTCOMP(site, signed_expr,
                                     unsigned_expr.ctype()))
            # we must convert (uint64)x < (int64)y to DML_lt(x, y), because
            # C:'s < would do an unsigned comparison. No need to do this if y
            # is a small constant, though.
            # Respect bitage and signage
            if not (signed_expr.constant and 0 <= signed_expr.value < 1 << 63):
                return mkApply(
                    site, mkLit(
                        site, cls.cmp_functions[rhtype.signed],
                        TFunction([TInt(64, lhtype.signed),
                                   TInt(64, rhtype.signed)],
                                  TBool())),
                    [lh, rh])
        if ((lhtype.is_arith and rhtype.is_arith)
            or (isinstance(lhtype, (TPtr, TArray))
                and isinstance(rhtype, (TPtr, TArray))
                and compatible_types(lhtype.base, rhtype.base))):
            return cls.make_simple(site, lh, rh)
        raise EILLCOMP(site, lh, lhtype, rh, rhtype)

    @classmethod
    def make_simple(cls, site, lh, rh):
        return cls(site, lh, rh)

class Compare_dml12(Test):
    # op = ''
    equality = False
    @auto_init
    def __init__(self, site, lh, rh): pass
    def __str__(self):
        lh = str(self.lh)
        rh = str(self.rh)
        if self.lh.priority <= self.priority:
            lh = '('+lh+')'
        if self.rh.priority <= self.priority:
            rh = '('+rh+')'
        return lh + ' ' + self.op + ' ' + rh
    def read(self):
        lh = self.lh.read()
        rh = self.rh.read()
        if self.lh.priority <= self.priority:
            lh = '('+lh+')'
        if self.rh.priority <= self.priority:
            rh = '('+rh+')'
        return lh + ' ' + self.op + ' ' + rh

    @classmethod
    def make(cls, site, lh, rh):
        if isinstance(realtype(lh.ctype()), IntegerType):
            lh = as_int(lh)
        if isinstance(realtype(rh.ctype()), IntegerType):
            rh = as_int(rh)
        assert_comparable_types(site, lh, rh, cls.equality)
        # The assumption when calling Compare_dml12.make_simple is that lh
        # and rh are real, comparable expressions.
        return cls.make_simple(site, lh, rh)

class LessThan_dml12(Compare_dml12):
    priority = 70
    op = '<'

    @staticmethod
    def make_simple(site, lh, rh):
        if lh.constant and rh.constant:
            return mkBoolConstant(site, lh.value < rh.value)
        return LessThan_dml12(site, lh, rh)

class LessThan(Compare):
    priority = 70
    op = '<'
    cmp_functions = ('DML_lt_iu', 'DML_lt_ui')
    @staticmethod
    def eval_const(lh, rh):
        return lh < rh

def mkLessThan(site, lh, rh):
    if dml.globals.compat_dml12_int(site):
        return LessThan_dml12.make(site, lh, rh)
    else:
        return LessThan.make(site, lh, rh)

class LessThanOrEquals_dml12(Compare_dml12):
    priority = 70
    op = '<='

    @staticmethod
    def make_simple(site, lh, rh):
        if lh.constant and rh.constant:
            return mkBoolConstant(site, lh.value <= rh.value)
        return LessThanOrEquals_dml12(site, lh, rh)

# this is needed, because a<b != !(b<=a) if one operand is NaN
class LessThanOrEquals(Compare):
    priority = 70
    op = '<='
    cmp_functions = ('DML_leq_iu', 'DML_leq_ui')
    @staticmethod
    def eval_const(lh, rh):
        return lh <= rh

def mkLessThanOrEquals(site, lh, rh):
    if dml.globals.compat_dml12_int(site):
        return LessThanOrEquals_dml12.make(site, lh, rh)
    else:
        return LessThanOrEquals.make(site, lh, rh)

class GreaterThan_dml12(Compare_dml12):
    priority = 70
    op = '>'

    @staticmethod
    def make_simple(site, lh, rh):
        if lh.constant and rh.constant:
            return mkBoolConstant(site, lh.value > rh.value)
        return GreaterThan_dml12(site, lh, rh)

def mkGreaterThan(site, lh, rh):
    if dml.globals.compat_dml12_int(site):
        return GreaterThan_dml12.make(site, lh, rh)
    else:
        return LessThan.make(site, rh, lh)

class GreaterThanOrEquals_dml12(Compare_dml12):
    priority = 70
    op = '>='

    @staticmethod
    def make_simple(site, lh, rh):
        if lh.constant and rh.constant:
            return mkBoolConstant(site, lh.value >= rh.value)
        return GreaterThanOrEquals_dml12(site, lh, rh)

def mkGreaterThanOrEquals(site, lh, rh):
    if dml.globals.compat_dml12_int(site):
        return GreaterThanOrEquals_dml12.make(site, lh, rh)
    else:
        return LessThanOrEquals.make(site, rh, lh)

class Equals_dml12(Compare_dml12):
    priority = 70
    op = '=='
    equality = True

    @staticmethod
    def make_simple(site, lh, rh):
        if lh.constant and  rh.constant:
            return mkBoolConstant(site, lh.value == rh.value)
        return Equals_dml12(site, lh, rh)

class Equals(BinOp):
    priority = 70
    type = TBool()
    op = '=='

    @classmethod
    def make(cls, site, lh, rh):
        lhtype = realtype(lh.ctype())
        rhtype = realtype(rh.ctype())

        if lh.constant and rh.constant:
            if lhtype.is_arith and rhtype.is_arith:
                return mkBoolConstant(site, lh.value == rh.value)
            if isinstance(lh, BoolConstant) and isinstance(rh, BoolConstant):
                return mkBoolConstant(site, lh.value == rh.value)
            # This is probably the single weirdest widely used feature
            # of DML. Preserved because of use cases for which we
            # don't have a good replacement yet.
            if (isinstance(lh, StringConstant)
                and isinstance(rh, StringConstant)):
                return mkBoolConstant(site, lh.value == rh.value)
            if isinstance(lh, ObjIdentity) and isinstance(rh, ObjIdentity):
                lh_indices = [idx.value for idx in lh.indices]
                rh_indices = [idx.value for idx in rh.indices]
                return mkBoolConstant(site, (lh.node is rh.node
                                             and lh_indices == rh_indices))
        if lhtype.is_int:
            lh = as_int(lh)
            lhtype = realtype(lh.ctype())
        if rhtype.is_int:
            rh = as_int(rh)
            rhtype = realtype(rh.ctype())
        if (isinstance(lhtype, TInt) and isinstance(rhtype, TInt)
            and lhtype.signed != rhtype.signed):
            # There is no primitive for signed/unsigned compare in C,
            # so use a lib function for it. However, we can fall back
            # to C's == in the very common case when comparing
            # unsigned to a constant literal.
            (signed_expr, unsigned_expr) = ((lh, rh) if lhtype.signed
                                            else (rh, lh))
            if signed_expr.constant and signed_expr.value < 0:
                report(WNEGCONSTCOMP(site, signed_expr, unsigned_expr.ctype()))
            if not (signed_expr.constant and 0 <= signed_expr.value < 1 << 63):
                return mkApply(
                    site, mkLit(
                        site, 'DML_eq',
                        TFunction([TInt(64, True), TInt(64, False)], TBool())),
                    [lh, rh])

        if ((lhtype.is_arith and rhtype.is_arith)
            or (isinstance(lhtype, (TPtr, TArray))
                and isinstance(rhtype, (TPtr, TArray))
                and compatible_types(lhtype, rhtype))
            or (isinstance(lhtype, TBool) and isinstance(rhtype, TBool))):
            return Equals(site, lh, rh)

        def mkIdentityEq(lh, rh):
            return mkApply(site,
                           mkLit(site, '_identity_eq',
                                 TFunction([TObjIdentity(), TObjIdentity()],
                                           TBool())),
                           [lh, rh])

        # Equality between trait references of the same template is
        # implicitly converted to equality between identities
        if (isinstance(lhtype, TTrait) and isinstance(rhtype, TTrait)
            and lhtype.trait is rhtype.trait):
            return mkIdentityEq(TraitObjIdentity(lh.site, lh),
                                TraitObjIdentity(rh.site, rh))

        if (isinstance(lhtype, TObjIdentity)
            and isinstance(rhtype, TObjIdentity)):
            return mkIdentityEq(lh, rh)
        raise EILLCOMP(site, lh, lhtype, rh, rhtype)

def mkEquals(site, lh, rh):
    if dml.globals.compat_dml12_int(site):
        return Equals_dml12.make(site, lh, rh)
    else:
        return Equals.make(site, lh, rh)

class NotEquals_dml12(Compare_dml12):
    priority = 70
    op = '!='
    equality = True

    @staticmethod
    def make_simple(site, lh, rh):
        # expand bitslice expressions
        if isinstance(lh, BitSlice):
            lh = lh.read_expr

        if lh.constant and rh.constant:
            return mkBoolConstant(site, lh.value != rh.value)
        return NotEquals_dml12(site, lh, rh)

def mkNotEquals(site, lh, rh):
    if dml.globals.compat_dml12_int(site):
        return NotEquals_dml12.make(site, lh, rh)
    else:
        return mkNot(site, Equals.make(site, lh, rh))

def usual_int_conv(lh, lhtype, rh, rhtype):
    assert lhtype.is_int and rhtype.is_int
    lh_is_uint64 = lhtype.bits == 64 and not lhtype.signed
    rh_is_uint64 = rhtype.bits == 64 and not rhtype.signed
    if lh_is_uint64 and rh_is_uint64:
        return (lh, rh, lhtype)
    if lh_is_uint64:
        return (lh, mkCast(rh.site, rh, lhtype), lhtype)
    if rh_is_uint64:
        return (mkCast(lh.site, lh, rhtype), rh, rhtype)
    int64 = TInt(64, True)
    if lhtype.bits != 64:
        lh = mkCast(lh.site, lh, int64)
    if rhtype.bits != 64:
        rh = mkCast(rh.site, rh, int64)
    return (lh, rh, int64)

class BitBinOp_dml12(BinOp):
    slots = ('type',)
    @auto_init
    def __init__(self, site, lh, rh):
        self.type = self.detect_type(lh, rh)

    @staticmethod
    def detect_type(lh, rh):
        ltype = lh.ctype()
        rtype = rh.ctype()
        return type_union(ltype, rtype)
    def __str__(self):
        lh = str(self.lh)
        rh = str(self.rh)
        if ((self.lh.priority <= self.priority) or
            isinstance(self.lh, ArithBinOp_dml12)):
            lh = '('+lh+')'
        if ((self.rh.priority <= self.priority) or
            isinstance(self.rh, ArithBinOp_dml12)):
            rh = '('+rh+')'
        return lh + ' ' + self.op + ' ' + rh
    def read(self):
        # Code like "x & y + z" is interpreted as "x & (y + z)".
        # However, gcc will warn, and it isn't obvious, so we add
        # parentheses.
        lh = self.lh.read()
        rh = self.rh.read()
        if ((self.lh.priority <= self.priority) or
            isinstance(self.lh, ArithBinOp_dml12)):
            lh = '('+lh+')'
        if ((self.rh.priority <= self.priority) or
            isinstance(self.rh, ArithBinOp_dml12)):
            rh = '('+rh+')'
        return lh + ' ' + self.op + ' ' + rh

class BitBinOp(BinOp):
    @auto_init
    def __init__(self, site, lh, rh, type):
        '''lh and rh must have 64-bit integer types with the given signedness'''
    def __str__(self):
        return '(%s) %s (%s)' % (self.lh, self.op, self.rh)
    def read(self):
        return '(%s) %s (%s)' % (self.lh.read(), self.op, self.rh.read())
    @abc.abstractmethod
    def eval_const(left, right): pass
    @classmethod
    def make_simple(cls, site, lh, rh):
        ltype, lh = arith_argument_conv(lh)
        rtype, rh = arith_argument_conv(rh)

        (lh, rh, common_type) = usual_int_conv(lh, ltype, rh, rtype)
        if lh.constant and rh.constant:
            return mkIntegerConstant(site, cls.eval_const(lh.value, rh.value),
                                     common_type.signed)
        return cls(site, lh, rh, common_type)

class BitAnd_dml12(BitBinOp_dml12):
    priority = 70
    op = '&'

    @staticmethod
    def detect_type(lh, rh):
        if lh.constant:
            return lh.ctype()
        elif rh.constant:
            return rh.ctype()
        else:
            return BitBinOp_dml12.detect_type(lh, rh)

    @staticmethod
    def make_simple(site, lh, rh):
        lh = as_int(lh)
        rh = as_int(rh)
        ltype = realtype(lh.ctype())
        rtype = realtype(rh.ctype())

        if lh.constant and rh.constant:
            return IntegerConstant_dml12(site, lh.value & rh.value)

        expr = BitAnd_dml12(site, lh, rh)
        etype = realtype(expr.ctype())
        if not etype.is_int:
            raise ICE(site,
                       "Strange! ANDed to non-integer ('%s' & '%s' -> '%s')"
                       % (ltype, rtype, etype))
        if etype.bits == 0:
            raise ICE(site,
                       "Strange! ANDing to empty type (%s)\n"
                       + binary_dump(lh, rh))

        if etype.bits < ltype.bits or etype.bits < rtype.bits:
            expr = mkCast(site, expr, etype)
        elif etype.bits <= 32 and (ltype.bits > 32 or rtype.bits > 32):
            expr = mkCast(site, expr, etype)

        return expr

class BitAnd(BitBinOp):
    priority = 70
    op = '&'
    @staticmethod
    def eval_const(lh, rh):
        return lh & rh

def mkBitAnd(site, lh, rh):
    if dml.globals.compat_dml12_int(site):
        return BitAnd_dml12.make(site, lh, rh)
    else:
        return BitAnd.make(site, lh, rh)

class BitOr_dml12(BitBinOp_dml12):
    priority = 70
    op = '|'

    @staticmethod
    def make_simple(site, lh, rh):
        lh = as_int(lh)
        rh = as_int(rh)

        if lh.constant and rh.constant:
            return IntegerConstant_dml12(site, lh.value | rh.value)

        # (X | 0)  =>  X
        if rh.constant and rh.value == 0:
            return lh

        return BitOr_dml12(site, lh, rh)

class BitOr(BitBinOp):
    priority = 70
    op = '|'
    @staticmethod
    def eval_const(lh, rh):
        return lh | rh

def mkBitOr(site, lh, rh):
    if dml.globals.compat_dml12_int(site):
        return BitOr_dml12.make(site, lh, rh)
    else:
        return BitOr.make(site, lh, rh)

class BitXOr_dml12(BitBinOp_dml12):
    priority = 70
    op = '^'

    @staticmethod
    def make_simple(site, lh, rh):
        lh = as_int(lh)
        rh = as_int(rh)

        if lh.constant and rh.constant:
            return IntegerConstant_dml12(site, lh.value ^ rh.value)
        return BitXOr_dml12(site, lh, rh)

class BitXOr(BitBinOp):
    priority = 70
    op = '^'
    @staticmethod
    def eval_const(lh, rh):
        return lh ^ rh

def mkBitXOr(site, lh, rh):
    if dml.globals.compat_dml12_int(site):
        return BitXOr_dml12.make(site, lh, rh)
    else:
        return BitXOr.make(site, lh, rh)

class BitShift_dml12(BitBinOp_dml12):
    priority = 110

class ShL_dml12(BitShift_dml12):
    op = '<<'

    @staticmethod
    def detect_type(lh, rh):
        return TInt(64, False)

    @staticmethod
    def make_simple(site, lh, rh):
        lh = as_int(lh)
        rh = as_int(rh)

        if rh.constant:
            if rh.value < 0:
                # it's an error in Python to shift a negative number of bits
                # (better raise an error instead of e.g. substituting 0)
                raise ESHNEG(site, rh)
            elif rh.value == 0:
                return lh

            if lh.constant:
                return IntegerConstant_dml12(site, lh.value << rh.value)

        # expand bitslice expressions
        if isinstance(lh, BitSlice):
            lh = lh.read_expr

        # Pattern match:
        #   0 << A  ->  0
        if (lh.constant and lh.value == 0):
            return lh

        # Match (((A >> B) & C) << D) with constant shifts
        if (rh.constant and isinstance(lh, BitAnd)
            and isinstance(lh.lh, ShR_dml12)
            and lh.lh.rh.constant):
            # Replace with ((A & (C << B)) << (D-B)), which has a better
            # chance of constant propagation.
            shiftval = mkSubtract(site, rh, lh.lh.rh)
            lh = mkBitAnd(site, lh.lh.lh, mkShL(site, lh.rh, lh.lh.rh))
            if shiftval.value == 0:
                return lh
            elif shiftval.value > 0:
                return mkShL(site, lh, shiftval)
            else:
                # dirty optimization
                shiftval.value = -shiftval.value
                return mkShR(site, lh, shiftval)

        lhtype = safe_realtype(lh.ctype())
        if lhtype.bits < 64 or lhtype.signed:
            lh = mkCast(site, lh, TInt(64, False))

        return ShL_dml12(site, lh, rh)

class BitShift(BinOp):
    priority = 110
    @auto_init
    def __init__(self, site, lh, rh, type):
        '''lh and rh must have 64-bit integer types with the given signedness'''
    def __str__(self):
        return '(%s) %s (%s)' % (self.lh, self.op, self.rh)
    @abc.abstractmethod
    def eval_const(left, right): pass
    @classmethod
    def make_simple(cls, site, lh, rh):
        ltype, lh = arith_argument_conv(lh)
        rtype, rh = arith_argument_conv(rh)

        (lh, rh, common_type) = usual_int_conv(lh, ltype, rh, rtype)
        if rh.constant and rh.value < 0:
            raise ESHNEG(site, rh)
        if lh.constant and rh.constant:
            return mkIntegerConstant(site, cls.eval_const(lh.value, rh.value),
                                     common_type.signed)
        return cls(site, lh, rh, common_type)

class ShL(BitShift):
    op = '<<'
    @staticmethod
    def eval_const(lh, rh):
        # compile-time optimization: if rh is huge, it's expensive to
        # create an explicit representation of lh << rh, and we know
        # it will be truncated to 0 anyway
        return lh << rh if rh < 64 else 0

    def read(self):
        if self.type.signed:
            return ('DML_shl(%s, %s, "%s", %s)'
                    % (self.lh.read(), self.rh.read(),
                       quote_filename(self.site.filename()),
                       self.site.lineno))
        else:
            return 'DML_shlu(%s, %s)' % (self.lh.read(), self.rh.read())

def mkShL(site, lh, rh):
    if dml.globals.compat_dml12_int(site):
        return ShL_dml12.make(site, lh, rh)
    else:
        return ShL.make(site, lh, rh)

class ShR_dml12(BitShift_dml12):
    op = '>>'

    @staticmethod
    def type_shift(site, etype, shift):
        etype = realtype(etype)
        if not etype.is_int:
            raise ICE(site, "Shifting a non-integer")
        bits = etype.bits + shift
        if bits < 0:
            bits = 0
        if bits > 64:
            bits = 64
        return TInt(bits, etype.signed if etype.is_int else False)

    @staticmethod
    def detect_type(lh, rh):
        if rh.constant:
            ltype = lh.ctype()
            if isinstance(ltype, TUnknown):
                return ltype
            return ShR_dml12.type_shift(lh.site, ltype, -rh.value)
        else:
            return TInt(64, False)

    @staticmethod
    def make_simple(site, lh, rh):
        lh = as_int(lh)
        rh = as_int(rh)

        if rh.constant:
            if rh.value < 0:
                # it's an error in Python to shift a negative number of bits
                # (better raise an error instead of e.g. substituting 0)
                raise ESHNEG(site, rh)
            elif rh.value == 0:
                return lh

            if lh.constant:
                return IntegerConstant_dml12(site, lh.value >> rh.value)

        expr = ShR_dml12(site, lh, rh)
        etype = realtype(expr.ctype())
        assert etype.is_int
        ltype = realtype(lh.ctype())
        if etype.bits < 1:
            report(WSHALL(site, lh, rh))
        elif ltype.bits > 32 and etype.bits <= 32:
            expr = mkCast(site, expr, etype)
        return expr

class ShR(BitShift):
    op = '>>'
    @staticmethod
    def eval_const(lh, rh):
        if rh < 64:
            return lh >> rh
        else:
            return -1 if lh < 0 else 0

    def read(self):
        if self.type.signed:
            return ('DML_shr(%s, %s, "%s", %s)'
                    % (self.lh.read(), self.rh.read(),
                       quote_filename(self.site.filename()),
                       self.site.lineno))
        else:
            return 'DML_shru(%s, %s)' % (self.lh.read(), self.rh.read())

def mkShR(site, lh, rh):
    if dml.globals.compat_dml12_int(site):
        return ShR_dml12.make(site, lh, rh)
    else:
        return ShR.make(site, lh, rh)

class ArithBinOp_dml12(BinOp):
    slots = ('type',)

    @auto_init
    def __init__(self, site, lh, rh):
        self.type = self.detect_type(lh, rh)

    @staticmethod
    def detect_type(lh, rh):
        ltype = safe_realtype(lh.ctype())
        rtype = safe_realtype(rh.ctype())
        if isinstance(ltype, TUnknown) or isinstance(rtype, TUnknown):
            return TUnknown()

        # This is actually only valid for add/sub, but put it here for
        # convenience
        if (isinstance(ltype, (TPtr, TArray))
            and isinstance(rtype, (TPtr, TArray))):
            return TNamed('int') # actually ptrdiff_t
        if isinstance(ltype, (TPtr, TArray)) and rtype.is_int:
            return ltype
        if isinstance(rtype, (TPtr, TArray)) and ltype.is_int:
            return rtype

        if ltype.is_float or rtype.is_float:
            return TFloat('double')
        assert ltype.is_int and rtype.is_int
        if ltype.bits > rtype.bits:
            return ltype
        else:
            return rtype
    def __str__(self):
        # The expression "a-(b+c)" is printed like that, but we want
        # to print "(a+b)-c" as "a+b-c" instead.  Arithmetic
        # operations are left-associative.
        lh = str(self.lh)
        rh = str(self.rh)
        if isinstance(self.lh, ArithBinOp_dml12):
            if (self.lh.priority < self.priority):
                lh = '('+lh+')'
        else:
            if (self.lh.priority <= self.priority):
                lh = '('+lh+')'
        if (self.rh.priority <= self.priority):
            rh = '('+rh+')'
        return lh + ' ' + self.op + ' ' + rh
    def read(self):
        # The expression "a-(b+c)" is printed like that, but we want
        # to print "(a+b)-c" as "a+b-c" instead.  Arithmetic
        # operations are left-associative.
        lh = self.lh.read()
        rh = self.rh.read()
        if isinstance(self.lh, ArithBinOp_dml12):
            if (self.lh.priority < self.priority):
                lh = '('+lh+')'
        else:
            if (self.lh.priority <= self.priority):
                lh = '('+lh+')'
        if (self.rh.priority <= self.priority):
            rh = '('+rh+')'
        return lh + ' ' + self.op + ' ' + rh

class ArithBinOp(BinOp):
    @auto_init
    def __init__(self, site, lh, rh, type):
        '''lh and rh must have types that produce res_type in C'''
    def __str__(self):
        return '(%s) %s (%s)' % (self.lh, self.op, self.rh)
    @abc.abstractmethod
    def eval_const(left, right): pass
    @classmethod
    def make_simple(cls, site, lh, rh):
        ltype, lh = arith_argument_conv(lh)
        rtype, rh = arith_argument_conv(rh)

        if ltype.is_float or rtype.is_float:
            lh = promote_float(lh, ltype)
            rh = promote_float(rh, rtype)
            return cls(site, lh, rh, TFloat('double'))

        int64_result = ((ltype.bits < 64 or ltype.signed)
                        and (rtype.bits < 64 or rtype.signed))
        if lh.constant and rh.constant:
            return mkIntegerConstant(site, cls.eval_const(lh.value, rh.value),
                                     int64_result)
        uint64 = TInt(64, False)
        result = cls(site, mkCast(site, lh, uint64),
                     mkCast(site, rh, uint64), uint64)
        if int64_result:
            result = mkCast(site, result, TInt(64, True))
        return result

class Mult_dml12(ArithBinOp_dml12):
    priority = 130
    op = '*'

    @staticmethod
    def make_simple(site, lh, rh):
        lhtype, lh = arith_argument_conv(lh)
        rhtype, rh = arith_argument_conv(rh)

        if lh.constant and rh.constant:
            if (isinstance(lh.value, int)
                and isinstance(rh.value, int)):
                return IntegerConstant_dml12(site, lh.value * rh.value)
            else:
                # don't try to be smart about floating-point
                pass

        # Simplify the code
        if lh.constant and lhtype.is_int:
            if lh.value == 1:
                return rh
        if rh.constant and rhtype.is_int:
            if rh.value == 1:
                return lh

        return Mult_dml12(site, lh, rh)

class Mult(ArithBinOp):
    priority = 130
    op = '*'
    @staticmethod
    def eval_const(left, right):
        return left * right

def mkMult(site, lh, rh):
    if dml.globals.compat_dml12_int(site):
        return Mult_dml12.make(site, lh, rh)
    else:
        return Mult.make(site, lh, rh)

class Div_dml12(ArithBinOp_dml12):
    priority = 130
    op = '/'

    @staticmethod
    def make_simple(site, lh, rh):
        lhtype, lh = arith_argument_conv(lh)
        rhtype, rh = arith_argument_conv(rh)

        if rh.constant and rhtype.is_int:
            if rh.value == 0:
                raise EDIVZ(rh, '/')
            elif rh.value == 1:
                return lh
            elif lh.constant and lhtype.is_int:
                return IntegerConstant_dml12(site, lh.value // rh.value)
        return Div_dml12(site, lh, rh)

class DivModOp(ArithBinOp):
    @classmethod
    def make_simple(cls, site, lh, rh):
        lhtype, lh = arith_argument_conv(lh)
        rhtype, rh = arith_argument_conv(rh)

        if lhtype.is_float or rhtype.is_float:
            lh = promote_float(lh, lhtype)
            rh = promote_float(rh, rhtype)
            return cls(site, lh, rh, TFloat('double'))

        (lh, rh, common_type) = usual_int_conv(lh, lhtype, rh, rhtype)
        if lh.constant and rh.constant:
            return mkIntegerConstant(site, cls.eval_const(lh.value, rh.value),
                                     common_type.signed)
        return cls(site, lh, rh, common_type)

class Div(DivModOp):
    priority = 130
    op = '/'
    @staticmethod
    def eval_const(left, right):
        if right == 0:
            raise EDIVZ(right, '/')
        if (left < 0) == (right < 0):
            return left // right
        else:
            return -(abs(left) // abs(right))
    def read(self):
        if self.type.is_float:
            return ArithBinOp.read(self)
        return ('%s(%s, %s, "%s", %s)'
                    % ('DML_div' if self.type.signed else 'DML_divu',
                       self.lh.read(), self.rh.read(),
                       quote_filename(self.site.filename()),
                       self.site.lineno))

def mkDiv(site, lh, rh):
    if dml.globals.compat_dml12_int(site):
        return Div_dml12.make(site, lh, rh)
    else:
        return Div.make(site, lh, rh)

class Mod_dml12(ArithBinOp_dml12):
    priority = 130
    op = '%'

    @staticmethod
    def make_simple(site, lh, rh):
        lhtype, lh = arith_argument_conv(lh)
        rhtype, rh = arith_argument_conv(rh)

        if rh.constant:
            if rh.value == 0:
                raise EDIVZ(rh, '%')
            elif lh.constant:
                return IntegerConstant_dml12(site, lh.value % rh.value)

        return Mod_dml12(site, lh, rh)

# We count mod as a bit operation, because it operates on integers only
class Mod(DivModOp):
    priority = 130
    op = '%'
    @staticmethod
    def eval_const(lh, rh):
        if rh == 0:
            raise EDIVZ(rh, '%')
        if lh < 0:
            return -(abs(lh) % abs(rh))
        else:
            return lh % abs(rh)

    def read(self):
        return ('%s(%s, %s, "%s", %s)'
                    % ('DML_mod' if self.type.signed else 'DML_modu',
                       self.lh.read(), self.rh.read(),
                       quote_filename(self.site.filename()),
                       self.site.lineno))

def mkMod(site, lh, rh):
    if dml.globals.compat_dml12_int(site):
        return Mod_dml12.make(site, lh, rh)
    else:
        return Mod.make(site, lh, rh)

class Add_dml12(ArithBinOp_dml12):
    # gcc warns for priority = 120
    priority = 110
    op = '+'

    @staticmethod
    def make_simple(site, lh, rh):
        # There is a special case here that allows addition of string
        # constants
        if (isinstance(lh, StringConstant) and isinstance(rh, StringConstant)):
            return mkStringConstant(site, lh.value + rh.value)

        lhtype = realtype(lh.ctype())
        rhtype = realtype(rh.ctype())

        # Type check
        if lhtype.is_arith and rhtype.is_arith:
            pass
        elif lhtype.is_int and isinstance(rhtype, (TPtr, TArray)):
            pass
        elif isinstance(lhtype, (TPtr, TArray)) and rhtype.is_int:
            pass
        else:
            raise EBINOP(site, '+', lh, rh)

        # Constant folding
        if lh.constant and rh.constant and lhtype.is_int and rhtype.is_int:
            return IntegerConstant_dml12(site, lh.value + rh.value)

        # Simplifications
        if lh.constant and lh.value == 0:
            return rh
        if rh.constant and rh.value == 0:
            return lh

        # Pattern match:
        #   (A + C1) + C2  ->  A + (C1 + C2)
        if rh.constant and isinstance(lh, Add) and lh.rh.constant:
            return mkAdd(site, lh.lh, mkAdd(site, lh.rh, rh))
        # Pattern match:
        #   (A - C1) + C2  ->  A + (C2 - C1)
        if rh.constant and isinstance(lh, Subtract) and lh.rh.constant:
            return mkAdd(site, lh.lh, mkSubtract(site, rh, lh.rh))

        if not isinstance(lhtype, (TPtr, TArray)):
            _, lh = arith_argument_conv(lh)
        if not isinstance(rhtype, (TPtr, TArray)):
            _, rh = arith_argument_conv(rh)

        return Add_dml12(site, lh, rh)

class Add(ArithBinOp):
    priority = 110
    op = '+'
    @staticmethod
    def eval_const(left, right):
        return left + right

    @staticmethod
    def make_simple(site, lh, rh):
        if isinstance(lh, StringConstant) and isinstance(rh, StringConstant):
            return StringConstant(site, lh.value + rh.value)
        lhtype = realtype(lh.ctype())
        rhtype = realtype(rh.ctype())
        # ECSADD should always be emitted when the operand types are equivalent
        # to char pointers/arrays -- even including when the operands are
        # explicitly typed as int8 pointers/arrays
        if (isinstance(lhtype, (TArray, TPtr))
            and isinstance(rhtype, (TArray, TPtr))
            and isinstance(lhtype.base, TInt) and isinstance(rhtype.base, TInt)
            and lhtype.base.bits == 8 and rhtype.base.bits == 8
            and lhtype.base.signed and rhtype.base.signed):
            raise ECSADD(site)
        if (isinstance(lhtype, (TArray, TPtr))
            and not lhtype.base.void):
            rtype, rh = arith_argument_conv(rh)
            return Add(site, lh, rh, lhtype)
        elif (isinstance(rhtype, (TArray, TPtr))
              and not rhtype.base.void):
            ltype, lh = arith_argument_conv(lh)
            return Add(site, lh, rh, rhtype)
        return super(Add, Add).make_simple(site, lh, rh)

def mkAdd(site, lh, rh):
    if dml.globals.compat_dml12_int(site):
        return Add_dml12.make(site, lh, rh)
    else:
        return Add.make(site, lh, rh)

class Subtract_dml12(ArithBinOp_dml12):
    # gcc warns for priority = 120
    priority = 110
    op = '-'

    @staticmethod
    def make_simple(site, lh, rh):
        lhtype = realtype(lh.ctype())
        rhtype = realtype(rh.ctype())

        # Type check
        if (isinstance(lhtype, (TPtr, TArray))
            and isinstance(rhtype, (TPtr, TArray, IntegerType))):
            pass
        elif lhtype.is_int and isinstance(rhtype, (TPtr, TArray)):
            pass
        elif lhtype.is_arith and rhtype.is_arith:
            pass
        else:
            raise EBINOP(site, '-', lh, rh)

        # Constant folding
        if (lh.constant and rh.constant
            and isinstance(lh.value, int)
            and isinstance(rh.value, int)):
            return IntegerConstant_dml12(site, lh.value - rh.value)

        # Simplifications
        if rh.constant and rh.value == 0:
            return lh
        # Pattern match:
        #   (A + C1) - C2  ->  A + (C1 - C2)
        if rh.constant and isinstance(lh, Add) and lh.rh.constant:
            return mkAdd(site, lh.lh, mkSubtract(site, lh.rh, rh))
        # Pattern match:
        #   (A - C1) - C2  ->  A - (C1 + C2)
        if rh.constant and isinstance(lh, Subtract) and lh.rh.constant:
            return mkSubtract(site, lh.lh, mkAdd(site, lh.rh, rh))
        # Pattern match:
        #   (C1 + A) - C2  ->  A + (C1 - C2)
        if rh.constant and isinstance(lh, Add) and lh.lh.constant:
            return lh.make(site, lh.rh, mkSubtract(site, lh.lh, rh))
        # Constant movement:
        #   (A - C) - B  ->  (A - B) - C
        if isinstance(lh, Subtract) and lh.rh.constant:
            return mkSubtract(site, mkSubtract(site, lh.lh, rh), lh.rh)

        if not isinstance(lhtype, (TPtr, TArray)):
            _, lh = arith_argument_conv(lh)
        if not isinstance(rhtype, (TPtr, TArray)):
            _, rh = arith_argument_conv(rh)

        return Subtract_dml12(site, lh, rh)

class Subtract(ArithBinOp):
    priority = 110
    op = '-'
    @staticmethod
    def eval_const(left, right):
        return left - right
    @staticmethod
    def make_simple(site, lh, rh):
        lhtype = realtype(lh.ctype())
        rhtype = realtype(rh.ctype())
        if (isinstance(lhtype, (TArray, TPtr))
            and not lhtype.base.void):
            if (isinstance(rhtype, (TArray, TPtr))
                and not rhtype.base.void):
                # ptrdiff case
                return Subtract(site, lh, rh, TInt(64, True))
            else:
                rh = as_int(rh)
                return Subtract(site, lh, rh, lhtype)
        elif (isinstance(rhtype, (TArray, TPtr))
              and not rhtype.base.void):
            lh = as_int(lh)
            return Subtract(site, lh, rh, rhtype)
        _, lh = arith_argument_conv(lh)
        _, rh = arith_argument_conv(rh)
        return super(Subtract, Subtract).make_simple(site, lh, rh)

def mkSubtract(site, lh, rh):
    if dml.globals.compat_dml12_int(site):
        return Subtract_dml12.make(site, lh, rh)
    else:
        return Subtract.make(site, lh, rh)

def source_for_assignment(site, target_type, source):
    """Check that 'source' can be assigned to a variable of type 'target_type'
    and return the source expression, maybe updated."""
    try:
        source_type = source.ctype()
        if isinstance(source_type, TUnknown):
            raise ENTYPE(site)
        real_target_type = realtype(target_type)
        real_source_type = realtype(source_type)
        ok, trunc, constviol = real_target_type.canstore(real_source_type)
        if constviol:
            raise EDISCONST(site)
        if not ok:
            # Assigning boolean values to one-bit targets is ok
            if (isinstance(real_target_type, TInt)
                and real_target_type.bits == 1
                and isinstance(real_source_type, TBool)):
                # Using IfExpr is a little overhead, but probably
                # not a problem
                if isinstance(source, Not):
                    source = mkIfExpr(site, source.rh,
                                      mkIntegerLiteral(site, 0),
                                      mkIntegerLiteral(site, 1))
                else:
                    source = mkIfExpr(site, source,
                                      mkIntegerLiteral(site, 1),
                                      mkIntegerLiteral(site, 0))
            else:
                raise EASTYPE(site, target_type, source)
        if ((isinstance(real_target_type, TInt)
             and not dml.globals.compat_dml12_int(site))
            or (real_target_type.is_int and real_target_type.is_endian)
            or (real_source_type.is_int and real_source_type.is_endian)):
            # For TInt, possibly truncate upper bits or coerce to endianint
            # For endian type, coerce to endianint struct
            source = mkCast(site, source, target_type)

    except DMLUnknownType as e:
        raise ETYPE(site, e.type)
    return source

class AssignOp(BinOp):
    priority = 20
    op = '='

    def __str__(self):
        return "%s = %s" % (self.lh, self.rh)

    def discard(self):
        return self.lh.write(self.rh)

    def read(self):
        return '((%s), (%s))' % (self.discard(), self.lh.read())

    def ctype(self):
        return self.lh.ctype()

def mkAssignOp(site, target, source):
    if isinstance(target, InlinedParam):
        raise EASSINL(target.site, target.name)
    if not target.writable:
        raise EASSIGN(site, target)

    target_type = target.ctype()

    source = source_for_assignment(site, target_type, source)
    if safe_realtype(target_type).const:
        raise ECONST(site)
    return AssignOp(site, target, source)

class UnaryOp(Expression):
    priority = 150
    @auto_init
    def __init__(self, site, rh): pass
    def __str__(self):
        rh = str(self.rh)
        if self.rh.priority <= self.priority:
            rh = '('+rh+')'
        return str(self.op) + rh
    def read(self):
        rh = self.rh.read()
        if self.rh.priority <= self.priority:
            rh = '('+rh+')'
        return str(self.op) + rh

    @classmethod
    def make(cls, site, rh):
        return cls.make_simple(site, rh)

    @classmethod
    def make_simple(cls, site, rh):
        return cls(site, rh)

class ArithUnaryOp(UnaryOp):
    @classmethod
    def make(cls, site, rh):
        (_, rh) = arith_argument_conv(rh)
        return super(ArithUnaryOp, cls).make(site, rh)

class AddressOf(UnaryOp):
    slots = ('type',)
    op = '&'
    @auto_init
    def __init__(self, site, rh):
        self.type = TPtr(rh.ctype())
    def read(self):
        if hasattr(self.rh, 'read_pointer'):
            return self.rh.read_pointer()
        return UnaryOp.read(self)
    @classmethod
    def make_simple(cls, site, rh):
        # Hack around a problem in the 3.0 API. This is still needed
        # in DML 1.2 only to support "stacked" events.
        if dml.globals.dml_version == (1, 2) and isinstance(rh, NodeRef):
            node, _ = rh.get_ref()
            if node.objtype == 'method' \
               and node.name == 'callback' \
               and node.parent.objtype == 'event':
                return AddressOf(site, mkLit(
                    site, '_DML_EV_'+crep.cref(node),
                    TFunction([TPtr(TNamed('conf_object_t')),
                               TPtr(TVoid())],
                              TVoid())))
        if not dml.globals.compat_dml12 and not isinstance(rh, LValue):
            raise ERVAL(rh.site, '&')
        return AddressOf(site, rh)

def mkAddressOf(site, rh):
    if dml.globals.compat_dml12_int(site):
        t = safe_realtype(rh.ctype())
        if t.is_int and t.is_endian:
            return mkCast(site, AddressOf.make(site, rh),
                          TPtr(TInt(t.bits, t.signed, t.members)))
    return AddressOf.make(site, rh)

class Dereference(UnaryOp, LValue):
    slots = ('type',)
    op = '*'
    explicit_type = True
    def __init__(self, site, rh):
        super(Dereference, self).__init__(site, rh)
        typ = realtype_shallow(self.rh.ctype())
        if isinstance(typ, TPtr):
            self.type = typ.base
        elif isinstance(typ, TUnknown):
            self.type = typ
        else:
            raise ICE(self.site, "unknown expression type")

    @staticmethod
    def make_simple(site, rh):
        etype = realtype(rh.ctype())

        if etype and not isinstance(etype, TPtr):
            raise ENOPTR(site, rh)
        return Dereference(site, rh)

mkDereference = Dereference.make

class Not(UnaryOp):
    op = '!'
    type = TBool()

    @staticmethod
    def make_simple(site, rh):
        if not dml.globals.compat_dml12:
            rh = as_bool(rh)
        if rh.constant:
            return mkBoolConstant(site, not rh.value)
        return Not(site, rh)

mkNot = Not.make

class BitNot_dml12(ArithUnaryOp):
    op = '~'

    def ctype(self):
        return self.rh.ctype()

class BitNot(ArithUnaryOp):
    op = '~'

    @auto_init
    def __init__(self, site, rh, signed): pass

    def ctype(self):
        return TInt(64, self.signed)

    @staticmethod
    def make_simple(site, rh):
        rh = as_int(rh)
        rhtype = realtype(rh.ctype())
        if rh.constant:
            return mkIntegerConstant(site, ~rh.value, rhtype.signed)
        return BitNot(site, rh, rhtype.signed)

def mkBitNot(site, rh):
    if dml.globals.compat_dml12_int(site):
        return BitNot_dml12.make(site, rh)
    else:
        return BitNot.make(site, rh)

def arith_argument_conv(expr):
    """Expect argument to be an integer, float, or double
    return (type, expr) where expr is potentially modified"""
    etype = safe_realtype(expr.ctype())
    if etype.is_float:
        return (etype, expr)
    else:
        try:
            new_expr = as_int(expr)
        except EBTYPE:
            # This translation result in slightly more informative errors
            raise EBTYPE(expr.site, expr.ctype(), "float or integer")
        return (safe_realtype(new_expr.ctype()), new_expr)

def promote_integer(expr, etype):
    assert etype.is_int
    signed = etype.bits < 64 or etype.signed
    return (mkCast(expr.site, expr, TInt(64, signed)), signed)

def promote_float(expr, etype):
    assert etype.is_arith
    if etype.is_float and etype.name == 'double':
        return expr
    if expr.constant:
        return FloatConstant(expr.site, float(expr.value))
    else:
        return mkCast(expr.site, expr, TFloat('double'))

class UnaryMinus(ArithUnaryOp):
    op = '-'
    def ctype(self):
        origtype = self.rh.ctype()
        t = safe_realtype(origtype)
        if t.is_int:
            if dml.globals.compat_dml12_int(self.site):
                return TInt(t.bits, True)
            else:
                assert t.bits == 64
                return t
        else:
            assert t.is_float
            return t

    @staticmethod
    def make_simple(site, rh):
        rhtype = safe_realtype(rh.ctype())

        if rhtype.is_float:
            if not dml.globals.compat_dml12_int(site):
                rh = promote_float(rh, rhtype)
        elif not isinstance(rhtype, TInt):
            raise ICE(site, "Unexpected wrong type of argument to unary minus")

        if rh.constant:
            if isinstance(rh, IntegerConstant):
                if dml.globals.compat_dml12_int(site):
                    return IntegerConstant_dml12(site, -rh.value)
                return mkIntegerConstant(site, -rh.value, rhtype.signed)
            elif isinstance(rh, FloatConstant):
                return mkFloatConstant(site, -rh.value)

        return UnaryMinus(site, rh)

mkUnaryMinus = UnaryMinus.make

def mkUnaryPlus(site, rh):
    rhtype, rh = arith_argument_conv(rh)
    if dml.globals.compat_dml12_int(site):
        return rh
    if rhtype.is_float:
        rh = promote_float(rh, rhtype)
    elif rhtype.is_int:
        rh, _ = promote_integer(rh, rhtype)
    else:
        raise ICE(site, "Unexpected arith argument to unary +")
    if isinstance(rh, LValue):
        # +x is a rvalue
        rh = mkRValue(rh)
    return rh

class IncDec(UnaryOp):
    # Common superclass for PreIncDec and PostIncDec
    def read(self):
        raise ICE(self, "unimplemented operation")
    def ctype(self):
        t = realtype(self.rh.ctype())
        if t.is_int:
            if t.is_endian:
                return t.access_type
            elif dml.globals.compat_dml12_int(self.site):
                return TInt(t.bits, True)
            else:
                return t
        else:
            assert isinstance(t, TPtr)
            return t
    @classmethod
    def make_simple(cls, site, rh):
        rhtype = safe_realtype(rh.ctype())
        if not isinstance(rhtype, (IntegerType, TPtr)):
            raise EINCTYPE(site, cls.op)
        if not isinstance(rh, LValue):
            if isinstance(rh, BitSlice):
                hint = 'try %s= 1' % (cls.base_op[0],)
            else:
                hint = None
            raise EINC(site, hint)
        result = cls(site, rh)
        if dml.globals.compat_dml12_int(site) or isinstance(rhtype, TPtr):
            return result
        else:
            (result, signed) = promote_integer(result, rhtype)
            return result

    @property
    def op(self):
        return self.base_op

class PreIncDec(IncDec):
    def __str__(self):
        return '%s(%s)' % (self.base_op, self.rh)
    def read(self):
        rh_type = safe_realtype(self.rh.ctype())
        if rh_type.is_int and rh_type.is_endian:
            return '%s(%s, %s, false)' % (
                rh_type.dmllib_fun("prechange"),
                AddressOf(self.rh.site, self.rh).read(),
                self.endian_change)
        else:
            return '%s(%s)' % (self.base_op, self.rh.read())

class PreInc(PreIncDec):
    base_op = '++'
    endian_change = "1"

class PreDec(PreIncDec):
    base_op = '--'
    endian_change = "-1"

class PostIncDec(IncDec):
    priority = 160
    # 'rh' is really 'lh' here...
    def __str__(self):
        return '(%s)%s' % (self.rh, self.base_op)
    def read(self):
        rh_type = safe_realtype(self.rh.ctype())
        if rh_type.is_int and rh_type.is_endian:
            return '%s(%s, %s, true)' % (
                rh_type.dmllib_fun("prechange"),
                AddressOf(self.rh.site, self.rh).read(),
                self.endian_change)
        else:
            return '(%s)%s' % (self.rh.read(), self.base_op)

class PostInc(PostIncDec):
    base_op = '++'
    endian_change = "1"

class PostDec(PostIncDec):
    base_op = '--'
    endian_change = "-1"

mkPreInc = PreInc.make
mkPreDec = PreDec.make
mkPostInc = PostInc.make
mkPostDec = PostDec.make

def read_iface_struct(iface_noderef):
    '''Given the NodeRef of an 'interface' object, return a pointer to the
    interface struct.'''
    if dml.globals.dml_version == (1, 2):
        assert isinstance(iface_noderef, NodeRefWithStorage)
        return '_dev->' + crep.cref_session(iface_noderef.node,
                                            iface_noderef.indices)
    else:
        assert isinstance(iface_noderef, PlainNodeRef)
        struct_name = param_str(iface_noderef.node, '_c_type')
        return mkCast(iface_noderef.site,
                      mkSubRef(iface_noderef.site, iface_noderef, 'val', '.'),
                      TPtr(TNamed(struct_name, const=True))).read()

class MethodPresent(Expression):
    '''Whether a method in an interface object is NULL'''
    type = TBool()
    @auto_init
    def __init__(self, site, expr):
        assert isinstance(expr, InterfaceMethodRef)
    def str(self):
        return '(%s != NULL)' % (self.expr,)
    def read(self):
        return '((%s)->%s != NULL)' % (
            read_iface_struct(self.expr.node_expr),
            self.expr.method_name)
mkMethodPresent = MethodPresent

class InterfaceMethodApply(Expression):
    '''A call to an interface method'''
    priority = 160
    explicit_type = True
    @auto_init
    def __init__(self, site, node_expr, method_name, args, type): pass
    def __str__(self):
        return "%s.%s(%s)" % (
            self.node_expr, self.method_name,
            '(' + ", ".join(str(e) for e in self.args) + ')')
    def read(self):
        return "(%s)->%s(%s)" % (
            read_iface_struct(self.node_expr), self.method_name,
            ", ".join(e.read() for e in self.args))

class InterfaceMethodRef(NonValue):
    '''Reference to an interface method'''
    @auto_init
    def __init__(self, site, node_expr, method_name, obj_arg, ftype): pass

    def __str__(self):
        return "%s.%s" % (self.node_expr, self.method_name)
    def apply(self, args):
        if self.ftype.varargs and len(args) > len(self.ftype.input_types):
            known_arglen = len(self.ftype.input_types)
        else:
            known_arglen = len(args)

        typecheck_inargs(
            self.site, args,
            [(str(i + 1), t)
             for (i, t) in enumerate(self.ftype.input_types[1:])],
            'function', known_arglen)

        args = [coerce_if_eint(arg) for arg in args]

        return InterfaceMethodApply(
            self.site, self.node_expr,
            self.method_name, [self.obj_arg] + args, self.ftype.output_type)
    def exc(self):
        return EIFREF(self.site, self)

def mkInterfaceMethodRef(site, iface_node, indices, method_name):
    struct_name = param_str(iface_node,
                            'c_type' if dml.globals.dml_version == (1, 2)
                            else '_c_type')
    stype = typedefs.get(struct_name)
    if not stype:
        # should never happen: If interface does not exist,
        # then EIFTYPE is signalled and creation of interface
        # node is suppressed
        raise ICE(site, "unknown type %r" % (struct_name,))
    stype = safe_realtype(stype)
    if not isinstance(stype, StructType):
        raise ENOSTRUCT(site, mkNodeRef(site, iface_node, indices), stype)
    ftype = stype.get_member_qualified(method_name)
    if not ftype:
        raise EMEMBER(site, struct_name, method_name)
    ftype = safe_realtype(ftype)
    if isinstance(ftype, TPtr):
        ftype = ftype.base

    if (not isinstance(ftype, TFunction)
        or not ftype.input_types
        or TPtr(safe_realtype(TNamed('conf_object_t'))).cmp(
            safe_realtype(ftype.input_types[0])) != 0):
        # non-method members are not accessible
        raise EMEMBER(site, struct_name, method_name)

    obj_node = iface_node.parent.get_component('obj')
    if not obj_node or not obj_node.objtype == 'session':
        raise ICE(site, 'connect without obj')
    obj_ref = mkNodeRef(site, obj_node, indices)
    return InterfaceMethodRef(
        site, mkNodeRef(site, iface_node, indices), method_name, obj_ref, ftype)

class BitSlice(Expression):
    # msb and lsb are expressed using le bitorder
    slots = ('type', 'read_expr')
    priority = 0
    explicit_type = True
    @auto_init
    def __init__(self, site, expr, msb, lsb, size, mask):
        # lsb is None if i[bitnum] (as opposed to i[msb:lsb]) notation was used
        if size:
            if size.constant:
                self.type = TInt(size.value, False)
            else:
                self.type = TInt(64, False)

        self.read_expr = mkBitAnd(site, mkShR(site, expr, lsb or msb), mask)

    def __str__(self):
        s = str(self.expr)
        if self.expr.priority < self.priority:
            s = '(' + s + ')'
        if self.lsb is None:
            return "%s[%s]" % (s, self.msb)
        else:
            return "%s[%s:%s]" % (s, self.msb, self.lsb)

    def read(self):
        return mkCast(self.site, self.read_expr, self.ctype()).read()

    @property
    def writable(self):
        return self.expr.writable

    def write(self, source):
        source_expr = source
        # if not self.size.constant or source.ctype() > self.type:
        #     source = mkBitAnd(source, self.mask)

        # If the source is a compatible bitslice or bitand, we can
        # simplify things
        if isinstance(source_expr, BitSlice):
            source_expr = source_expr.read_expr

        lsb = self.lsb or self.msb
        source_expr = mkShL(self.site, source_expr, lsb)
        mask = mkShL(self.site, self.mask, lsb)

        expr = mkApply(self.site,
                       mkLit(self.site, 'DML_combine_bits',
                             TFunction([TInt(64, False), TInt(64, False),
                                        TInt(64, False)],
                                       TInt(64, False))),
                       (self.expr, source_expr, mask))
        target_type = realtype(self.expr.ctype())
        if target_type.is_int and target_type.is_endian:
            expr = mkCast(self.site, expr, target_type)
        return self.expr.write(expr)

def mkBitSlice(site, expr, msb, lsb, bitorder):
    # lsb == None means that only one bit number was given (expr[i]
    # rather than expr[i:j]). This in turn means that lsb==msb, and
    # that the expression may be used as a bool even if the bit number
    # is not a constant.

    t = realtype(expr.ctype())
    if not t.is_int:
        report(EBSLICE(site))
        return expr

    if not bitorder:
        bitorder = site.bitorder()

    if bitorder != 'le':
        if not expr.explicit_type:
            #dbg('BITSLICE %r : %r' % (expr, t))
            raise EBSBE(site)

        # Normalize to le bitorder
        expr_bits = mkIntegerLiteral(site, t.bits)
        msb = endian_convert_expr(site, msb, bitorder, expr_bits)
        if lsb is not None:
            lsb = endian_convert_expr(site, lsb, bitorder, expr_bits)

    if lsb is None:
        size = mkIntegerLiteral(site, 1)
    else:
        size = mkAdd(site,
                     mkSubtract(site, msb, lsb),
                     mkIntegerLiteral(site, 1))

    if size.constant:
        if size.value <= 0 or size.value > 64:
            raise EBSSIZE(site, size)
        mask = mkIntegerConstant(site, (1 << size.value) - 1, False)
    else:
        mask = mkSubtract(site,
                          ShL.make(site, mkIntegerLiteral(site, 1), size),
                          mkIntegerLiteral(site, 1))

    return BitSlice(site, expr, msb, lsb, size, mask)

class TraitMethodApplyIndirect(Expression):
    '''The C expression of a trait method call'''
    @auto_init
    def __init__(self, site, traitref, methname, inargs, type): pass

    def __str__(self):
        return '%s.%s(%s)' % (self.traitref, self.methname,
                              ', '.join(map(str, self.inargs)))

    def read(self):
        if self.inargs:
            return "CALL_TRAIT_METHOD(%s, %s, _dev, %s)" % (
                cident(realtype(self.traitref.ctype()).trait.name),
                self.methname,
                ', '.join([arg.read()
                           for arg in [self.traitref] + self.inargs]))
        else:
            return "CALL_TRAIT_METHOD0(%s, %s, _dev, %s)" % (
                cident(realtype(self.traitref.ctype()).trait.name),
                self.methname, self.traitref.read())

class TraitMethodApplyDirect(Expression):
    '''The C expression of a trait method call'''
    @auto_init
    def __init__(self, site, traitref, methodref, inargs, type):
        # traitref is a reference to method's vtable trait
        assert realtype(traitref.ctype()).trait == methodref.vtable_trait
        assert methodref.__class__.__name__ == 'TraitMethod'

    def __str__(self):
        return '%s(%s)' % (self.methodref, ', '.join(map(str, self.inargs)))

    def read(self):
        return "%s(_dev, %s)" % (
            self.methodref.cname(),
            ', '.join([arg.read()
                       for arg in [self.traitref] + self.inargs]))

class New(Expression):
    priority = 160 # f()
    slots = ('type',)
    @auto_init
    def __init__(self, site, newtype, count):
        self.type = TPtr(newtype)
    def __str__(self):
        if self.count:
            return 'new %s[%s]' % (self.newtype, self.count)
        else:
            return 'new %s' % self.newtype
    def read(self):
        t = self.newtype.declaration('')
        if self.count:
            return 'MM_ZALLOC(%s, %s)' % (self.count.read(), t)
        else:
            return 'MM_ZALLOC(1, %s)' % (t)

def mkNew(site, newtype, count = None):
    if count:
        count = as_int(count)
    return New(site, newtype, count)

class ListItems(metaclass=abc.ABCMeta):
    '''A series of consecutive list elements, where each list element
    corresponds to one index in a multi-dimensional (but possibly
    zero-dimensional) object array.

    For example, if the dimsizes property is (3, 2), then the list
    elements are yielded by calling expr() six times, with index
    expressions corresponding to (0,0),(0,1),(1,0),(1,1),(2,0) and
    (2,1).
    '''
    __slots__ = ()
    @abc.abstractproperty
    def dimsizes(self): pass
    @abc.abstractmethod
    def expr(self, indices):
        '''Return expression for a specific set of indices. indices is a tuple
        of integer expressions, with the same length as self.dimsizes.'''

class ObjectArray(ListItems):
    __slots__ = ('site', 'node', 'indices', 'dimsizes')
    def __init__(self, site, node, indices, dimsizes):
        self.site = site
        self.node = node
        self.indices = indices
        self.dimsizes = dimsizes
    def expr(self, indices):
        return mkNodeRef(self.site, self.node, self.indices + indices)

class ListItem(ListItems):
    __slots__ = ('_expr',)
    def __init__(self, expr):
        self._expr = expr
    dimsizes = ()
    def expr(self, indices):
        return self._expr

class AbstractList(NonValue):
    def __str__(self):
        return "[%s]" % (", ".join(str(r) for r in self.iter_flat()),)
    @property
    def value(self):
        return list(self.iter_flat())
    @abc.abstractmethod
    def iter(self):
        """Return the list's values, as an iterator over ListItems objects."""

    def iter_flat(self):
        """Return a generator that produces the list items as expressions."""
        for item in self.iter():
            for indices in itertools.product(
                    *(list(range(sz)) for sz in item.dimsizes)):
                yield item.expr(
                    tuple(mkIntegerLiteral(self.site, idx)
                          for idx in indices))

class List(AbstractList):
    slots = ('constant',)
    @auto_init
    def __init__(self, site, value):
        self.constant = all(e.constant for e in value)
    def __str__(self):
        return "[%s]" % (",".join([str(x) for x in self.value]))
    def iter(self):
        return (ListItem(expr) for expr in self.value)

def mkList(site, value):
    # dbg('mkListConstant(%r)' % value)
    for v in value:
        assert v is None or isinstance(v, Expression)
    return List(site, value)

class ObjectList(AbstractList):
    @auto_init
    def __init__(self, site, instances): pass
        # 'instances' is a list of triples (node, indices, dimsizes).
        # 'indices' are the
        # outermost array indices, and dimsizes are the dimensions of
        # the innermost indices. If dimsizes is empty, this triple
        # represents one single object instance; if dimsizes is
        # non-empty, it represents a list of all possible indices in
        # those dimensions.
    def iter(self):
        for (node, indices, dimsizes) in self.instances:
            if node.dimensions != len(indices) + len(dimsizes):
                raise ICE(node.site,
                          'bad instance list: %s %s' % (indices, dimsizes))
            yield ObjectArray(self.site, node, indices, dimsizes)

mkObjectList = ObjectList

class EachIn(Expression):
    # TODO: One EachIn may be a subset of another, in two ways:
    #   1. (each register in $bank) vs (each register in $bank.group)
    #   2. (each subtrait in $obj) vs (each supertrait in $obj)
    # We don't cover (1), and cover (2) only partially.

    # Map (trait, node) pair to list of subobjs implementing the trait
    instances = {}

    def __init__(self, site, trait, node, indices):
        Expression.__init__(self, site)
        self.trait = trait
        self.node = node
        assert isinstance(indices, tuple)
        self.indices = indices

    def __str__(self):
        return 'each %s in %s' % (
            self.trait.name, self.node.logname(self.indices))

    def ctype(self):
        return TTraitList(self.trait.name)

    @staticmethod
    def ident(node, trait):
        '''C identifier name for vtable_list_t instance'''
        return '_each__%s__in__%s' % (trait.name,
                                      node.attrname() or node.name)

    @staticmethod
    def subobjs_implementing(node, trait):
        '''Find all objects implementing a trait, recursively descending only
        into subobjects of objects that do not implement the trait'''
        for sub in node.get_components():
            if isinstance(sub, objects.CompositeObject):
                if dml.globals.dml_version == (1, 2) and not node.name:
                    # avoid trouble from implicit fields
                    pass
                elif trait in sub.traits.ancestors:
                    yield sub
                else:
                    for match in EachIn.subobjs_implementing(sub, trait):
                        yield match

    def mark_referenced(self, subobjs):
        key = (self.node, self.trait)
        if key not in self.instances:
            self.instances[key] = subobjs
            for sub in subobjs:
                sub.traits.mark_referenced(self.trait)

    def read(self):
        subobjs = list(self.subobjs_implementing(self.node, self.trait))
        if not subobjs:
            # Iteration will never start, so NULL will suffice
            return '(_each_in_t){NULL, 0,  0, 0}'
        self.mark_referenced(subobjs)
        if self.indices:
            dimsizes = self.node.dimsizes
            array_size = reduce(operator.mul, dimsizes)
            array_idx = encode_indices(self.indices, dimsizes)
        else:
            array_size = 1
            array_idx = "0"
        return '(_each_in_t){%s, 0, %d, %s, %d}' % (
            self.ident(self.node, self.trait),
            len(subobjs), array_idx, array_size)

mkEachIn = EachIn

class SequenceLength(Expression):
    '''The length of a sequence'''
    type = TInt(64, False)
    @auto_init
    def __init__(self, site, expr):
        assert isinstance(expr, EachIn)

    def __str__(self):
        return "%s.len" % self.expr
    def read(self):
        return "_count_eachin(%s)" % self.expr.read()

mkSequenceLength = SequenceLength

class Constant(Expression):
    constant = True
    priority = 1000
    @auto_init
    def __init__(self, site, value): pass

class IntegerConstant(Constant):
    '''Base class for all constant 64-bit signed or unsigned integer
    values'''

    def __init__(self, site, value, type):
        assert type.is_int
        self.check_type(type)
        assert isinstance(value, int)
        if type.signed:
            limit = 1 << (type.bits - 1)
            assert -limit <= value < limit
        else:
            assert 0 <= value < 1 << type.bits
        Constant.__init__(self, site, int(value))
        self.type = type

    @staticmethod
    def check_type(type):
        assert type.bits == 64

    @property
    def signed(self):
        return self.type.signed

    def __str__(self):
        return str(self.value)

    def read(self):
        value = self.value
        if value == -(1 << 63):
            # C parses -128 as unary minus on 128; this works as you would
            # expect on all values expect LLONG_MIN so we need a special case
            return '(-0x%xLL - 1)' % ((1 << 63) - 1,)
        s = str(value) if value < 256 else '%#x' % value
        if self.type.signed:
            return s + 'LL'
        else:
            return s + 'ULL'

def mkIntegerConstant(site, value, signed):
    '''A constant integer value in DML 1.4'''
    if dml.globals.compat_dml12_int(site):
        return IntegerConstant_dml12(site, value)
    return IntegerConstant(site, truncate_int_bits(value, signed),
                           TInt(64, signed))

def mkIntegerLiteral(site, value):
    '''Convenience for a nonnegative integer constant with natural sign'''
    assert 0 <= value < 1 << 64
    if dml.globals.compat_dml12_int(site):
        return IntegerConstant_dml12(site, value)
    return mkIntegerConstant(site, value, value < 1 << 63)

class TypedIntegerConstant(IntegerConstant):
    '''Integer constant with explicit integer type'''
    priority = 140 # Cast.priority

    @staticmethod
    def check_type(type):
        assert 0 < type.bits <= 64

    def read(self):
        return '(%s)%s' % (self.type.declaration(''),
                           super(TypedIntegerConstant, self).read())

class IntegerConstant_dml12(IntegerConstant):
    slots = ('explicit_type',)
    def __init__(self, site, value, type = None):
        self.explicit_type = bool(type)
        if type is None:
            type = self._detect_ctype(value)
        if type is None:
            raise ICE(site, "too large constant: " + repr(value))
        IntegerConstant.__init__(self, site, int(value), type)

    @staticmethod
    def check_type(type):
        assert 0 < type.bits

    def read(self):
        value = self.value
        type = self.ctype()

        if value < 0:
            sign = '-'
            sign_suffix = ''
            value = -value
        else:
            sign = ''
            sign_suffix = 'U'

        if value < 256:
            s = str(self.value)
        else:
            s = sign + '%#x' % value

        if type.bits > 32:
            s += sign_suffix + "LL"
        elif value > 0x7fffffff:
            s += sign_suffix
        return s

    @staticmethod
    def _detect_ctype(value):
        if value < 0:
            if value >= -128:
                return TInt(8, True)
            elif value >= -(2**15):
                return TInt(16, True)
            elif value >= -(2**31):
                return TInt(32, True)
            elif value >= -(2**63):
                return TInt(64, True)
        else:
            if value < 2:
                return TInt(1, False)
            if value < 256:
                return TInt(8, False)
            elif value < (2**16):
                return TInt(16, False)
            elif value < (2**32):
                return TInt(32, False)
            elif value < (2**64):
                return TInt(64, False)
            elif value < (2**128):
                return TInt(128, False)
        return None

def all_index_exprs(node):
    return itertools.product(
        *([mkIntegerLiteral(node.site, i) for i in range(dimsize)]
          for dimsize in node.dimsizes))

class FloatConstant(Constant):
    type = TFloat('double')
    def __init__(self, site, value):
        assert_type(site, value, float)
        Constant.__init__(self, site, value)
    def __str__(self):
        return repr(self.value)
    def read(self):
        if math.isnan(self.value):
            return '(double)NAN'
        elif math.isinf(self.value):
            return ('(double)-INFINITY' if self.value < 0
                    else '(double)INFINITY')
        else:
            return float.hex(self.value)
    def copy(self, site):
        return FloatConstant(site, self.value)

mkFloatConstant = FloatConstant

def char_escape(m):
    c = m.group(0)
    if c == b'"' or c == b'\\':
        return b'\\' + c
    return b"\\%03o" % ord(c)

char_escape_re = re.compile(br'[\x00-\x1f"\\\x7f-\xff]')

def string_escape(s):
    return char_escape_re.sub(char_escape, s).decode('utf-8')

class StringConstant(Constant):
    type = TPtr(TNamed('char', const=True))
    def __init__(self, site, value):
        # Store the value in UTF-8 (to permit both unicode and byte strings)
        if isinstance(value, str):
            value = value.encode('utf-8')
        Constant.__init__(self, site, value)
    @property
    def quoted(self):
        return '"' + string_escape(self.value) + '"'
    def __str__(self):
        return self.quoted
    def read(self):
        return self.quoted
    def unicode_value(self):
        # self.value is in UTF-8; this method gives it in unicode.
        return self.value.decode('utf-8')

mkStringConstant = StringConstant

class BoolConstant(Constant):
    type = TBool()
    def __str__(self):
        if self.value:
            return 'true'
        else:
            return 'false'
    def read(self):
        if self.value:
            return '1'
        else:
            return '0'
    def copy(self, site):
        return BoolConstant(site, self.value)

mkBoolConstant = BoolConstant

class Undefined(NonValue):
    undefined = True
    def __str__(self):
        return 'undefined'
    def exc(self):
        return EUNDEF(self)

mkUndefined = Undefined

def endian_convert_expr(site, idx, endian, size):
    """Convert a bit index to little-endian (lsb=0) numbering.

    The numbers given and returned are ctree expressions."""

    if endian == 'le':
        return idx
    elif endian == 'be':
        return mkSubtract(site,
                          mkSubtract(site, size, mkIntegerLiteral(None, 1)),
                          idx)
    else:
        raise ICE(site, 'unknown bitorder: %r' % (endian,))

class ObjTraitRef(Expression):
    '''Reference to a specific trait of a specific object'''
    def __init__(self, site, node, trait, indices,
                 ancestry_path=None):
        Expression.__init__(self, site)
        if not isinstance(indices, tuple):
            raise ICE(site, 'bad indices: %r' % (indices,))
        assert len(indices) == node.dimensions
        self.node = node
        self.trait = trait
        self.indices = indices
        # All references are expressed as an upcast of an
        # ObjTraits. In case of multiple inheritance, there can be
        # more than one way to upcast; ancestry_path shows the upcast
        # path to use.
        self.ancestry_path = (
            self.node.traits.ancestry_paths[trait][0]
            if ancestry_path is None else ancestry_path)

    def __str__(self):
        return "%s.%s" % (self.node.logname(self.indices), self.trait.name)

    def ctype(self):
        return TTrait(self.trait)

    def read(self):
        self.node.traits.mark_referenced(self.trait)
        if all(idx.constant for idx in self.indices):
            indices_decl = None
            indices = self.indices
        else:
            # Create intermediate array to avoid evaluating indices multiple
            # times
            indices_decl = ('uint32 __indices[] = {%s}'
                            % (', '.join(i.read() for i in self.indices)))
            indices = tuple(mkLit(self.site, '__indices[%d]' % (i,),
                                  TInt(32, False))
                            for i in range(self.node.dimensions))
        structref = (self.node.traits.vtable_cname(self.ancestry_path[0])
                     + ''.join('[%s]' % (i.read(),) for i in indices))
        pointer = '(&%s)' % ('.'.join([structref] + [
            cident(t.name) for t in self.ancestry_path[1:]]))
        id = ObjIdentity(self.site, self.node, indices).read()
        traitref_expr = ('((%s) {%s, %s})'
                         % (cident(self.trait.name), pointer, id))
        if indices_decl:
            return '({%s; %s;})' % (indices_decl, traitref_expr)
        else:
            return traitref_expr


def try_convert_identity(expr, convert_value_noderefs = True):
    if (isinstance(expr, NodeRef)
        and isinstance(expr.node, objects.CompositeObject)
        and (convert_value_noderefs or isinstance(expr, NonValue))):
        return ObjIdentity(expr.site, expr.node, expr.indices)
    elif not isinstance(expr, NonValue):
        typ = safe_realtype(expr.ctype())
        if isinstance(typ, TObjIdentity):
            return expr
        elif isinstance(typ, TTrait):
            return TraitObjIdentity(expr.site, expr)
    return None

class ObjIdentity(Expression):
    '''A unique identifier for a specific compound object'''
    slots = ('constant', 'constant_indices', 'value')
    def __init__(self, site, node, indices):
        Expression.__init__(self, site)
        if not isinstance(indices, tuple):
            raise ICE(site, 'bad indices: %r' % (indices,))
        assert len(indices) == node.dimensions
        self.node = node
        self.indices = indices
        if all(idx.constant for idx in indices):
            self.constant = True
            self.constant_indices = tuple(idx.value for idx in indices)
            self.value = (node, self.constant_indices)
        else:
            self.constant = False
            self.constant_indices = None
            self.value = None

    def __str__(self):
        return "%s" % (self.node.logname(self.indices),)

    def ctype(self):
        return TObjIdentity()

    def read(self):
        if self.constant:
            encoded_index = encode_indices_constant(self.constant_indices,
                                                    self.node.dimsizes)
        else:
            encoded_index = encode_indices(self.indices, self.node.dimsizes)
        return ('((_identity_t) {.id = %d, .encoded_index = %s})'
                % (self.node.uniq, encoded_index))

def encode_indices_constant(indices, dimsizes):
    return reduce(lambda x, y: x*y[0] + y[1], zip(dimsizes, indices), 0)

def encode_indices(indices, dimsizes):
    return reduce(lambda x, y: ('(%s) * %d + (%s)' % (x, y[0], y[1].read())),
                  zip(dimsizes[1:], indices[1:]),
                  indices[0].read())

class TraitObjIdentity(Expression):
    @auto_init
    def __init__(self, site, traitref): pass

    def __str__(self):
        return "%s" % (self.traitref,)

    def ctype(self):
        return TObjIdentity()

    def read(self):
        return "(%s).id" % (self.traitref.read(),)

class TraitUpcast(Expression):
    @auto_init
    def __init__(self, site, sub, parent): pass

    def __str__(self):
        return "cast(%s, %s)" % (self.sub, self.parent.name)

    def ctype(self):
        return TTrait(self.parent)

    def read(self):
        typ = safe_realtype(self.sub.ctype())
        assert isinstance(typ, TTrait)
        if self.parent not in typ.trait.ancestors:
            raise ICE(self.site, 'cannot upcast %s to %s'
                      % (typ.trait.name, self.parent.name))

        return ("UPCAST(%s, %s, %s)"
                % (self.sub.read(), cident(typ.trait.name),
                   ".".join(cident(t.name) for t in
                            typ.trait.ancestry_paths[self.parent][0])))

def mkTraitUpcast(site, sub, parent):
    typ = safe_realtype(sub.ctype())
    if isinstance(typ, TTrait):
        if typ.trait is parent:
            return sub
        elif parent in typ.trait.ancestors:
            return TraitUpcast(site, sub, parent)
    raise ETEMPLATEUPCAST(site, typ, parent.type())

def vtable_read(expr):
    typ = realtype(expr.ctype())
    assert isinstance(typ, TTrait)
    return '((struct _%s *) (%s).trait)' % (cident(typ.trait.name),
                                            expr.read())

class TraitParameter(Expression):
    priority = 160 # SubRef.priority
    @auto_init
    def __init__(self, site, traitref, name, type): pass

    def __str__(self):
        return "%s.%s" % (self.traitref, self.name)

    def read(self):
        return ("%s->%s"
                % (vtable_read(self.traitref), self.name))

class TraitSessionRef(Expression):
    '''A reference to a trait session variable.

    Hack: this actually represents the address of the variable, so an
    actual reference must be wrapped in a Dereference. The Dereference
    indirection is there as an easy way to make session references lvalues.'''
    priority = 140 # Cast.priority
    @auto_init
    def __init__(self, site, traitref, name, type_): pass

    def __str__(self):
        return "&%s.%s" % (self.traitref, self.name)

    def ctype(self):
        return TPtr(self.type_)

    def read(self):
        return '(%s)((uintptr_t)_dev + %s->%s)' % (
            TPtr(self.type_).declaration(''), vtable_read(self.traitref),
            self.name)

class TraitMethodRef(NonValue):
    '''A reference to a bound method in a trait. The expression
    cannot currently be referenced standalone; it needs to be
    called.'''

    # We could have calculated rettype locally instead of using an
    # arg, but that requires c_rettype() which is not visible here
    @abc.abstractmethod
    def call_expr(self, inargs, rettype):
        '''Return a C expression to invoke the method. The caller is
        responsible for doing type-checking. inargs denote C
        representations of explicit DML method args (possibly both
        input and output), and rettype is the C function's return type.'''

    @abc.abstractproperty
    def inp(self): pass
    @abc.abstractproperty
    def outp(self): pass
    @abc.abstractproperty
    def throws(self): pass

    def apply(self, args):
        '''Return expression for application as a function'''
        if self.throws or len(self.outp) > 1:
            raise EAPPLY(self, 'method')
        # Run typecheck before coercing endian integers, slightly better
        # error messages
        typecheck_inargs(self.site, args, self.inp, 'method')
        args = [coerce_if_eint(arg) for arg in args]
        if self.outp:
            [(_, rettype)] = self.outp
        else:
            rettype = TVoid()
        return self.call_expr(args, rettype)

class TraitMethodDirect(TraitMethodRef):
    '''The DML expression 'default', evaluated inside a trait method. In
    other words, a direct reference to a specific trait method
    instance, as opposed to an indirect vtable lookup.'''
    @auto_init
    def __init__(self, site, traitref, methodref): pass

    def __str__(self):
        return "%s.%s" % (str(realtype(self.traitref.ctype()).trait.name),
                          self.methodref.name)

    @property
    def inp(self):
        return self.methodref.inp

    @property
    def outp(self):
        return self.methodref.outp

    @property
    def throws(self):
        return self.methodref.throws

    def call_expr(self, inargs, rettype):
        return TraitMethodApplyDirect(self.site, self.traitref,
                                      self.methodref, inargs, rettype)

class TraitMethodIndirect(TraitMethodRef):
    '''An indirect reference to a trait method,
    meaning that the reference is looked up in runtime via a
    vtable. The expression cannot currently be referenced standalone;
    it needs to be called.'''
    @auto_init
    def __init__(self, site, traitref, methname, inp, outp, throws): pass

    def __str__(self):
        return "%s.%s" % (str(self.traitref), self.methname)

    def call_expr(self, inargs, rettype):
        return TraitMethodApplyIndirect(self.site, self.traitref,
                                        self.methname, inargs, rettype)

def lookup_component(site, base, indices, name, only_local):
    '''Lookup a name in object scope 'base' and return an
    Expression, or None.'''
    assert isinstance(base, objects.DMLObject)
    assert isinstance(indices, tuple)

    if name == 'this':
        if isinstance(base, objects.Method):
            base = base.parent
        return mkNodeRef(site, base, indices)

    node = base.get_component(name)
    if node:
        if node.objtype == 'parameter':
            return node.get_expr(indices).copy(site)
        if node.isindexed() and len(indices) < node.dimensions:
            return NodeArrayRef(site, node, indices)
        else:
            return mkNodeRef(site, node, indices)

    if base.traits:
        # A shared method implementation does not end up as an object
        # in the object hierarchy, so need to look for it explicitly
        shared_method = base.traits.lookup_shared_method_impl(
            site, name, indices)
        if shared_method is not None:
            return shared_method

    # Search upwards in outer scopes if it wasn't found
    if not only_local and base.parent:
        if base.isindexed():
            indices = indices[:-base.local_dimensions()]
        return lookup_component(site, base.parent, indices, name, False)

    if dml.globals.compat_dml12 and not base.parent:
        # Last resort is to look for components in anonymous banks
        for bank in base.get_components('bank'):
            if not bank.name:
                node = bank.get_component(name)
                if node:
                    if node.objtype == 'parameter':
                        return node.get_expr(indices).copy(site)
                    if node.isindexed() and len(indices) < node.dimensions:
                        return NodeArrayRef(site, node, indices)
                    else:
                        return mkNodeRef(site, node, indices)

    return None


# Return the node type for a node
def node_type(node, site):
    if node.objtype == 'register' and node.wholefield:
        node = node.wholefield
    return crep.node_storage_type(node, site)

class NodeRef(Expression):
    "A reference to a node in the device specification"
    priority = 1000
    explicit_type = True
    @auto_init
    def __init__(self, site, node, indices):
        assert isinstance(node, objects.DMLObject)
        assert isinstance(indices, tuple)
    def __str__(self):
        name = self.node.logname(self.indices)
        if name:
            return dollar(self.site)+name
        else:
            assert dml.globals.dml_version == (1, 2)
            return '$<anonymous %s>' % self.node.objtype
    def get_ref(self):
        return (self.node, self.indices)
    def apply(self, args):
        '''Apply as an expression'''
        if (self.node.objtype == 'method' and not self.node.throws
            and len(self.node.outp) < 2):
            return codegen_call_expr(self.site, self.node, self.indices, args)
        raise EAPPLY(self, self.node.objtype + " object")

class NodeRefWithStorage(NodeRef, LValue):
    '''Reference to node that also contains storage, such as allocated
    register, field or attribute in DML 1.2'''
    slots = ('type',)

    @auto_init
    def __init__(self, site, node, indices):
        NodeRef.__init__(self, site, node, indices)
        assert dml.globals.dml_version == (1, 2)
        self.type = node_type(node, site)
        assert all(not isinstance(i, NonValue) for i in self.indices)

    def read(self):
        if (logging.show_porting
            and self.node.objtype in ['field', 'register',
                                      'connect', 'attribute',
                                      'interface']
            and not self.site.filename().endswith('dml-builtins.dml')
            # some DML-generated expressions, like 'parent', use
            # object's site
            and self.site is not self.node.site):
            report(PVAL(dmlparse.end_site(self.site)))
        node = self.node

        if node.objtype == 'method':
            # enforced by crep.node_storage_type
            assert dml.globals.dml_version == (1, 2)
            from . import codegen
            method = codegen.method_instance(node)
            codegen.mark_method_referenced(method)
            expr = '_DML_M_' + crep.cref(node)
        elif node.objtype == 'device':
            assert dml.globals.dml_version == (1, 2)
            return '_dev'
        else:
            expr = '_dev->' + crep.cref_session(node, self.indices)
        return expr

    def apply(self, args):
        if self.node.objtype == 'method':
            assert dml.globals.dml_version == (1, 2)
            raise EAPPLY(self, "method")
        # storage might be a function pointer
        return mkApply(self.site, self, args)

class SessionVariableRef(LValue):
    "A reference to a session variable"
    slots = ('type',)
    priority = 1000
    explicit_type = True
    @auto_init
    def __init__(self, site, node, indices):
        assert isinstance(node, objects.DMLObject)
        assert isinstance(indices, tuple)
        assert node.objtype in ('session', 'saved')
        self.type = node._type
    def __str__(self):
        name = self.node.logname(self.indices)
        assert name
        return name
    def read(self):
        return '_dev->' + crep.cref_session(self.node, self.indices)

class PlainNodeRef(NodeRef, NonValue):
    pass

# Two variations of PlainNodeRef, specific to DML 1.2, which permit
# typeof() and which give more helpful error messages
class NoallocNodeRef(PlainNodeRef):
    @auto_init
    def __init__(self, site, node, indices, node_type): pass
    def exc(self):
        return ENALLOC(self.site, self.node)

class RegisterWithFields(PlainNodeRef):
    @auto_init
    def __init__(self, site, node, indices, node_type): pass
    def exc(self):
        return EREGVAL(self.site, self.node)

# This one can happen in both 1.2 and 1.4, for data members.
class IncompleteNodeRefWithStorage(PlainNodeRef):
    '''NodeRefWithStorage where not all indices are defined'''
    @auto_init
    def __init__(self, site, node, indices, node_type, static_index):
        assert isinstance(static_index, NonValue)
    def exc(self):
        return self.static_index.copy(self.site).exc()

def mkNodeRef(site, node, indices):
    if node.dimensions != len(indices):
        raise ICE(site, 'Reference to node with wrong number of indices: %r'
                  % (node,))
    # We can pretend 'saved' references are actually 'session' references,
    # as the access method is the same
    if node.objtype == 'session' or node.objtype == 'saved':
        for i in indices:
            if isinstance(i, NonValue):
                raise i.copy(site).exc()
        return SessionVariableRef(site, node, indices)
    t = node_type(node, site)
    if not t:
        return PlainNodeRef(site, node, indices)
    if dml.globals.dml_version == (1, 2):
        # Registers and fields have a type, but sometimes not a value
        if node.objtype == "register" and not node.wholefield:
            return RegisterWithFields(site, node, indices, t)
        if (node.objtype in ('register', 'field')
            and not param_bool(node, 'allocate')):
            return NoallocNodeRef(site, node, indices, t)
        if node.objtype == 'method':
            from . import codegen
            method = codegen.method_instance(node)
            if not method.throws:
                # We forbid references to non-throwing methods for two
                # reasons:
                # 1. The type of a method node is the type of the
                #    generated C function, for historical reasons. For
                #    some non-throwing methods, the method node is
                #    itself a callable expression, but in that call
                #    expression the $dev argument is
                #    implicit. Allowing a reference to a non-throwing
                #    method as an expression might cause confusion
                #    around the type.
                # 2. We may want to support true method references in
                #    the future, where the reference would include
                #    index parameters. Forbidding non-call references
                #    to non-throwing methods will allow us to support
                #    this without breaking compatibility.
                return PlainNodeRef(site, node, indices)
    for i in indices:
        if isinstance(i, StaticIndex):
            return IncompleteNodeRefWithStorage(site, node, indices, t, i)
    return NodeRefWithStorage(site, node, indices)

class NodeArrayRef(NonValue):
    '''Reference to an array node before it's indexed. Indexing is the
    only supported operation.'''
    @auto_init
    def __init__(self, site, node, indices):
        if not node.isindexed():
            raise ICE(site, 'incomplete non-array: %r' % (node,))
        if len(indices) >= node.dimensions:
            raise ICE(site,
                      "Too many indices in reference to incomplete node: %r"
                      % (node,))
        if len(indices) < node.nonlocal_dimensions():
            raise ICE(site,
                      "Too few indices in reference to incomplete node: %r"
                      % (node,))
    def __str__(self):
        name = self.node.logname(self.indices)
        return dollar(self.site) + name
    def exc(self):
        return EARRAY(self.site, self.node)

class Variable(LValue):
    "Variable storage"
    priority = 1000
    explicit_type = True
    @auto_init
    def __init__(self, site, sym):
        sym.incref()
    def ctype(self):
        return self.sym.type
    def __str__(self):
        return self.sym.name
    def read(self):
        if self.sym.value:
            s = self.sym.value
        else:
            s = self.sym.name
        return s
    def incref(self):
        self.sym.incref()
    def decref(self):
        self.sym.decref()

class LocalVariable(Variable):
    "Local variable storage"

mkLocalVariable = LocalVariable

class StaticVariable(Variable):
    "Static variable storage"
    @auto_init
    def __init__(self, site, sym):
        assert_type(site, sym, symtab.StaticSymbol)
    def read(self):
        return '_dev->'+self.sym.value

mkStaticVariable = StaticVariable

class StructMember(LValue):
    priority = 160
    explicit_type = True
    @auto_init
    def __init__(self, site, expr, sub, type, op):
        assert_type(site, expr, Expression)
        assert_type(site, sub, str)

    def __str__(self):
        s = str(self.expr)
        if self.expr.priority < self.priority:
            s = '(' + s + ')'
        return s + self.op + self.sub
    def read(self):
        s = self.expr.read()
        if self.expr.priority < self.priority:
            s = '(' + s + ')'
        return s + self.op + self.sub

def mkSubRef(site, expr, sub, op):
    if isinstance(expr, NodeRef):
        node, indices = expr.get_ref()
        # Only allow dot for node lookups
        if op == '.' and isinstance(node, objects.CompositeObject):
            e = lookup_component(site, node, indices, sub, True)
            if e:
                return e

        (node, indices) = expr.get_ref()
        if node.objtype == 'interface':
            if op == "->":
                raise ENOPTR(site, expr)
            return mkInterfaceMethodRef(site, node, indices, sub)
        else:
            raise EREF(site, sub, expr)

    elif isinstance(expr, AddressOf) and op == '->':
        expr = expr.rh
        op = '.'
    elif isinstance(expr, Dereference) and op == '.':
        expr = expr.rh
        op = '->'

    etype = expr.ctype()
    etype = safe_realtype(etype)

    if isinstance(etype, TPtr):
        if op == '.':
            raise ENOSTRUCT(site, expr)
        basetype = realtype(etype.base)
        baseexpr = mkDereference(site, expr)
    else:
        if op == '->':
            raise ENOPTR(site, expr)
        basetype = etype
        baseexpr = expr

    basetype = basetype.resolve()

    if isinstance(basetype, StructType):
        typ = basetype.get_member_qualified(sub)
        if not typ:
            raise EMEMBER(site, baseexpr, sub)
        return StructMember(site, expr, sub, typ, op)
    elif basetype.is_int and basetype.is_bitfields:
        member = basetype.members.get(sub)
        if member is None:
            raise EMEMBER(site, expr, sub)
        (_, msb, lsb) = member
        return mkBitSlice(site,
                          baseexpr,
                          mkIntegerLiteral(site, msb),
                          mkIntegerLiteral(site, lsb),
                          'le')
    elif isinstance(basetype, TTrait):
        m = basetype.trait.lookup(sub, baseexpr, site)
        if not m:
            raise EMEMBER(site, expr, sub)
        return m
    elif isinstance(basetype, TArray) and sub == 'len':
        if basetype.size.constant:
            return mkIntegerConstant(site, basetype.size.value, False)
        else:
            raise EVLALEN(site)
    else:
        raise ENOSTRUCT(site, expr)

class ArrayRef(LValue):
    slots = ('type',)
    priority = 160
    explicit_type = True
    @auto_init
    def __init__(self, site, expr, idx):
        self.type = realtype_shallow(expr.ctype()).base
    def __str__(self):
        return '%s[%s]' % (self.expr, self.idx)
    def read(self):
        expr = self.expr.read()
        if self.expr.priority < self.priority:
            expr = '(%s)' % (expr,)
        return '%s[%s]' % (expr, self.idx.read())

class VectorRef(LValue):
    slots = ('type',)
    @auto_init
    def __init__(self, site, expr, idx):
        self.type = realtype(self.expr.ctype()).base
    def read(self):
        return 'VGET(%s, %s)' % (self.expr.read(), self.idx.read())
    def write(self, source):
        return "VSET(%s, %s, %s)" % (self.expr.read(), self.idx.read(),
                                     source.read())

def mkIndex(site, expr, idx):
    if isinstance(idx, NonValue):
        if isinstance(idx, StaticIndex) and isinstance(expr, NodeArrayRef):
            # handled by NodeArrayRef case below
            pass
        else:
            raise idx.exc()
    else:
        idx = as_int(idx)
    if isinstance(expr, NonValue):
        if isinstance(expr, NodeArrayRef):
            node = expr.node
            indices = expr.indices
            if not isinstance(idx, StaticIndex):
                if idx.constant:
                    if (idx.value < 0 or
                        idx.value >= node.arraylens()[
                            len(indices) - node.nonlocal_dimensions()]):
                        raise EOOB(idx)
            indices += (idx,)
            if node.dimensions > len(indices):
                return NodeArrayRef(site, node, indices)
            else:
                return mkNodeRef(site, node, indices)
        if isinstance(expr, AbstractList):
            if idx.constant:
                if idx.value < 0 or idx.value >= len(expr.value):
                    raise EOOB(expr)

                assert isinstance(expr.value[idx.value], Expression)
                return expr.value[idx.value]

            raise EAVAR(idx.site)
        raise expr.exc()

    typ = safe_realtype(expr.ctype())

    if typ.is_int:
        return mkBitSlice(site, expr, idx, None, None)

    if isinstance(typ, (TArray, TPtr)):
        return ArrayRef(site, expr, idx)

    if isinstance(typ, (TVector)):
        return VectorRef(site, expr, idx)

    raise ENARRAY(expr)

class Cast(Expression):
    "A C type cast"
    priority = 140
    explicit_type = True
    @auto_init
    def __init__(self, site, expr, type): pass
    def __str__(self):
        return 'cast(%s, %s)' % (self.expr, self.type.declaration(""))
    def read(self):
        s = self.expr.read()
        if self.expr.priority <= self.priority:
            s = '(' + s + ')'
        return '(%s)%s' % (self.type.declaration(''), s)

def mkCast(site, expr, new_type):
    real = safe_realtype(new_type)
    if isinstance(real, TTrait):
        if isinstance(expr, NodeRef):
            (node, indices) = expr.get_ref()
            if real.trait in node.traits.ancestors:
                return ObjTraitRef(site, node, real.trait, indices)
            raise ETEMPLATEUPCAST(site, "object", new_type)
        else:
            return mkTraitUpcast(site, expr, real.trait)
    elif isinstance(real, TObjIdentity):
        converted = try_convert_identity(expr)
        if converted:
            return converted
        else:
            raise ICE(site, "invalid cast to '_identity_t'")
    old_type = safe_realtype(expr.ctype())
    if (dml.globals.compat_dml12_int(site)
        and (isinstance(old_type, (TStruct, TVector, TLayout))
             or isinstance(real, (TStruct, TVector, TLayout)))):
        # these casts are permitted by C only if old and new are
        # the same type, which is useless
        return Cast(site, expr, new_type)
    if isinstance(real, (TVoid, TStruct, TArray, TVector, TTraitList,
                         TLayout, TFunction)):
        raise ECAST(site, expr, new_type)
    if isinstance(old_type, (TVoid, TStruct, TVector, TTraitList, TLayout,
                             TTrait, TObjIdentity)):
        raise ECAST(site, expr, new_type)
    if old_type.is_int and old_type.is_endian:
        expr = as_int(expr)
        old_type = safe_realtype(expr.ctype())
    if real.is_int and not real.is_endian:
        if isinstance(expr, IntegerConstant):
            value = truncate_int_bits(expr.value, real.signed, real.bits)
            if dml.globals.compat_dml12_int(site):
                return IntegerConstant_dml12(site, value, real)
            elif real.bits == 64:
                return mkIntegerConstant(site, value, real.signed)
            else:
                return TypedIntegerConstant(site, value, real)
        # Shorten redundant chains of integer casts. Avoids insane C
        # output for expressions like a+b+c+d.
        if (isinstance(expr, Cast)
            and isinstance(expr.type, TInt)
            and expr.type.bits >= real.bits):
            # (uint64)(int64)x -> (uint64)x
            expr = expr.expr
            old_type = safe_realtype(expr.ctype())
        if isinstance(old_type, (TFloat, TBool, TUnknown)):
            old_type = TInt(64, True)
            expr = Cast(site, expr, old_type)
        elif isinstance(old_type, (TPtr, TArray, TFunction)):
            old_type = TInt(64, False)
            expr = Cast(site, expr, old_type)
        elif not old_type.is_int:
            raise ECAST(site, expr, new_type)
        if not dml.globals.compat_dml12_int(site):
            # (uint64)x -> x if x is already uint64
            if (isinstance(old_type, TInt)
                and isinstance(real, TInt)
                and old_type.bits == real.bits
                and old_type.signed == real.signed):
                return expr
            if real.bits not in {64, 32, 16, 8}:
                mask = mkIntegerLiteral(site, (1 << real.bits) - 1)
                sign_bit = mkIntegerLiteral(site, 1 << (real.bits - 1))
                if real.signed:
                    expr = mkSubtract(
                        site,
                        mkBitAnd(site, mkAdd(site, expr, sign_bit), mask),
                        sign_bit)
                else:
                    expr = mkBitAnd(site, expr, mask)
            if (type(old_type) != type(real)
                or old_type.bits != real.bits
                or old_type.signed != real.signed):
                expr = Cast(site, expr, new_type)
            return expr
    elif real.is_int and real.is_endian:
        old_type = safe_realtype(expr.ctype())
        if real.cmp(old_type) == 0:
            return expr
        else:
            return mkApply(
                expr.site,
                mkLit(expr.site, *real.get_store_fun()),
                (mkCast(expr.site, expr, TInt(64, False)),))

    return Cast(site, expr, new_type)

class RValue(Expression):
    '''Wraps an lvalue to prohibit write. Useful when a composite
    expression is reduced down to a single variable.'''
    writable = False
    @auto_init
    def __init__(self, site, expr): pass
    def __str__(self):
        return str(self.expr)
    def ctype(self):
        return self.expr.ctype()
    def read(self):
        return self.expr.read()
    def discard(self): pass

def mkRValue(expr):
    if isinstance(expr, LValue) or expr.writable:
        return RValue(expr.site, expr)
    return expr

class InlinedParam(RValue):
    '''An input parameter to a method. Wraps an expression to prohibit it
    from being written.'''
    @auto_init
    def __init__(self, site, expr, name): pass
    @property
    def constant(self):
        return self.expr.constant
    @property
    def value(self):
        return self.expr.value

def mkInlinedParam(site, expr, name, type):
    if not defined(expr):
        raise ICE(site, 'undefined parameter')
    if isinstance(expr, IntegerConstant):
        value = expr.value
        type = realtype(type)
        etype = realtype(expr.ctype())
        # Checking type.canstore(etype) isn't good enough here
        if not type.signed and value < 0:
            value += (1 << etype.bits)
        valmax = 1 << type.bits
        value = value & (valmax - 1)
        if type.signed and value >= (valmax / 2):
            value -= valmax
        expr = IntegerConstant_dml12(site, value, type)
    return InlinedParam(site, expr, name)

class QName(Expression):
    type = TPtr(TNamed('char', const = True))
    @auto_init
    def __init__(self, site, node, relative, indices): pass
    def __str__(self):
        return dollar(self.site) + '%s.qname' % (self.node)
    def read(self):
        if (dml.globals.dml_version == (1, 2)
            and self.node.logname() != self.node.logname_anonymized()):
            report(WCONFIDENTIAL(self.site))

        if self.indices and not all(x.constant for x in self.indices):
            dml.globals.device.use_qname_cache = True
            idx_args = [", (int)" + idx.read() for idx in self.indices]
            return '__qname(&_dev->_qname_cache, "%s"%s)' \
                   % (self.node.logname(("%u",) * len(idx_args),
                                        relative=self.relative),
                      "".join(idx_args))
        else:
            return '"%s"' % self.node.logname(self.indices,
                                              relative=self.relative)
    def fmt(self):
        # This assumes that index variables are always ints
        return (self.node.logname_anonymized(['%u' for _ in self.indices],
                                             relative=self.relative),
                self.indices)

mkQName = QName

def get_anonymized_name(obj):
    if obj.objtype == 'register':
        offset = param_expr(obj, 'offset',
                            (mkIntegerLiteral(obj.site, 0),) * obj.dimensions)
        if undefined(offset):
            # acquire a unique name, based on this banks unmapped registers
            [(idx, _)] = [(idx, r) for (idx, r)
                        in enumerate(obj.bank.unmapped_registers)
                        if r.node == obj]
            return "_ur%d" % idx
        else:
            return "_r%#x" % offset.value
    else:
        assert(obj.objtype == 'field')
        [msb, lsb] = [
            param_int(
                obj, name,
                indices=(mkIntegerLiteral(obj.site, 0),) * obj.dimensions)
            for name in ['msb', 'lsb']]
        return "_b%d_%d" % (msb, lsb)

class HiddenName(StringConstant):
    "Name of the object that will be anonymized in log statements"
    slots = ('node',)
    type = TPtr(TNamed('char', const=True))
    @auto_init
    def __init__(self, site, value, node):
        assert(node.objtype in ('register', 'field'))
    def __str__(self):
        return dollar(self.site) + '%s.name' % (self.node,)
    def read(self):
        report(WCONFIDENTIAL(self.site))
        return self.quoted
    def fmt(self):
        return (get_anonymized_name(self.node), ())

mkHiddenName = HiddenName

class HiddenQName(Expression):
    "QName of the object that will be anonymised in log statements"
    type = TPtr(TNamed('char', const = True))
    @auto_init
    def __init__(self, site, node, indices):
        assert(node.objtype in ('register', 'field'))
    def __str__(self):
        return dollar(self.site) + '%s.qname' % (self.node)
    def read(self):
        return QName(self.site, self.node, 'device', self.indices).read()
    def fmt(self):
        if any(isinstance(i, NonValue) for i in self.indices):
            # Should not happen: fmt is only called from log
            # statements in methods, where indices are known
            raise ICE(self.site, 'cannot format qname with static indices')
        return (self.node.logname_anonymized(('%u',)*len(self.indices)),
                self.indices)

mkHiddenQName = HiddenQName

class DeviceObject(Expression):
    type = TPtr(TNamed('conf_object_t'))
    def read(self):
        return "&_dev->obj"

mkDeviceObject = DeviceObject

class Initializer(object):
    """An initializer is either an expression, or a
    list structure that matches the structure of the data
    being initialized."""
    __slots__ = ()

class ExpressionInitializer(Initializer):
    __slots__ = ('expr',)
    def __init__(self, expr):
        assert isinstance(expr, Expression)
        self.expr = expr
    def __str__(self):
        return "%s" % self.expr
    def __repr__(self):
        return "ExpressionInitializer(%r)" % self.expr
    def incref(self):
        self.expr.incref()
    def decref(self):
        self.expr.decref()
    def read(self):
        return self.expr.read()
    def assign_to(self, dest, typ):
        mkCopyData(self.expr.site, self.expr, dest).toc()

class CompoundInitializer(Initializer):
    '''Initializer for a variable of struct or array type, using the
    {value1, value2, ...}  syntax in C'''
    __slots__ = ('site', 'init')
    def __init__(self, site, init):
        self.site = site
        self.init = init
    def __str__(self):
        return "{%s}" % ",".join(str(e) for e in self.init)
    def __repr__(self):
        return "CompoundInitializer({%s})" % ",".join(
            repr(e) for e in self.init)
    def incref(self):
        for i in self.init:
            i.incref()
    def decref(self):
        for i in self.init:
            i.decref()
    def read(self):
        return '{' + ", ".join(i.read() for i in self.init) + '}'
    def assign_to(self, dest, typ):
        '''output C statements to assign an lvalue'''
        if isinstance(typ, TNamed):
            out('%s = (%s)%s;\n' %
                (dest.read(), typ.declaration(''), self.read()))
        elif isinstance(typ, TArray):
            out('memcpy(%s, (%s)%s, sizeof %s);\n'
                % (dest.read(), typ.declaration(''),
                   self.read(), dest.read()))
        elif isinstance(typ, TStruct):
            out('memcpy(&%s, (%s){%s}, sizeof %s);\n' % (
                dest.read(),
                TArray(typ, mkIntegerLiteral(self.site, 1)).declaration(''),
                self.read(), dest.read()))
        else:
            raise ICE(self.site, 'strange type %s' % typ)

class DesignatedStructInitializer(Initializer):
    '''Initializer for a variable of an extern-declared struct type, using
    the {.member = value, ...}  syntax in C'''
    __slots__ = ('site', 'init')
    def __init__(self, site, init):
        self.site = site
        self.init = init
    def __str__(self):
        return "{%s}" % (','.join(f".{m}={e}" for (m, e) in self.init.items()),)
    def __repr__(self):
        return f"DesignatedStructInitializer({self.init})"
    def incref(self):
        # TODO: not tested.  incref/decref are related to
        # symtab.LocalSymbol: if a symbol is created with stmt=False,
        # then its declaration can be optimized out if refcount
        # remains zero. Also, we sometimes skip the UNUSED annotation
        # if refcount > 0.  Looks like the logic is a bit shaky;
        # Add(x,y).incref() does not incref x, for instance. In any
        # case, it seems that this optimization is mostly a
        # micro-optimization for for inline methods, and we should
        # probably just remove it and add that UNUSED annotation
        # everywhere instead.
        for i in self.init.values():
            i.incref()
    def decref(self):
        for i in self.init.values():
            i.decref()
    def read(self):
        return '{%s}' % (
            ", ".join(f'.{m}={i.read()}' for (m, i) in self.init.items()))
    def assign_to(self, dest, typ):
        '''output C statements to assign an lvalue'''
        typ = safe_realtype(typ)
        if isinstance(typ, StructType):
            out('memcpy(&%s, (%s){%s}, sizeof %s);\n' % (
                dest.read(),
                TArray(typ, mkIntegerLiteral(self.site, 1)).declaration(''),
                self.read(), dest.read()))
        else:
            raise ICE(self.site, f'unexpected type for initializer: {typ}')

class MemsetInitializer(Initializer):
    '''Initializer that clears a struct or array to zero, using memset if
    possible: Because of GCC bugs #53119 and #80454, using {0} as
    initializer for compound types can give an incorrect compile
    warning, so we want to avoid that. And we cannot always use full
    initializers; this would sometimes give an error in case of
    'extern typedef struct' or a VLA.

    The solution is to use a separate memset statement in the common case,
    and fall back to {0} for other cases.

    This initializer may only be used for struct or array initializers.
    '''
    __slots__ = ('site',)
    def __init__(self, site):
        self.site = site
    def __str__(self):
        return self.read()
    def __repr__(self):
        return "MemsetInitializer()"
    def incref(self):
        pass
    def decref(self):
        pass
    def read(self):
        return '{0}'
    def assign_to(self, dest, typ):
        '''output C statements to assign an lvalue'''
        assert isinstance(safe_realtype(typ),
                          (TExternStruct, TStruct, TArray, TEndianInt, TTrait,
                           TObjIdentity))
        out('memset(&%s, 0, sizeof(%s));\n'
            % (dest.read(), typ.declaration('')))

class StructDefinition(Statement):
    """A C struct definition appearing in a local scope, like
    'struct X { ... };'. This is used for anonymous structs; it is
    consistently referenced to as 'struct X', and the first reference
    is preceded by a StructDefinition."""
    @auto_init
    def __init__(self, site, structtype): pass
    def toc(self):
        self.structtype.print_struct_definition()
mkStructDefinition = StructDefinition

class Declaration(Statement):
    "A variable declaration"
    is_declaration = True
    def __init__(self, site, name, type, init = None, unused = False):
        self.site = site
        self.name = name
        self.type = type
        if init:
            assert isinstance(init, Initializer)
        self.init = init

        if name.startswith("__"):
            assert unused == True
        self.unused = unused

    def toc(self):
        self.linemark()

        if isinstance(self.init, MemsetInitializer):
            # ducks a GCC warning
            self.type.print_declaration(self.name, unused = self.unused)
            self.init.assign_to(mkLit(self.site, self.name, self.type),
                                self.type)
        else:
            self.type.print_declaration(
                self.name, init=self.init.read() if self.init else None,
                unused=self.unused)

mkDeclaration = Declaration

def possible_side_effect(init):
    """Return True if this expression might have some side effect
    This means that the expression has to be evaluated exactly once."""
    if not init:
        return False
    # Hack: init can currently be either an Expression or an
    # Initializer. We should make it more consistent.
    if isinstance(init, ExpressionInitializer):
        expr = init.expr
    elif isinstance(init, Expression):
        expr = init
    else:
        return True
    if expr.constant:
        return False
    if isinstance(expr, Variable):
        return False
    return True

def sym_declaration(sym):
    assert not isinstance(sym, symtab.StaticSymbol)
    refcount = sym.refcount()
    if not sym.stmt and refcount == 0 and not possible_side_effect(sym.init):
        # dbg('ignoring %r (init = %r)' % (sym.value, sym.init))
        if sym.init:
            sym.init.decref()
        return None

    # This will prevent warnings from the C compiler
    unused = (refcount == 0) or sym.value.startswith("__")

    return mkDeclaration(sym.site, sym.value, sym.type,
                         sym.init, unused)

###

non_alnum_re = re.compile('[^A-Z0-9]')
def dmldir_macro(path):
    h_path = (path[:-4] if path.endswith('.dml') else path) + '.h'
    ident = 'DMLDIR_' + non_alnum_re.sub(
        '_', os.path.basename(h_path).upper())
    return (ident, h_path)

class CText(Code):
    "A C fragment to be inserted"
    def __init__(self, site, text, impl_header = None):
        self.site = site
        self.text = text
        self.impl_header = impl_header
    def toc(self):
        path = self.site.filename()
        (ident, h_path) = dmldir_macro(path)
        out('#define %s "%s"\n' % (ident, quote_filename(h_path)))
        if dml.globals.linemarks:
            out('#line %d "%s"\n' % (self.site.lineno, quote_filename(path)))
        out(self.text)
        out('\n#undef %s\n' % (ident,))

mkCText = CText

def lookup_var(site, scope, name):
    "Lookup a symbol in scope or parameters and create an expression for it"
    # dbg("LOOKUP_VAR %s %s" % (scope.id, name))
    sym = scope.lookup(name)
    if not sym:
        return None
    elif isinstance(sym, symtab.LocalSymbol):
        expr = mkLocalVariable(site, sym)
        assert expr.site == site
        return expr
    elif isinstance(sym, symtab.StaticSymbol):
        expr = mkStaticVariable(site, sym)
        assert expr.site == site
        return expr
    else:
        return sym.expr(site)

def log_statement(site, node, indices, logtype, level, groups, fmt, *args):
    if logtype in ['error', 'critical']:
        lvl = []
    else:
        if level is None:
            level = mkIntegerLiteral(site, 1)
        lvl = [level]
    logfunc = "SIM_LOG_" + logtype.replace("-", "_").upper()
    if groups is None:
        groups = mkIntegerLiteral(site, 0)

    inargtypes = (([TInt(32, True)] if lvl else [])
                  + [TPtr(TNamed("conf_object_t")), TInt(32, True),
                     TPtr(TInt(8, True, const=True))])
    fun = mkLit(site, logfunc, TFunction(inargtypes, TVoid(), varargs=True))

    logobj = mkLit(site, crep.conf_object(node, indices),
                   TPtr(TNamed("conf_object_t")))
    x = Apply(site, fun,
              lvl +
              [ logobj,
                groups,
                mkStringConstant(site, fmt) ] +
              list(args),
              fun.ctype())
    return mkExpressionStatement(site, x)
