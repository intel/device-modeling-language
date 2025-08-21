# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

# Intermediate representation of the device model

import os.path
import re
import abc
import operator
import itertools
import math
from functools import reduce

from dml import objects, symtab, logging, crep
from .logging import *
from .messages import *
from .output import *
from .types import *
from .expr import *
from .expr_util import *
from .set import Set
from .slotsmeta import auto_init
from . import dmlparse, output
from . import compat
import dml.globals
# set from codegen.py
codegen_call_expr = None

__all__ = (
    'endian_convert_expr',
    'source_for_assignment',

    'Location',

    'ExpressionSymbol',
    'LiteralSymbol',

    'SimpleParamExpr',
    'param_str_fixup',
    'param_bool_fixup',

    'mkCompound',
    'mkNull', 'Null',
    'mkLabel',
    'mkUnrolledLoop',
    'mkGoto',
    'mkReturnFromInline',
    'mkThrow',
    'mkGotoBreak',
    'mkTryCatch',
    'mkInline',
    'mkInlinedMethod',
    'mkIndicesAssert',
    'mkAssert',
    'mkReturn',
    'mkDelete',
    'mkExpressionStatement',
    'mkAfter',
    'mkAfterOnHook',
    'mkImmediateAfter',
    'mkIf',
    'mkWhile',
    'mkDoWhile',
    'mkFor',
    'mkForeachSequence', 'ForeachSequence',
    'mkSwitch',
    'mkSubsequentCases',
    'mkCase',
    'mkDefault',
    'mkVectorForeach',
    'mkBreak',
    'mkContinue',
    'mkAssignStatement',
    'mkCopyData',
    'mkIfExpr', 'IfExpr',
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
    'mkBitOr', 'BitOr', 'BitOr_dml12',
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
    'mkHookSendNowRef', 'HookSendNowRef',
    'mkHookSendNowApply', 'HookSendNowApply',
    'mkHookSendRef', 'HookSendRef',
    'mkHookSendApply', 'HookSendApply',
    'mkTemplatesRef', 'TemplatesRef',
    'mkTemplatesSubRef', 'TemplatesSubRef',
    'mkTemplateQualifiedMethodRef',
    'mkTraitTemplatesRef', 'TraitTemplatesRef',
    'mkTraitTemplatesSubRef', 'TraitTemplatesSubRef',
    'mkTraitTemplateQualifiedMethodRef',
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
    'TraitHookRef',
    'TraitHookArrayRef',
    'TraitMethodRef',
    'TraitMethodIndirect',
    'TraitMethodDirect',
    'mkTraitUpcast',
    'TraitUpcast',
    'TraitObjectCast',
    'ObjIdentity',
    'TraitObjIdentity',
    'ObjTraitRef',
    'PortObjectFromObjIdentity',
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
    'InvalidSymbol',
    'mkQName', 'QName',
    'mkDeviceObject',
    'LogGroup',
    'mkStructDefinition',
    'mkDeclaration',
    'mkCText',
    'Initializer', 'ExpressionInitializer', 'CompoundInitializer',
    'DesignatedStructInitializer', 'MemsetInitializer',

    'as_bool',
    'as_int',
    'sym_declaration',
    'lookup_var',
    'log_object',
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


class SimpleParamExpr(objects.ParamExpr):
    '''A parameter expression, using a simple index-free Expression as
    value'''
    __slots__ = ('expr',)
    def __init__(self, expr: Expression):
        self.expr = expr
    @property
    def site(self): return self.expr.site
    def mkexpr(self, indices):
        return self.expr


def param_str_fixup(node, name: str, fixup: str) -> str:
    """Return the value of a string parameter. On error, report the
    error, change the parameter to evaluate to `fixup`, and return
    that.
    """
    try:
        return param_str(node, name)
    except DMLError as e:
        report(e)
        pnode = node.get_component(name)
        pnode.set_expr(SimpleParamExpr(mkStringConstant(
            node.site, fixup)))
        return param_str(node, name)


def param_bool_fixup(node, name: str, fixup: bool) -> bool:
    """Return a parameter's value. On error,
    report the error, change the parameter to evaluate to `fixup`, and
    return that.
    """
    try:
        return param_bool(node, name)
    except DMLError as e:
        report(e)
        pnode = node.get_component(name)
        pnode.set_expr(SimpleParamExpr(mkBoolConstant(node.site, fixup)))
        return param_bool(node, name)


class ExpressionSymbol(symtab.Symbol):
    """A symbol that corresponds to an expression."""
    def __init__(self, name, expr, site):
        super(ExpressionSymbol, self).__init__(name, value = expr, site = site)
    @property
    def type(self):
        return self.value.ctype()
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

    # Emit a single (labeled) C statement.
    @abc.abstractmethod
    def toc_stmt(self): pass
    # Emit any number of (labeled) C statements or declarations. These may be
    # placed inside an existing C block.
    def toc(self): self.toc_stmt()
    # Emit any number of (labeled) C statements or declarations. These are
    # guaranteed to be placed in a dedicated C block.
    def toc_inline(self): self.toc()


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
    def __repr__(self):
        return f'Compound({self.substatements!r})'

    def __init__(self, site, substatements, rbrace_site=None):
        assert isinstance(substatements, list)
        self.site = site
        self.substatements = substatements
        self.rbrace_site = rbrace_site

    def toc_stmt(self):
        self.linemark()
        out('{\n', postindent=1)
        self.toc_inline()
        site_linemark(self.rbrace_site)
        out('}\n', preindent=-1)

    @property
    def is_empty(self):
        return all(stmt.is_empty for stmt in self.substatements)

    def toc(self):
        if any(sub.is_declaration for sub in self.substatements):
            self.toc_stmt()
        else:
            self.toc_inline()

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

def mkCompound(site, statements, rbrace_site=None):
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
    else:
        return Compound(site, collapsed, rbrace_site)

class Null(Statement):
    is_empty = True
    def toc_stmt(self):
        self.linemark()
        out(';\n')
    def toc(self):
        pass

mkNull = Null

class Label(Statement):
    def __init__(self, site, label, unused=False):
        Statement.__init__(self, site)
        self.label = label
        self.unused = unused
    def toc_stmt(self):
        self.linemark()
        out('%s: %s;\n' % (self.label, 'UNUSED'*self.unused),
            preindent=-1, postindent=1)

mkLabel = Label

class UnrolledLoop(Statement):
    @auto_init
    def __init__(self, site, substatements, break_label):
        assert isinstance(substatements, list)

    def toc_stmt(self):
        self.linemark()
        out('{\n', postindent=1)
        self.toc_inline()
        self.linemark()
        out('}\n', preindent=-1)

    def toc(self):
        for substatement in self.substatements:
            substatement.toc()
        if self.break_label is not None:
            self.linemark()
            out(f'{self.break_label}: UNUSED;\n')

    def control_flow(self):
        bodyflow = mkCompound(self.site, self.substatements).control_flow()
        return bodyflow.replace(fallthrough=(bodyflow.fallthrough
                                             or bodyflow.br), br=False)

mkUnrolledLoop = UnrolledLoop

class Goto(Statement):
    @auto_init
    def __init__(self, site, label): pass
    def toc_stmt(self):
        self.linemark()
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

class GotoBreak(Goto):
    def control_flow(self):
        return ControlFlow(br=True)

mkGotoBreak = GotoBreak

class TryCatch(Statement):
    '''A DML try/catch statement. Catch block is represented as an if (false)
    block with a catch label, to which Throw statements inside will go.'''
    @auto_init
    def __init__(self, site, label, tryblock, catchblock): pass
    def toc_stmt(self):
        self.linemark()
        out('{\n', postindent=1)
        self.toc_inline()
        self.linemark()
        out('}\n', preindent=-1)

    def toc(self):
        self.tryblock.toc()

        if (dml.globals.dml_version != (1, 2)
            and not self.tryblock.control_flow().fallthrough):
            site_linemark(self.catchblock.site)
            out(f'{self.label}:;\n')
            self.catchblock.toc()
        else:
            # Our fallthrough analysis is more conservative than Coverity's
            coverity_marker('unreachable', site=self.catchblock.site)
            out(f'if (false) {self.label}:\n')
            toc_under_if(self.catchblock)

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
    def toc_stmt(self):
        self.linemark()
        out(self.str + '\n')

mkInline = Inline

class InlinedMethod(Statement):
    '''Wraps the body of an inlined method, both to protect it from analysis
       and to ensure linemarks are created for it'''
    @auto_init
    def __init__(self, site, method, pre, body, post): pass
    def toc_stmt(self):
        self.linemark()
        out('{\n', postindent = 1)
        self.toc_inline()
        self.linemark()
        out('}\n', preindent = -1)
    def toc(self):
        if (any(stmt.is_declaration
                for stmt in itertools.chain(self.pre, self.body, self.post))):
            self.toc_stmt()
        else:
            self.toc_inline()
    def toc_inline(self):
        for stmt in self.pre:
            stmt.toc()
        with allow_linemarks():
            for stmt in self.body:
                stmt.toc()
        for stmt in self.post:
            stmt.toc()
    def control_flow(self):
        return ControlFlow(fallthrough=True, throw=self.method.throws)

def mkInlinedMethod(site, method, pre, body, post):
    collapsed = []
    for stmt in body:
        if (isinstance(stmt, Compound)
            and not any(subsub.is_declaration
                        for subsub in stmt.substatements)):
            collapsed.extend(stmt.substatements)
        elif not stmt.is_empty:
            collapsed.append(stmt)
    return InlinedMethod(site, method, pre, collapsed, post)

class IndicesAssert(Statement):
    @auto_init
    def __init__(self, site, method, indices): pass

    def toc_stmt(self):
        self.linemark()
        length = len(self.indices)
        indices = ', '.join(f'({idx.read()})' for idx in self.indices)
        dimsizes = ', '.join(map(str, self.method.dimsizes))
        check = ' || '.join(f'({idx.read()}) >= {size}'
                            for (idx, size) in zip(self.indices,
                                                   self.method.dimsizes))
        out('DML_ASSERT_INDICES('
            + f'{check}, _id_infos[{self.method.parent.uniq - 1}].logname, '
            + f'"{self.method.name}", ((const uint32 []) {{ {indices} }}), '
            + f'((const uint32 []) {{ {dimsizes} }}), {length});\n')

mkIndicesAssert = IndicesAssert

class Assert(Statement):
    @auto_init
    def __init__(self, site, expr): pass
    def toc_stmt(self):
        self.linemark()
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
    def toc_stmt(self):
        self.linemark()
        out('return;\n' if self.expr is None else
            f'return {self.expr.read()};\n')
    def control_flow(self):
        return ControlFlow()

def mkReturn(site, expr, rettype=None):
    if rettype and not dml.globals.compat_dml12_int(site):
        expr = source_for_assignment(site, rettype, expr)
    return Return(site, expr)

class Delete(Statement):
    @auto_init
    def __init__(self, site, expr): pass
    def toc_stmt(self):
        self.linemark()
        out(f'MM_FREE({self.expr.read()});\n')

def mkDelete(site, expr):
    return Delete(site, expr)

class ExpressionStatement(Statement):
    @auto_init
    def __init__(self, site, expr): pass
    def toc_stmt(self):
        self.linemark()
        out(self.expr.discard()+';\n')

def mkExpressionStatement(site, expr):
    if isinstance(expr, Constant):
        return mkNull(site)
    return ExpressionStatement(site, expr)

def toc_constsafe_pointer_assignment(site, source, target, typ):
    target_val = mkDereference(site,
        Cast(site, mkLit(site, target, TPtr(void)), TPtr(typ)))
    mkAssignStatement(site, target_val,
                      ExpressionInitializer(mkLit(site, source, typ))).toc()

class After(Statement):
    @auto_init
    def __init__(self, site, clock, unit, delay, domains, info, indices,
                 args_init):
        crep.require_dev(site)

    def toc_stmt(self):
        self.linemark()
        out('{\n', postindent=1)
        self.toc_inline()
        self.linemark()
        out('}\n', preindent=-1)

    def toc(self):
        def lm_out(*args, **kwargs):
            self.linemark()
            out(*args, **kwargs)
        objarg = '&_dev->obj'
        lm_out(f'if (SIM_object_clock({objarg}) == NULL)\n', postindent=1)
        lm_out(f'''SIM_log_error({objarg}, 0, "Attribute 'queue' is '''
               + '''not set, ignoring delayed call");\n''')
        lm_out('else {\n', preindent=-1, postindent=1)
        if self.indices or self.info.args_type or self.domains:
            lm_out('_simple_event_data_t *_data = MM_ZALLOC(1, '
                   + '_simple_event_data_t);\n')

            if self.indices:
                lm_out(f'uint32 *_event_indices = '
                       + f'MM_MALLOC({len(self.indices)}, uint32);\n')
                for (i, index_expr) in enumerate(self.indices):
                    lm_out(f'_event_indices[{i}] = {index_expr.read()};\n')
                lm_out('_data->indices = _event_indices;\n')

            args_type = self.info.args_type
            if args_type:
                lm_out('%s = %s;\n'
                    % (args_type.declaration('_event_args'),
                       self.args_init.args_init()))
                lm_out('_data->args = MM_MALLOC(1, %s);\n'
                    % (args_type.declaration(''),))

                toc_constsafe_pointer_assignment(self.site, '_event_args',
                                                 '_data->args', args_type)

            if self.domains:
                lm_out('_identity_t *_event_domains = '
                       + f'MM_MALLOC({len(self.domains)}, _identity_t);\n')
                for (i, domain) in enumerate(self.domains):
                    lm_out(f'_event_domains[{i}] = {domain.read()};\n')
                lm_out(f'_data->no_domains = {len(self.domains)};\n')
                lm_out('_data->domains = _event_domains;\n')

            data = '(lang_void *)_data'
        else:
            data = 'NULL'
        lm_out(f'SIM_event_post_{self.unit}({self.clock}, '
               + f'{crep.get_evclass(self.info.key)}, {objarg}, '
               + f'{self.delay.read()}, {data});\n')
        lm_out("}\n", preindent = -1)

mkAfter = After

def resolve_hookref(hookref):
    return ('&_dev->' + crep.cref_hook(hookref.hook, hookref.indices)
            if isinstance(hookref, HookRef) else
            f'_DML_resolve_hookref(_dev, _hook_aux_infos, {hookref.read()})')

class AfterOnHook(Statement):
    slots = ('info',)
    @auto_init
    def __init__(self, site, domains, hookref_expr, info, indices,
                 args_init):
        crep.require_dev(site)
    def toc_stmt(self):
        hookref = resolve_hookref(self.hookref_expr)
        indices = ('(const uint32 []) {%s}'
                   % (', '.join(i.read() for i in self.indices),)
                   if self.indices else 'NULL')
        args = ('(%s){%s}'
                % (TArray(self.info.args_type,
                          mkIntegerLiteral(self.site, 1)).declaration(''),
                   self.args_init.args_init())
                if self.info.args_type else 'NULL')
        domains = ('(const _identity_t []) {%s}'
                   % (', '.join(domain.read() for domain in self.domains),)
                   if self.domains else 'NULL')
        self.linemark()
        # Block is to delimit lifetime of compound literals
        out(f'{{ _DML_attach_callback_to_hook({hookref}, '
            + f'&_after_on_hook_infos[{self.info.uniq}], {indices}, '
            + f'{len(self.indices)}, {args}, {domains}, '
            + f'{len(self.domains)}); }}\n')

mkAfterOnHook = AfterOnHook

class ImmediateAfter(Statement):
    @auto_init
    def __init__(self, site, domains, info, indices, args_init):
        crep.require_dev(site)
    def toc_stmt(self):
        indices = ('(const uint32 []) {%s}'
                   % (', '.join(i.read() for i in self.indices),)
                   if self.indices else 'NULL')
        if self.info.args_type is not None:
            args = ('(%s){%s}'
                    % (TArray(self.info.args_type,
                              mkIntegerLiteral(self.site, 1)).declaration(''),
                       self.args_init.args_init()))
            args_size = f'sizeof({self.info.args_type.declaration("")})'
        else:
            (args, args_size) = ('NULL', '0')
        domains = ('(const _identity_t []) {%s}'
                   % (', '.join(domain.read() for domain in self.domains),)
                   if self.domains else 'NULL')
        self.linemark()
        # Block is to delimit lifetime of compound literals
        out('{ _DML_post_immediate_after('
            + '&_dev->obj, _dev->_immediate_after_state, '
            + f'{self.info.cident_callback}, {indices}, {len(self.indices)}, '
            + f'{args}, {args_size}, {domains}, {len(self.domains)}); }}\n')

mkImmediateAfter = ImmediateAfter

# To be used instead of .toc_stmt() when generating the statement under an
# `if`.
# This applies a work-around to avoid possible `if`-`else` ambiguity in
# generated C, which GCC would otherwise complain about
def toc_under_if(stmt):
    if isinstance(stmt, (If, For, While, VectorForeach)):
        stmt.linemark()
        out('{\n', postindent=1)
        stmt.toc_inline()
        stmt.linemark()
        out('}\n', preindent=-1)
    else:
        stmt.toc_stmt()

class If(Statement):
    @auto_init
    def __init__(self, site, cond, truebranch, falsebranch, else_site):
        assert_type(site, cond.ctype(), TBool)
        assert_type(site, truebranch, Statement)
        assert_type(site, falsebranch, (Statement, type(None)))
    def toc_stmt(self):
        self.linemark()
        out(f'if ({self.cond.read()})\n')
        toc_under_if(self.truebranch)

        if self.falsebranch:
            site_linemark(self.else_site)
            out('else\n')
            self.falsebranch.toc_stmt()

    def control_flow(self):
        a = self.truebranch.control_flow()
        b = (self.falsebranch.control_flow() if self.falsebranch
             else ControlFlow(fallthrough=True))
        return a.union(b)

def mkIf(site, cond, truebranch, falsebranch = None, else_site=None):
    assert isinstance(cond.ctype(), TBool)
    if cond.constant:
        if cond.value:
            return truebranch
        elif falsebranch:
            return falsebranch
        else:
            return mkNull(site)
    return If(site, cond, truebranch, falsebranch, else_site)

class While(Statement):
    @auto_init
    def __init__(self, site, cond, stmt):
        assert_type(site, cond.ctype(), TBool)
        assert_type(site, stmt, Statement)
    def toc_stmt(self):
        self.linemark()
        out(f'while ({self.cond.read()})\n')
        self.stmt.toc_stmt()

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
    def toc_stmt(self):
        self.linemark()
        out('do\n')
        self.stmt.toc_stmt()
        site_linemark(self.cond.site)
        out(f'while ({self.cond.read()});\n')
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
    def toc_stmt(self):
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
            out('({\n', postindent=1)
            for post in self.posts:
                post.toc()
            self.linemark()
            out(' })', preindent=-1)

        out(')\n')
        self.stmt.toc_stmt()

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

class ForeachSequence(Statement):
    @staticmethod
    def itervar_initializer(site, trait):
        trait_type = TTrait(trait)
        vtable_init = ExpressionInitializer(mkLit(site, '_list.vtable',
                                                  trait_type))

        list_id_init = ExpressionInitializer(mkLit(site, '_list.id',
                                                   TInt(32, False)))
        inner_idx_init = ExpressionInitializer(mkLit(site, '_inner_idx',
                                                     TInt(32, False)))
        obj_ref_init = CompoundInitializer(site,
                                           [list_id_init, inner_idx_init])

        return CompoundInitializer(site, [vtable_init, obj_ref_init])

    @auto_init
    def __init__(self, site, trait, each_in_expr, body, break_label): pass
    def toc_stmt(self):
        self.linemark()
        out('{\n', postindent=1)
        self.toc_inline()
        self.linemark()
        out('}\n', preindent=-1)

    def toc_inline(self):
        def lm_out(*args, **kwargs):
            self.linemark()
            out(*args, **kwargs)
        lm_out(f'_each_in_t __each_in_expr = {self.each_in_expr.read()};\n')
        coverity_marker('unreachable', site=self.site)
        out('for (uint32 _outer_idx = 0; _outer_idx < __each_in_expr.num; '
            + '++_outer_idx) {\n', postindent=1)
        lm_out(f'_vtable_list_t _list = {EachIn.array_ident(self.trait)}'
               + '[__each_in_expr.base_idx + _outer_idx];\n')
        lm_out('uint32 _num = _list.num / __each_in_expr.array_size;\n')
        lm_out('uint32 _start = _num * __each_in_expr.array_idx;\n')
        coverity_marker('unreachable', site=self.site)
        out('for (uint32 _inner_idx = _start; _inner_idx < _start + _num; '
            + '++_inner_idx)\n')
        self.body.toc_stmt()
        lm_out('}\n', preindent=-1)
        if self.break_label is not None:
            lm_out(f'{self.break_label}: UNUSED;\n', preindent=-1,
                   postindent=1)

    def control_flow(self):
        bodyflow = self.body.control_flow()
        # fallthrough is possible if the sequence is empty
        return bodyflow.replace(fallthrough=True, br=False)

mkForeachSequence = ForeachSequence

class Switch(Statement):
    @auto_init
    def __init__(self, site, expr, stmt):
        assert_type(site, expr, Expression)
        assert_type(site, stmt, Statement)
    def toc_stmt(self):
        self.linemark()
        out(f'switch ({self.expr.read()})\n')
        self.stmt.toc_stmt()

    def control_flow(self):
        assert self.site.dml_version() != (1, 2)
        # guaranteed by grammar in DML 1.4.
        assert isinstance(self.stmt, Compound)
        found_default = False
        # The possible exit paths from the sequence of statements
        # processed so far
        flow = ControlFlow(fallthrough=True)
        for stmt in self.stmt.substatements:
            if (isinstance(stmt, Default)
                or (isinstance(stmt, SubsequentCases)
                    and stmt.has_default)):
                found_default = True
            if isinstance(stmt, (Default, Case, SubsequentCases)):
                flow = flow.replace(fallthrough=True)
            elif flow.fallthrough:
                f = stmt.control_flow()
                flow = flow.union(f, fallthrough=f.fallthrough)
        return flow.replace(
            fallthrough=flow.fallthrough or flow.br or not found_default,
            br=False)

def mkSwitch(site, expr, stmt):
    return Switch(site, as_int(expr), stmt)

class SubsequentCases(Statement):
    @auto_init
    def __init__(self, site, cases, has_default):
        assert len(self.cases) > 0

    def toc_stmt(self):
        for (i, case) in enumerate(self.cases):
            assert isinstance(case, (Case, Default))
            site_linemark(case.site)
            semi = ';' * (i == len(self.cases) - 1)
            if isinstance(case, Case):
                out(f'case {case.expr.read()}:{semi}\n', preindent = -1,
                    postindent = 1)
            else:
                out(f'default:{semi}\n', preindent = -1, postindent = 1)

mkSubsequentCases = SubsequentCases

class Case(Statement):
    @auto_init
    def __init__(self, site, expr): pass
    def toc_stmt(self):
        self.linemark()
        out('case %s: ;\n' % self.expr.read(), preindent = -1, postindent = 1)

mkCase = Case

class Default(Statement):
    @auto_init
    def __init__(self, site): pass
    def toc_stmt(self):
        self.linemark()
        out('default: ;\n', preindent = -1, postindent = 1)

mkDefault = Default

class VectorForeach(Statement):
    @auto_init
    def __init__(self, site, vect, var, stmt): pass

    def toc_stmt(self):
        self.linemark()
        out(f'VFOREACH({self.vect.read()}, {self.var.read()})\n')
        self.stmt.toc_stmt()

    def control_flow(self):
        flow = self.stmt.control_flow()
        return flow.replace(fallthrough=flow.fallthrough or flow.br, br=False)

def mkVectorForeach(site, vect, var, stmt):
    return VectorForeach(site, vect, var, stmt)

class Break(Statement):
    def toc_stmt(self):
        self.linemark()
        out('break;\n')
    def control_flow(self):
        return ControlFlow(br=True)

mkBreak = Break

class Continue(Statement):
    def toc_stmt(self):
        self.linemark()
        out('continue;\n')
    def control_flow(self):
        return ControlFlow()

mkContinue = Continue

class AssignStatement(Statement):
    @auto_init
    def __init__(self, site, target, initializer):
        assert isinstance(initializer, Initializer)
    def toc_stmt(self):
        self.linemark()
        out('{\n', postindent=1)
        self.toc_inline()
        self.linemark()
        out('}\n', preindent=-1)
    def toc_inline(self):
        self.linemark()
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
        if not realtype(e.ctype()).eq(target_type):
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
                    + f' {source.read()})')
        return '%s = %s' % (self.read(), source.read())

    @property
    def is_stack_allocated(self):
        '''Returns true only if it's known that writing to the lvalue will
           write to stack-allocated data'''
        return False

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
            assert rtype.eq(utype)
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
            if (isinstance(ttype, (TPtr, TArray))
                and isinstance(ftype, (TPtr, TArray))
                and (ttype.base.void or ftype.base.void
                     or safe_realtype_unconst(ttype.base).eq(
                         safe_realtype_unconst(ftype.base)))):
                # if any branch is void, then the union is too
                base = (ftype if ftype.base.void else ttype).base.clone()
                # if any branch is const *, then the union is too
                base.const = ((isinstance(ttype, TArray) and ttype.const)
                              or (isinstance(ftype, TArray) and ftype.const)
                              or shallow_const(ttype.base)
                              or shallow_const(ftype.base))
                utype = TPtr(base)
            elif safe_realtype_unconst(ttype).eq(safe_realtype_unconst(ftype)):
                utype = ttype
            else:
                raise EBINOP(site, ':', texpr, fexpr)
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
        if (dml.globals.dml_version == (1, 2)
            and (isinstance(lhtype, TUnknown) or isinstance(rhtype, TUnknown))):
            # urgh, some classes take an extra constructor arg
            if issubclass(cls, (ArithBinOp, BitBinOp, BitShift)):
                return cls(site, lh, rh, TUnknown)
            else:
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

    @staticmethod
    @abc.abstractmethod
    def eval_const(lh, rh):
        pass

    @classmethod
    def make(cls, site, lh, rh):
        lhtype = realtype(lh.ctype())
        rhtype = realtype(rh.ctype())

        if (lhtype.is_arith and rhtype.is_arith
            and lh.constant and rh.constant):
            return mkBoolConstant(site, cls.eval_const(lh.value, rh.value))
        if lhtype.is_int:
            lh_maybe_negative = lhtype.signed
            lh = as_int(lh)
            lhtype = realtype(lh.ctype())
        if rhtype.is_int:
            rh_maybe_negative = rhtype.signed
            rh = as_int(rh)
            rhtype = realtype(rh.ctype())
        if (isinstance(lhtype, TInt)
            and isinstance(rhtype, TInt)
            and lh_maybe_negative != rh_maybe_negative):
            (signed_expr, unsigned_expr) = ((lh, rh) if lh_maybe_negative
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
                        site, cls.cmp_functions[rh_maybe_negative],
                        TFunction([TInt(64, lh_maybe_negative),
                                   TInt(64, rh_maybe_negative)],
                                  TBool())),
                    [lh, rh])
        if ((lhtype.is_arith and rhtype.is_arith)
            or (isinstance(lhtype, (TPtr, TArray))
                and isinstance(rhtype, (TPtr, TArray))
                and safe_realtype_unconst(lhtype.base).eq(
                    safe_realtype_unconst(rhtype.base)))):
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
            (lhc, rhc) = tuple(e.expr if isinstance(e, InlinedParam) else e
                               for e in (lh, rh))
            if lhtype.is_arith and rhtype.is_arith:
                return mkBoolConstant(site, lhc.value == rhc.value)
            if isinstance(lhc, BoolConstant) and isinstance(rhc, BoolConstant):
                return mkBoolConstant(site, lhc.value == rhc.value)
            # This is probably the single weirdest widely used feature
            # of DML. Preserved because of use cases for which we
            # don't have a good replacement yet.
            if (isinstance(lhc, StringConstant)
                and isinstance(rhc, StringConstant)):
                return mkBoolConstant(site, lhc.value == rhc.value)
            if (isinstance(lhc, ObjTraitRef) and isinstance(rhc, ObjTraitRef)
                and lhc.trait is rhc.trait):
                lhc_indices = [idx.value for idx in lhc.indices]
                rhc_indices = [idx.value for idx in rhc.indices]
                return mkBoolConstant(site, (lhc.node is rhc.node
                                             and lhc_indices == rhc_indices))
            if (isinstance(lhc, HookRef) and isinstance(rhc, HookRef)
                and lhtype.eq(rhtype)):
                lh_indices = [idx.value for idx in lhc.indices]
                rh_indices = [idx.value for idx in rhc.indices]
                return mkBoolConstant(site, (lhc.hook is rhc.hook
                                             and lh_indices == rh_indices))
            if isinstance(lhc, NullConstant) or isinstance(rhc, NullConstant):
                if (isinstance(lhc, NullConstant)
                    and isinstance(rhc, NullConstant)):
                    return mkBoolConstant(site, True)
                elif (isinstance(lhtype, (TPtr, TArray))
                      and isinstance(rhtype, (TPtr, TArray))):
                    for expr in (lhc, rhc):
                        assert isinstance(expr, (NullConstant, StringConstant,
                                                 AddressOfMethod))
                    return mkBoolConstant(site, False)
        if lhtype.is_int:
            lh_maybe_negative = lhtype.signed
            lh = as_int(lh)
            lhtype = realtype(lh.ctype())
        if rhtype.is_int:
            rh_maybe_negative = rhtype.signed
            rh = as_int(rh)
            rhtype = realtype(rh.ctype())

        if (isinstance(lhtype, TInt) and isinstance(rhtype, TInt)
            and lh_maybe_negative != rh_maybe_negative):
            # There is no primitive for signed/unsigned compare in C,
            # so use a lib function for it. However, we can fall back
            # to C's == in the very common case when comparing
            # unsigned to a constant literal.
            (signed_expr, unsigned_expr) = ((lh, rh) if lh_maybe_negative
                                            else (rh, lh))
            if signed_expr.constant and signed_expr.value < 0:
                report(WNEGCONSTCOMP(site, signed_expr, unsigned_expr.ctype()))
            if not (signed_expr.constant and 0 <= signed_expr.value < 1 << 63):
                return mkApply(
                    site, mkLit(
                        site, 'DML_eq',
                        TFunction([TInt(64, False), TInt(64, False)], TBool())),
                    [lh, rh])

        if ((lhtype.is_arith and rhtype.is_arith)
            or (isinstance(lhtype, (TPtr, TArray))
                and isinstance(rhtype, (TPtr, TArray))
                and (lhtype.base.void or rhtype.base.void
                     or safe_realtype_unconst(lhtype.base).eq(
                         safe_realtype_unconst(rhtype.base))))
            or (isinstance(lhtype, TBool) and isinstance(rhtype, TBool))):
            return Equals(site, lh, rh)

        if (isinstance(lhtype, TTrait) and isinstance(rhtype, TTrait)
            and lhtype.trait is rhtype.trait):
            return IdentityEq(site, TraitObjIdentity(lh.site, lh),
                              TraitObjIdentity(rh.site, rh))
        if (isinstance(lhtype, THook) and isinstance(rhtype, THook)
            and lhtype.eq(rhtype)):
            return IdentityEq(site, lh, rh)

        raise EILLCOMP(site, lh, lhtype, rh, rhtype)

def mkEquals(site, lh, rh):
    if dml.globals.compat_dml12_int(site):
        return Equals_dml12.make(site, lh, rh)
    else:
        return Equals.make(site, lh, rh)

class IdentityEq(Expression):
    priority = dml.expr.Apply.priority
    type = TBool()

    @auto_init
    def __init__(self, site, lh, rh): pass

    def __str__(self):
        lh = str(self.lh)
        rh = str(self.rh)
        if self.lh.priority <= Equals.priority:
            lh = f'({lh})'
        if self.rh.priority <= Equals.priority:
            rh = f'({rh})'
        return f'{lh} == {rh}'

    def read(self):
        if self.site is None:
            return f'_identity_eq({self.lh.read()}, {self.rh.read()})'
        else:
            return (f'_identity_eq_at_site({self.lh.read()}, {self.rh.read()}'
                    + f', "{quote_filename(self.site.filename())}", '
                    + f'{self.site.lineno})')


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
    @staticmethod
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
    @staticmethod
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
    @staticmethod
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

    @property
    def is_pointer_to_stack_allocation(self):
        return (isinstance(safe_realtype_shallow(self.ctype()), (TPtr, TArray))
                and (self.lh.is_pointer_to_stack_allocation
                     or self.rh.is_pointer_to_stack_allocation))

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

    @property
    def is_pointer_to_stack_allocation(self):
        return self.rh.is_pointer_to_stack_allocation

def mkAssignOp(site, target, source):
    if isinstance(target, InlinedParam):
        raise EASSINL(target.site, target.name)
    if not target.writable:
        raise EASSIGN(site, target)

    target_type = target.ctype()

    source = source_for_assignment(site, target_type, source)
    if deep_const(target_type):
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
                    site, '_DML_EV_'+crep.cref_method(node),
                    TFunction([TPtr(TNamed('conf_object_t')),
                               TPtr(TVoid())],
                              TVoid())))
        if (compat.dml12_misc not in dml.globals.enabled_compat
            and not isinstance(rh, LValue)):
            raise ERVAL(rh.site, '&')
        return AddressOf(site, rh)

    @property
    def is_pointer_to_stack_allocation(self):
        return isinstance(self.rh, LValue) and self.rh.is_stack_allocated

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

    @property
    def is_stack_allocated(self):
        return self.rh.is_pointer_to_stack_allocation

    @property
    def is_pointer_to_stack_allocation(self):
        return isinstance(self.type, TArray) and self.is_stack_allocated

mkDereference = Dereference.make

class Not(UnaryOp):
    op = '!'
    type = TBool()

    @staticmethod
    def make_simple(site, rh):
        if compat.dml12_misc not in dml.globals.enabled_compat:
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

    @property
    def is_pointer_to_stack_allocation(self):
        return self.rh.is_pointer_to_stack_allocation

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

        # HACK this ad-hoc DeviceInstanceContext is needed to construct this
        # expression (which is then immediately .read()) as at this point,
        # DMLC is at the output stage.
        with crep.DeviceInstanceContext():
            return mkCast(iface_noderef.site,
                          mkSubRef(iface_noderef.site, iface_noderef, 'val',
                                   '.'),
                          TPtr(TNamed(struct_name, const=True))).read()

class MethodPresent(Expression):
    '''Whether a method in an interface object is NULL'''
    type = TBool()
    @auto_init
    def __init__(self, site, expr):
        assert isinstance(expr, InterfaceMethodRef)
    def __str__(self):
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
    def apply(self, inits, location, scope):
        args = typecheck_inarg_inits(
            self.site, inits,
            [(str(i + 1), t)
             for (i, t) in enumerate(self.ftype.input_types[1:])],
            location, scope, 'function', self.ftype.varargs)

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

    if (not isinstance(ftype, TPtr)
        or not isinstance(ftype.base, TFunction)
        or not ftype.base.input_types
        or not TPtr(safe_realtype_unconst(TNamed('conf_object_t'))).eq(
            safe_realtype_unconst(ftype.base.input_types[0]))):
        # non-method members are not accessible
        raise EMEMBER(site, struct_name, method_name)

    obj_node = iface_node.parent.get_component('obj')
    if not obj_node or not obj_node.objtype == 'session':
        raise ICE(site, 'connect without obj')
    obj_ref = mkNodeRef(site, obj_node, indices)
    return InterfaceMethodRef(
        site, mkNodeRef(site, iface_node, indices), method_name, obj_ref,
        ftype.base)

class BitSlice(Expression):
    # msb and lsb are expressed using le bitorder
    slots = ('type', 'read_expr')
    priority = 0
    explicit_type = True
    @auto_init
    def __init__(self, site, expr, msb, lsb, size, mask):
        # lsb is None if i[bitnum] (as opposed to i[msb:lsb]) notation was used
        if size:
            const = deep_const(self.expr.ctype())
            if size.constant:
                self.type = TInt(size.value, False, const=const)
            else:
                self.type = TInt(64, False, const=const)

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
    def __init__(self, site, traitref, methname, independent, inargs, type):
        if not independent:
            crep.require_dev(site)

    def __str__(self):
        return '%s.%s(%s)' % (self.traitref, self.methname,
                              ', '.join(map(str, self.inargs)))

    def read(self):
        infix_independent = 'INDEPENDENT_' if self.independent else ''
        suffix_noarg = '' if self.inargs else '0'
        macro = f'CALL_{infix_independent}TRAIT_METHOD{suffix_noarg}'
        args = (['_dev'] * (not self.independent)
                + [arg.read() for arg in [self.traitref] + self.inargs])
        trait_name = cident(realtype(self.traitref.ctype()).trait.name)
        return f"{macro}({trait_name}, {self.methname}, {', '.join(args)})"

class TraitMethodApplyDirect(Expression):
    '''The C expression of a trait method call'''
    @auto_init
    def __init__(self, site, traitref, methodref, inargs, type):
        # traitref is a reference to method's vtable trait
        assert realtype(traitref.ctype()).trait == methodref.vtable_trait
        assert methodref.__class__.__name__ == 'TraitMethod'
        if not methodref.independent:
            crep.require_dev(site)

    def __str__(self):
        return '%s(%s)' % (self.methodref, ', '.join(map(str, self.inargs)))

    def read(self):
        return "%s(%s)" % (
            self.methodref.cname(),
            ', '.join(['_dev'] * (not self.methodref.independent)
                      + [arg.read() for arg in [self.traitref] + self.inargs]))

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
        for index in indices:
            if isinstance(index, NonValue):
                raise index.exc()
        self.indices = indices

    def __str__(self):
        return 'each %s in %s' % (
            self.trait.name, self.node.logname(self.indices))

    def ctype(self):
        return TTraitList(self.trait.name)

    @staticmethod
    def index_ident(node, trait):
        '''C identifier name for vtable_list_t instance'''
        # 'dev' is guaranteed not to clash with the name of another object
        name = 'dev' if node is dml.globals.device else node.attrname()
        return f'_each__{trait.name}__in__{name}'

    @staticmethod
    def array_ident(trait):
        '''C identifier name for vtable_list_t instance'''
        return f'_each__{trait.name}'

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
                    yield from EachIn.subobjs_implementing(sub, trait)

    def mark_referenced(self, subobjs):
        key = (self.node, self.trait)
        if key not in self.instances:
            self.instances[key] = subobjs
            for sub in subobjs:
                sub.traits.mark_referenced(self.trait)

    def each_in_t_members(self):
        subobjs = list(self.subobjs_implementing(self.node, self.trait))
        if not subobjs:
            # Iteration will never start. Note: array_size==1, because
            # 0 is used as a sentinel value by _each_in_param_t.
            return ([], '0', 0, '0', 1)
        if self.indices:
            dimsizes = self.node.dimsizes
            array_size = reduce(operator.mul, dimsizes)
            array_idx = encode_indices(self.indices, dimsizes)
        else:
            array_size = 1
            array_idx = "0"
        return (subobjs, self.index_ident(self.node, self.trait),
                len(subobjs), array_idx, array_size)

    def read(self):
        (subobjs, ident, num, array_idx, array_size) = self.each_in_t_members()
        self.mark_referenced(subobjs)
        return '(_each_in_t){%s, %d, %s, %d}' % (
            ident, num, array_idx, array_size)

mkEachIn = EachIn

class SequenceLength(Expression):
    '''The length of a sequence'''
    type = TInt(64, False)
    @auto_init
    def __init__(self, site, expr, trait): pass

    def __str__(self):
        return "%s.len" % self.expr
    def read(self):
        return (f'_count_eachin({self.expr.read()}'
                f', {EachIn.array_ident(self.trait)})')

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

class AddressOfMethod(Constant):
    def ctype(self):
        params = (self.value.cparams if self.value.independent else
                  [("_obj", TPtr(TNamed("conf_object_t")))]
                  + self.value.cparams[1:])
        return TPtr(TFunction([typ for (_, typ) in params],
                              self.value.rettype))

    def read(self):
        prefix = '_trampoline' * (not self.value.independent)
        return f'(&{prefix}{self.value.get_cname()})'


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
    slots = ('node', 'trait', 'indices', 'ancestry_path', 'constant', 'value')
    def __init__(self, site, node, trait, indices,
                 ancestry_path=None):
        Expression.__init__(self, site)
        if not isinstance(indices, tuple):
            raise ICE(site, 'bad indices: %r' % (indices,))
        assert len(indices) == node.dimensions
        for index in indices:
            if isinstance(index, NonValue):
                raise index.exc()
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

        if all(idx.constant for idx in indices):
            self.constant = True
            self.value = (node, tuple(idx.value for idx in indices))
        else:
            self.constant = False
            self.value = None

    def __str__(self):
        return "%s.%s" % (self.node.logname(self.indices), self.trait.name)

    def ctype(self):
        return TTrait(self.trait)

    def read(self):
        self.node.traits.mark_referenced(self.trait)
        if self.constant:
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
        structref = self.node.traits.vtable_cname(self.ancestry_path[0])
        pointer = '(&%s)' % ('.'.join([structref] + [
            cident(t.name) for t in self.ancestry_path[1:]]))
        id = ObjIdentity(self.site, self.node, indices).read()
        traitref_expr = ('((%s) {%s, %s})'
                         % (cident(self.trait.name), pointer, id))
        if indices_decl:
            return '({%s; %s;})' % (indices_decl, traitref_expr)
        else:
            return traitref_expr

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
        return TNamed('_identity_t')

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
        return TNamed('_identity_t')

    def read(self):
        return "(%s).id" % (self.traitref.read(),)

class PortObjectFromObjIdentity(Expression):
    priority = dml.expr.Apply.priority
    @auto_init
    def __init__(self, site, identity):
        crep.require_dev(site)

    def ctype(self):
        return TNamed('conf_object_t *')

    def read(self):
        return ('_identity_to_portobj(_port_object_assocs, &_dev->obj, '
                + f'{self.identity.read()})')

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

class TraitObjectCast(Expression):
    @auto_init
    def __init__(self, site, sub): pass

    def __str__(self):
        return f'cast({self.sub}, object)'

    def ctype(self):
        return TTrait(dml.globals.object_trait)

    def read(self):
        return (f'({{_identity_t __id = ({self.sub.read()}).id; '
                + '(_traitref_t) {_object_vtables[__id.id], __id};})')

def mkTraitUpcast(site, sub, parent):
    if isinstance(sub, NonValue):
        raise sub.exc()
    typ = safe_realtype(sub.ctype())
    assert dml.globals.object_trait
    if isinstance(typ, TTrait):
        if typ.trait is parent:
            return sub
        elif parent in typ.trait.ancestors:
            return TraitUpcast(site, sub, parent)
        elif parent is dml.globals.object_trait:
            return TraitObjectCast(site, sub)
    raise ETEMPLATEUPCAST(site, typ, parent.type())

def vtable_read(expr):
    typ = realtype(expr.ctype())
    assert isinstance(typ, TTrait)
    return '((struct _%s *) (%s).trait)' % (cident(typ.trait.name),
                                            expr.read())

class TraitParameter(Expression):
    priority = dml.expr.Apply.priority
    @auto_init
    def __init__(self, site, traitref, name, type): pass

    def __str__(self):
        return "%s.%s" % (self.traitref, self.name)

    def read(self):
        t = realtype(self.traitref.ctype())
        assert isinstance(t, TTrait)
        vtable_type = f'struct _{cident(t.trait.name)}'
        if isinstance(realtype(self.type), TTraitList):
            return (f'_vtable_sequence_param({self.traitref.read()},'
                    f' offsetof({vtable_type}, {self.name}))')
        else:
            return (f'VTABLE_PARAM({self.traitref.read()}, {vtable_type}'
                    f', {self.name})')

class TraitSessionRef(Expression):
    '''A reference to a trait session variable.

    Hack: this actually represents the address of the variable, so an
    actual reference must be wrapped in a Dereference. The Dereference
    indirection is there as an easy way to make session references lvalues.'''
    priority = 140 # Cast.priority
    @auto_init
    def __init__(self, site, traitref, name, type_):
        crep.require_dev(self.site)

    def __str__(self):
        return "&%s.%s" % (self.traitref, self.name)

    def ctype(self):
        return TPtr(self.type_)

    def read(self):
        t = realtype(self.traitref.ctype())
        assert isinstance(t, TTrait)
        vtable_type = f'struct _{cident(t.trait.name)}'
        return (f'VTABLE_SESSION(_dev, {self.traitref.read()}, {vtable_type}'
                f', {self.name}, {self.ctype().declaration("")})')

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
    @abc.abstractproperty
    def independent(self): pass

    def apply(self, inits, location, scope):
        '''Return expression for application as a function'''
        if self.throws or len(self.outp) > 1:
            raise EAPPLYMETH(self.site, self)
        if crep.TypedParamContext.active and self.independent:
            raise ETYPEDPARAMVIOL(self.site)
        args = typecheck_inarg_inits(self.site, inits, self.inp,
                                     location, scope, 'method')
        if self.outp:
            [(_, rettype)] = self.outp
        else:
            rettype = TVoid()
        return self.call_expr(args, rettype)

class TraitMethodDirect(TraitMethodRef):
    '''A direct reference to a specific trait method instance,
    as opposed to an indirect vtable lookup. This is used for references
    to unoverridable shared implementations, the DML expression 'default'
    when evaluated inside a shared method, and template-qualified method
    implementation calls to shared implementations'''
    @auto_init
    def __init__(self, site, traitref, methodref): pass

    def __str__(self):
        return "%s.templates.%s.%s" % (
            self.traitref, str(realtype(self.traitref.ctype()).trait.name),
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

    @property
    def independent(self):
        return self.methodref.independent

    def call_expr(self, inargs, rettype):
        return TraitMethodApplyDirect(self.site, self.traitref,
                                      self.methodref, inargs, rettype)

class TraitMethodIndirect(TraitMethodRef):
    '''An indirect reference to a trait method,
    meaning that the reference is looked up in runtime via a
    vtable. The expression cannot currently be referenced standalone;
    it needs to be called.'''
    @auto_init
    def __init__(self, site, traitref, methname, inp, outp, throws,
                 independent): pass

    def __str__(self):
        return "%s.%s" % (str(self.traitref), self.methname)

    def call_expr(self, inargs, rettype):
        return TraitMethodApplyIndirect(self.site, self.traitref,
                                        self.methname, self.independent,
                                        inargs, rettype)

class TraitHookArrayRef(NonValueArrayRef):
    @auto_init
    def __init__(self, site, dimsizes, hooktyp, traitref, name, indices):
        assert len(indices) < len(dimsizes)

    @property
    def local_indices(self):
        return self.indices

    @property
    def local_dimsizes(self):
        return self.dimsizes

    def __str__(self):
        return "%s.%s%s" % (self.traitref, self.name,
                          ''.join(f'[{expr}]' for expr in self.indices))

class TraitHookRef(Expression):
    priority = 1000
    @auto_init
    def __init__(self, site, dimsizes, hooktyp, traitref, name, indices):
        assert len(dimsizes) == len(self.indices)

    def __str__(self):
        return "%s.%s%s" % (self.traitref, self.name,
                          ''.join(f'[{expr}]' for expr in self.indices))

    def ctype(self):
        return self.hooktyp

    def read(self):
        t = realtype(self.traitref.ctype())
        assert isinstance(t, TTrait)
        vtable_type = f'struct _{cident(t.trait.name)}'

        coeff = math.prod(self.dimsizes)
        if all(idx.constant for idx in self.indices):
            index_offset = encode_indices_constant(
                (idx.value for idx in self.indices), self.dimsizes)
        else:
            index_offset = encode_indices(self.indices, self.dimsizes)
        return (f'VTABLE_HOOK({self.traitref.read()}, {vtable_type}, '
                + f'{self.name}, {coeff}, {index_offset})')


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

    if (compat.dml12_misc in dml.globals.enabled_compat
        and not base.parent):
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
    def apply(self, inits, location, scope):
        '''Apply as an expression'''
        if self.node.objtype == 'method':
            if self.node.throws or len(self.node.outp) > 1:
                raise EAPPLYMETH(self.site, self.node.name)
            return codegen_call_expr(self.site, self.node, self.indices, inits,
                                     location, scope)
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
            and self.node.objtype in {'field', 'register',
                                      'connect', 'attribute',
                                      'interface'}
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
            expr = '_DML_M_' + crep.cref_method(node)
        elif node.objtype == 'device':
            assert dml.globals.dml_version == (1, 2)
            return '_dev'
        else:
            expr = '_dev->' + crep.cref_session(node, self.indices)
        return expr

    def apply(self, inits, location, scope):
        if self.node.objtype == 'method':
            assert dml.globals.dml_version == (1, 2)
            raise EAPPLYMETH(self.site, self.node.name)
        # storage might be a function pointer
        return mkApplyInits(self.site, self, inits, location, scope)

class SessionVariableRef(LValue):
    "A reference to a session variable"
    slots = ('type',)
    priority = 1000
    explicit_type = True
    @auto_init
    def __init__(self, site, node, indices):
        assert isinstance(node, objects.DMLObject)
        assert isinstance(indices, tuple)
        assert node.objtype in {'session', 'saved'}
        self.type = node._type
        crep.require_dev(site)
    def __str__(self):
        name = self.node.logname(self.indices)
        assert name
        return name
    def read(self):
        return '_dev->' + crep.cref_session(self.node, self.indices)

class HookRef(Expression):
    "A reference to a hook"
    slots = ('type', 'value', 'constant', 'constant_indices')
    explicit_type = True
    @auto_init
    def __init__(self, site, hook, indices):
        assert isinstance(hook, objects.DMLObject)
        assert isinstance(indices, tuple)
        assert hook.objtype == 'hook'
        self.type = THook(hook.msg_types, validated=True)

        if all(idx.constant for idx in indices):
            self.constant = True
            self.constant_indices = tuple(idx.value for idx in indices)
            self.value = (hook, self.constant_indices)
        else:
            self.constant = False
            self.constant_indices = None
            self.value = None
    def __str__(self):
        name = self.hook.logname(self.indices)
        assert name
        return name
    def read(self):
        if self.constant:
            encoded_index = encode_indices_constant(self.constant_indices,
                                                    self.hook.dimsizes)
        else:
            encoded_index = encode_indices(self.indices, self.hook.dimsizes)
        return ('((_hookref_t) {.id = %d, .encoded_index = %s})'
                % (self.hook.uniq, encoded_index))

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

class InvalidSymbol(NonValue):
    '''Used as the expression of an ExpressionSymbol to represent an invalid
    identifier.'''
    @auto_init
    def __init__(self, site, name, make_error): pass
    def __str__(self):
        return self.name
    def exc(self):
        return self.make_error(self.site)

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
    elif node.objtype == 'hook':
        for i in indices:
            if isinstance(i, NonValue):
                raise i.copy(site).exc()
        return HookRef(site, node, indices)
    t = node_type(node, site)
    if not t:
        return PlainNodeRef(site, node, indices)
    if dml.globals.dml_version == (1, 2):
        # Registers and fields have a type, but sometimes not a value
        if node.objtype == "register" and not node.wholefield:
            return RegisterWithFields(site, node, indices, t)
        if (node.objtype in {'register', 'field'}
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

class NodeArrayRef(NonValueArrayRef):
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

    @property
    def local_indices(self):
        return self.indices[self.node.nonlocal_dimensions():]

    @property
    def local_dimsizes(self):
        return self.node.dimsizes[self.node.nonlocal_dimensions():]

    def __str__(self):
        name = self.node.logname(self.indices)
        return dollar(self.site) + name

    def exc(self):
        return EARRAY(self.site, self.node.identity())

class HookSuspended(Expression):
    '''Reference to the suspended member of a hook'''
    priority = 160
    type = TInt(64, False)

    @auto_init
    def __init__(self, site, hookref_expr): pass

    def __str__(self):
        return "%s.suspended" % (self.hookref_expr,)

    def read(self):
        hookref = resolve_hookref(self.hookref_expr)
        return f'(uint64)VLEN(({hookref})->queue)'

mkHookSuspended = HookSuspended

class HookSendNowRef(NonValue):
    '''Reference to the send_now pseudomethod of a hook'''
    @auto_init
    def __init__(self, site, hookref_expr): pass

    def __str__(self):
        return "%s.send_now" % (self.hookref_expr,)
    def apply(self, inits, location, scope):
        msg_types = safe_realtype_shallow(self.hookref_expr.ctype()).msg_types
        args = typecheck_inarg_inits(
            self.site, inits,
            [(f'comp{i}', t)
             for (i, t) in enumerate(msg_types)],
            location, scope, 'send_now')
        return mkHookSendNowApply(self.site, self.hookref_expr, args)

mkHookSendNowRef = HookSendNowRef

class HookSendNowApply(Expression):
    '''Application of the send_now pseudomethod with valid arguments'''
    slots = ('msg_struct',)
    type = TInt(64, False)
    priority = dml.expr.Apply.priority
    @auto_init
    def __init__(self, site, hookref_expr, args):
        crep.require_dev(site)
        msg_types = safe_realtype(hookref_expr.ctype()).msg_types
        from .codegen import get_type_sequence_info
        self.msg_struct = get_type_sequence_info(msg_types,
                                                 create_new=True).struct

    def __str__(self):
        return '%s.send_now(%s)' % (self.hookref_expr,
                                    ', '.join(str(e) for e in self.args))
    def read(self):
        msg = (('&(%s_t) {%s}'
                % (self.msg_struct.label,
                   ', '.join(arg.read() for arg in self.args)))
               if self.args else 'NULL')
        hookref = resolve_hookref(self.hookref_expr)

        return ('_DML_send_hook(&_dev->obj, '
                + f'&_dev->_detached_hook_queue_stack, {hookref}, {msg})')

mkHookSendNowApply = HookSendNowApply

class HookSendRef(NonValue):
    '''Reference to the send pseudomethod of a hook'''
    @auto_init
    def __init__(self, site, hookref_expr): pass

    def __str__(self):
        return "%s.send" % (self.hookref_expr,)
    def apply(self, inits, location, scope):
        msg_types = safe_realtype_shallow(self.hookref_expr.ctype()).msg_types
        args = typecheck_inarg_inits(
            self.site, inits,
            [(f'comp{i}', t)
             for (i, t) in enumerate(msg_types)],
            location, scope, 'send',
            on_ptr_to_stack=(lambda x: report(WHOOKSEND(x.site, x))))
        from .codegen import get_type_sequence_info, get_immediate_after
        typeseq_info = get_type_sequence_info(msg_types, create_new=True)
        after_info = get_immediate_after(typeseq_info)
        return mkHookSendApply(self.site, self.hookref_expr, args, after_info)

mkHookSendRef = HookSendRef

class HookSendApply(Expression):
    '''Application of the send pseudomethod with valid arguments'''
    type = void
    priority = dml.expr.Apply.priority
    @auto_init
    def __init__(self, site, hookref_expr, args, info):
        crep.require_dev(site)

    def __str__(self):
        return '%s.send(%s)' % (self.hookref_expr,
                                ', '.join(str(e) for e in self.args))
    def read(self):
        if self.args:
            args = ('&(%s){ %s, { %s } }'
                    % (self.info.args_type.declaration(''),
                       self.hookref_expr.read(),
                       ', '.join(arg.read() for arg in self.args)))
        else:
            args = ('(%s){%s}'
                    % (TArray(self.info.args_type,
                              mkIntegerLiteral(self.site, 1)).declaration(''),
                       self.hookref_expr.read()))
        args_size = f'sizeof({self.info.args_type.declaration("")})'
        return ('_DML_post_immediate_after('
                + '&_dev->obj, _dev->_immediate_after_state, '
                + f'{self.info.cident_callback}, NULL, 0, {args}, '
                + f'{args_size}, NULL, 0)')

mkHookSendApply = HookSendApply

class TemplatesRef(NonValue):
    '''Reference to the 'templates' param of an object, used for
       template-qualified method implementation calls'''
    @auto_init
    def __init__(self, site, node, indices): pass

    def __str__(self):
        return f'{self.node.logname(self.indices)}.templates'

mkTemplatesRef = TemplatesRef

class TemplatesSubRef(NonValue):
    '''A subreference of the 'templates' param of an object, specifying the
       template of the template-qualified method implementation call'''
    @auto_init
    def __init__(self, site, templates_ref, template_name): pass

    def __str__(self):
        return f'{self.templates_ref}.{self.template_name}'

def mkTemplatesSubRef(site, templates_ref, template_name):
    tmpl = dml.globals.templates.get(template_name)
    if tmpl is None:
        raise ENTMPL(site, template_name)
    if tmpl not in templates_ref.node.templates:
        raise ETQMIC(site, templates_ref.node.identity(), template_name)
    return TemplatesSubRef(site, templates_ref, template_name)

def mkTemplateQualifiedMethodRef(site, templates_subref, method_name):
    node = templates_subref.templates_ref.node
    indices = templates_subref.templates_ref.indices

    template_name = templates_subref.template_name
    template = dml.globals.templates[template_name]

    rank_to_candidate = {}

    # Add any shared method implementation as a possible candidate
    trait = dml.globals.traits.get(template_name)
    shared_method = None
    if trait is not None:
        for impl_trait in trait.method_impl_traits.get(method_name, []):
            impl_template = dml.globals.templates[impl_trait.name]
            shared_method = impl_trait.method_impls[method_name]
            rank_to_candidate[impl_template.spec.rank] = (impl_template,
                                                          shared_method)

    some_spec_eliminated = False

    # Don't bother going through regular methods if if some shared method
    # implementation is unoverridable (can only be one if so)
    if shared_method is None or shared_method.overridable:
        studied_templates = set()
        pending_templates = [template]
        # Find the highest ranking candidate of each distinct template
        # hierarchy that is actually implemented for the object
        while pending_templates:
            tmpl = pending_templates.pop()
            if tmpl in studied_templates:
                continue
            studied_templates.add(tmpl)
            (implements_method,
             next_templates) = tmpl.get_potential_method_impl_details(
                 method_name)

            if implements_method:
                candidate = node.template_method_impls.get((tmpl, method_name))
                if candidate is not None:
                    rank_to_candidate[tmpl.spec.rank] = (tmpl, candidate)
                    continue
                else:
                    some_spec_eliminated = True

            pending_templates.extend(next_templates)

    # Eliminate any candidates with lower rank of those found
    # Needed because distinct template hierarchies may still share tails.
    # E.g. you can have
    # T -> X (doesn't provide impl. due to #if) -> Z (provides impl.)
    # T -> Y (provides impl.) -> Z
    # ... in which case the candidate implementation of Z should be excluded
    from .traits import get_highest_ranks
    candidates = [rank_to_candidate[r]
                  for r in get_highest_ranks(Set(rank_to_candidate))]

    if not candidates:
        abstract = (trait is not None
                    and trait.member_declaration(method_name) is not None
                    and trait.member_kind(method_name) == 'method')
        raise EMEMBERTQMIC(site, template, method_name, abstract,
                           node if some_spec_eliminated else None)

    if len(candidates) > 1:
        raise EAMBTQMIC(site, template_name, method_name, some_spec_eliminated,
                        candidates)

    (tmpl, method) = candidates[0]

    if isinstance(method, objects.Method):
        return mkNodeRef(site, method, indices)
    else:
        impl_trait = method.trait
        ancestry_path = node.traits.ancestry_paths[impl_trait][0]
        if impl_trait is not method.vtable_trait:
            # This is safe; ancestry paths are tuples
            ancestry_path += impl_trait.ancestry_paths[method.vtable_trait][0]
        traitref = ObjTraitRef(site, node, method.vtable_trait, indices,
                               ancestry_path=ancestry_path)
        return TraitMethodDirect(site, traitref, method)

class TraitTemplatesRef(NonValue):
    '''Reference to the 'templates' member of a traitref, used for
       template-qualified method implementation calls'''
    @auto_init
    def __init__(self, site, trait, traitref): pass

    def __str__(self):
        return f'{self.traitref}.templates'

mkTraitTemplatesRef = TraitTemplatesRef

class TraitTemplatesSubRef(NonValue):
    '''A subreference of the 'templates' param of an object, specifying the
       template of the template-qualified method implementation call'''
    @auto_init
    def __init__(self, site, templates_ref, trait): pass

    def __str__(self):
        return f'{self.templates_ref}.{self.trait.name}'

def mkTraitTemplatesSubRef(site, templates_ref, template_name):
    trait = dml.globals.traits.get(template_name)
    if trait is None:
        raise ENTMPL(site, template_name)
    if not (templates_ref.trait.implements(trait)
            or trait is dml.globals.object_trait):
        raise ETTQMIC(site, trait.name, templates_ref.trait.name)

    return TraitTemplatesSubRef(site, templates_ref, trait)

def mkTraitTemplateQualifiedMethodRef(site, templates_subref, method_name):
    trait = templates_subref.trait

    tmpl = dml.globals.templates[trait.name]
    (provides_impl, next_tmpls) = tmpl.get_potential_method_impl_details(
        method_name)
    impl_tmpls = (tmpl,) if provides_impl else next_tmpls

    impl_traits = trait.method_impl_traits.get(method_name)
    if not impl_traits:
        if impl_tmpls:
            raise ENSHAREDTQMIC(site, trait, method_name)
        else:
            abstract = (trait.member_declaration(method_name) is not None
                        and trait.member_kind(method_name) == 'method')
            raise EMEMBERTQMIC(site, trait, method_name, abstract, None)

    # If there is any non-shared method specification not lower in rank than
    # some shared method implementation, then it could be a potential
    # implementation candidate, which renders the TQMIC invalid
    for impl_tmpl in impl_tmpls:
        if all(impl_tmpl.spec.rank
               not in dml.globals.templates[impl_trait.name].spec.rank.inferior
               for impl_trait in impl_traits):
            raise ENSHAREDTQMIC(site, trait, method_name)

    if len(impl_traits) > 1:
        raise EAMBTQMIC(site, trait.name, method_name, False,
                        [(impl_trait, impl_trait.method_impls[method_name])
                         for impl_trait in impl_traits])

    impl_trait = impl_traits[0]
    method = impl_trait.method_impls[method_name]
    traitref = templates_subref.templates_ref.traitref
    traitref_trait = templates_subref.templates_ref.trait
    if not traitref_trait.implements(impl_trait):
        assert trait is dml.globals.object_trait
        traitref = TraitObjectCast(site, traitref)
        traitref_trait = dml.globals.object_trait
    if traitref_trait is not impl_trait:
        traitref = mkTraitUpcast(site, traitref, impl_trait)
    if impl_trait is not method.vtable_trait:
        traitref = mkTraitUpcast(site, traitref, method.vtable_trait)

    return TraitMethodDirect(site, traitref, method)

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
    @property
    def is_stack_allocated(self):
        return True

    @property
    def is_pointer_to_stack_allocation(self):
        return isinstance(safe_realtype_shallow(self.ctype()), TArray)

mkLocalVariable = LocalVariable

class StaticVariable(Variable):
    "Static variable storage"
    @auto_init
    def __init__(self, site, sym):
        assert_type(site, sym, symtab.StaticSymbol)
        crep.require_dev(site)
    def read(self):
        return '_dev->%s' % (self.sym.value,)

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

    @property
    def is_stack_allocated(self):
        return isinstance(self.expr, LValue) and self.expr.is_stack_allocated

    @property
    def is_pointer_to_stack_allocation(self):
        return isinstance(self.type, TArray) and self.is_stack_allocated

def try_resolve_len(site, lh):
    if isinstance(lh, NonValue):
        if isinstance(lh, AbstractList):
            return mkIntegerConstant(site,
                                     len(tuple(lh.iter_flat())), False)
        elif isinstance(lh, NonValueArrayRef):
            return mkIntegerConstant(site,
                                     lh.local_dimsizes[len(lh.local_indices)],
                                     False)
    return None

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

    if isinstance(expr, NonValue):
        if op == '.':
            if site.dml_version() != (1, 2) and sub == 'len':
                member = try_resolve_len(site, expr)
                if member:
                    return member

            if isinstance(expr, TemplatesRef):
                return mkTemplatesSubRef(site, expr, sub)
            elif isinstance(expr, TemplatesSubRef):
                return mkTemplateQualifiedMethodRef(site, expr, sub)
            elif isinstance(expr, TraitTemplatesRef):
                return mkTraitTemplatesSubRef(site, expr, sub)
            elif isinstance(expr, TraitTemplatesSubRef):
                return mkTraitTemplateQualifiedMethodRef(site, expr, sub)

        raise expr.exc()

    if isinstance(expr, AddressOf) and op == '->':
        expr = expr.rh
        op = '.'
    elif isinstance(expr, Dereference) and op == '.':
        expr = expr.rh
        op = '->'

    etype = expr.ctype()
    real_etype = safe_realtype_shallow(etype)

    if isinstance(real_etype, TPtr):
        if op == '.':
            raise ENOSTRUCT(site, expr)
        basetype = real_etype.base
        real_basetype = safe_realtype(basetype)
        baseexpr = mkDereference(site, expr)
    else:
        if op == '->':
            raise ENOPTR(site, expr)
        basetype = etype
        real_basetype = safe_realtype(etype)
        baseexpr = expr

    real_basetype = real_basetype.resolve()

    if isinstance(real_basetype, StructType):
        typ = real_basetype.get_member_qualified(sub)
        if not typ:
            raise EMEMBER(site, baseexpr, sub)
        return StructMember(site, expr, sub, typ, op)
    elif real_basetype.is_int and real_basetype.is_bitfields:
        member = real_basetype.members.get(sub)
        if member is None:
            raise EMEMBER(site, expr, sub)
        (_, msb, lsb) = member
        return mkBitSlice(site,
                          baseexpr,
                          mkIntegerLiteral(site, msb),
                          mkIntegerLiteral(site, lsb),
                          'le')
    elif isinstance(real_basetype, TTrait):
        m = real_basetype.trait.lookup(sub, baseexpr, site)
        if not m:
            raise EMEMBER(site, expr, sub)
        return m
    elif isinstance(real_basetype, TArray) and sub == 'len':
        if real_basetype.size.constant:
            return mkIntegerConstant(site, real_basetype.size.value, False)
        else:
            raise EVLALEN(site)
    elif isinstance(real_basetype, TTraitList) and sub == 'len':
        try:
            trait = dml.globals.traits[real_basetype.traitname]
        except KeyError:
            raise ETYPE(basetype.declaration_site or site, basetype)
        return mkSequenceLength(site, baseexpr, trait)
    elif isinstance(real_basetype, THook):
        real_basetype.validate(basetype.declaration_site or site)
        if sub == 'send':
            return mkHookSendRef(site, baseexpr)
        elif sub == 'send_now':
            return mkHookSendNowRef(site, baseexpr)
        elif sub == 'suspended':
            return mkHookSuspended(site, baseexpr)

    raise ENOSTRUCT(site, expr)

class ArrayRef(LValue):
    slots = ('type',)
    priority = 160
    explicit_type = True
    @auto_init
    def __init__(self, site, expr, idx):
        expr_type = realtype_shallow(expr.ctype())
        self.type = conv_const(expr_type.const
                               and isinstance(expr_type, TArray),
                               expr_type.base)
    def __str__(self):
        return '%s[%s]' % (self.expr, self.idx)
    def read(self):
        expr = self.expr.read()
        if self.expr.priority < self.priority:
            expr = '(%s)' % (expr,)
        return '%s[%s]' % (expr, self.idx.read())

    @property
    def is_stack_allocated(self):
        return self.expr.is_pointer_to_stack_allocation

    @property
    def is_pointer_to_stack_allocation(self):
        return isinstance(self.type, TArray) and self.is_stack_allocated

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
            # handled by NonValueArrayRef case below
            pass
        else:
            raise idx.exc()
    else:
        idx = as_int(idx)
    if isinstance(expr, NonValue):
        if isinstance(expr, NonValueArrayRef):
            local_indices = expr.local_indices
            if not isinstance(idx, StaticIndex):
                if idx.constant:
                    if (idx.value < 0 or
                        idx.value >= expr.local_dimsizes[len(local_indices)]):
                        raise EOOB(idx)
            if len(expr.local_dimsizes) > len(local_indices) + 1:
                if isinstance(expr, NodeArrayRef):
                    return NodeArrayRef(site, expr.node, expr.indices + (idx,))
                else:
                    assert isinstance(expr, TraitHookArrayRef)
                    return TraitHookArrayRef(site, expr.dimsizes,
                                             expr.hooktyp, expr.traitref,
                                             expr.name, expr.indices + (idx, ))
            else:
                if isinstance(expr, NodeArrayRef):
                    return mkNodeRef(site, expr.node, expr.indices + (idx,))
                else:
                    return TraitHookRef(
                        site, expr.dimsizes, expr.hooktyp, expr.traitref,
                        expr.name, expr.indices + (idx,))
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

    @property
    def is_pointer_to_stack_allocation(self):
        return (isinstance(self.type, TPtr)
                and self.expr.is_pointer_to_stack_allocation)

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

    if (compat.dml12_misc in dml.globals.enabled_compat
        and isinstance(expr, InterfaceMethodRef)):
        # Workaround for SIMICS-9868
        return mkLit(site, "%s->%s" % (
            expr.node_expr.read(), expr.method_name), new_type)

    if isinstance(expr, NonValue):
        raise expr.exc()
    old_type = safe_realtype(expr.ctype())
    if (dml.globals.compat_dml12_int(site)
        and (isinstance(old_type, (TStruct, TVector))
             or isinstance(real, (TStruct, TVector)))):
        # these casts are permitted by C only if old and new are
        # the same type, which is useless
        return Cast(site, expr, new_type)
    if isinstance(real, (TVoid, TArray, TFunction)):
        raise ECAST(site, expr, new_type)
    if old_type.eq(real):
        if (dml.globals.compat_dml12_int(expr.site)
            and old_type.is_int
            and not old_type.is_endian):
            # 1.2 integer expressions often lie about their actual type,
            # and require a "redundant" cast! Why yes, this IS horrid!
            if expr.constant:
                # urgh, SIMICS-23124
                return IntegerConstant_dml12(
                    site,
                    truncate_int_bits(expr.value, real.signed, real.bits),
                    real)
            return Cast(site, expr, new_type)
        return mkRValue(expr)
    if isinstance(real, (TStruct, TExternStruct, TVector, TTraitList)):
        raise ECAST(site, expr, new_type)
    if isinstance(old_type, (TVoid, TStruct, TVector, TTraitList, TTrait)):
        raise ECAST(site, expr, new_type)
    if old_type.is_int and old_type.is_endian:
        expr = as_int(expr)
        old_type = safe_realtype(expr.ctype())
    if real.is_int and not real.is_endian:
        if old_type.is_int and expr.constant:
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
            and isinstance(old_type, TInt)
            and old_type.bits >= real.bits):
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
        if old_type.is_arith or isinstance(old_type, TPtr):
            return mkApply(
                expr.site,
                mkLit(expr.site, *real.get_store_fun()),
                (mkCast(expr.site, expr, TInt(64, False)),))
        else:
            raise ECAST(site, expr, new_type)
    if ((real.is_arith or isinstance(real, TBool))
         and (old_type.is_arith or isinstance(old_type, TBool))):
        assert (not (real.is_int and real.is_endian)
                and not (old_type.is_int and old_type.is_endian))
        return Cast(site, expr, new_type)
    if ((isinstance(real, (TBool, TPtr)) or real.is_int)
        and (isinstance(old_type, (TBool, TPtr, TArray, TFunction))
             or old_type.is_int)):
        assert (not (real.is_int and real.is_endian)
                and not (old_type.is_int and old_type.is_endian))
        if isinstance(old_type, (TPtr, TArray)) and isinstance(real, TPtr):
            old_base = old_type.base
            new_base = real.base
            old_base_deep = old_base
            while isinstance(old_base_deep, TArray):
                old_base_deep = old_base_deep.base

            if (not dml.globals.compat_dml12_int(site)
                and isinstance(old_base_deep, (TLayout, TEndianInt))
                and new_base.is_int and not new_base.is_endian
                and not new_base.bits == 8
                and not (old_base_deep.is_int and old_base_deep.bits == 8)):
                byte_order = (old_base_deep.byte_order
                              if old_base_deep.is_int
                              else old_base_deep.endian)
                likely_intended = TEndianInt(new_base.bits,
                                             new_base.signed,
                                             byte_order,
                                             const=new_base.const)
                report(WPCAST(site, old_base, new_base, likely_intended))

        return Cast(site, expr, new_type)

    # Allow unsafe casts from method references to function pointers in DML 1.2
    # for compatibility reasons
    if (dml.globals.dml_version == (1, 2) and isinstance(expr, NodeRef)
        and expr.get_ref()[0].objtype == 'method'
        and isinstance(real, TPtr) and isinstance(real.base, TFunction)):
        return Cast(site, expr, new_type)
    # Allow casts from dev to pointer types in DML 1.2 for compatibility
    # reasons
    if (dml.globals.dml_version == (1, 2) and isinstance(expr, NodeRef)
        and expr.get_ref()[0].objtype == 'device' and isinstance(real, TPtr)):
        return Cast(site, expr, new_type)

    raise ECAST(site, expr, new_type)

class RValue(Expression):
    '''Wraps an lvalue to prohibit write. Useful when a composite
    expression is reduced down to a single variable.'''
    @auto_init
    def __init__(self, site, expr): pass
    def __str__(self):
        return str(self.expr)
    def ctype(self):
        return self.expr.ctype()
    def read(self):
        return self.expr.read()
    def discard(self):
        return self.expr.discard()
    def incref(self):
        self.expr.incref()
    def decref(self):
        self.expr.decref()
    @property
    def explicit_type(self):
        return self.expr.explicit_type
    @property
    def type(self):
        assert self.explicit_type
        return self.expr.type
    @property
    def is_pointer_to_stack_allocation(self):
        return self.expr.is_pointer_to_stack_allocation

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
    if isinstance(expr, InlinedParam):
        expr = expr.expr
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
    def __init__(self, site, node, relative, indices):
        if self.indices and not all(x.constant for x in self.indices):
            crep.require_dev(site)
    def __str__(self):
        return dollar(self.site) + '%s.qname' % (self.node)
    def read(self):
        if (dml.globals.dml_version == (1, 2)
            and self.node.logname() != self.node.logname_anonymized()):
            report(WCONFIDENTIAL(self.site))

        if self.indices and not all(x.constant for x in self.indices):
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
        assert(node.objtype in {'register', 'field'})
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
        assert(node.objtype in {'register', 'field'})
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
    @auto_init
    def __init__(self, site):
        crep.require_dev(site)

    def read(self):
        return "&_dev->obj"

mkDeviceObject = DeviceObject

class LogGroup(Expression):
    type = TInt(64, False, const=True)
    slots = ('name',)
    priority = 1000
    @auto_init
    def __init__(self, site, name): pass

    def __str__(self):
        return self.name

    def read(self):
        return crep.cloggroup(self.name)

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
    @property
    def site(self):
        return self.expr.site
    def incref(self):
        self.expr.incref()
    def decref(self):
        self.expr.decref()
    def read(self):
        return self.expr.read()
    def as_expr(self, typ):
        return source_for_assignment(self.expr.site, typ, self.expr)
    def assign_to(self, dest, typ):
        # Assigns to (partially) const-qualified targets can happen as part of
        # initializing (partially) const-qualified session variables. To allow
        # for such assigns while avoiding GCC errors, memcpy is used together
        # with a cast to (void *).
        # Since session variables are allocated on the heap, this should *not*
        # be UB as long as the session variable hasn't been initialized
        # previously.
        site = self.expr.site
        if deep_const(typ):
            out('memcpy((void *)&%s, (%s){%s}, sizeof %s);\n'
                % (dest.read(),
                   TArray(typ, mkIntegerLiteral(site, 1)).declaration(''),
                   mkCast(site, self.expr, typ).read(),
                   dest.read()))
        else:
            with disallow_linemarks():
                mkCopyData(site, self.expr, dest).toc()

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
    def as_expr(self, typ):
        return CompoundLiteral(self.site, self, typ)
    def assign_to(self, dest, typ):
        '''output C statements to assign an lvalue'''
        # (void *) cast to avoid GCC erroring if the target type is (partially)
        # const-qualified. See ExpressionInitializer.assign_to
        if isinstance(typ, TNamed):
            out('memcpy((void *)&%s, &(%s)%s, sizeof %s);\n' %
                (dest.read(), typ.declaration(''), self.read(),
                 dest.read()))
        elif isinstance(typ, TArray):
            out('memcpy((void *)%s, (%s)%s, sizeof %s);\n'
                % (dest.read(), typ.declaration(''),
                   self.read(), dest.read()))
        elif isinstance(typ, TStruct):
            out('memcpy((void *)&%s, (%s){%s}, sizeof %s);\n' % (
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
    def as_expr(self, typ):
        return CompoundLiteral(self.site, self, typ)
    def assign_to(self, dest, typ):
        '''output C statements to assign an lvalue'''
        typ = safe_realtype(typ)
        if isinstance(typ, StructType):
            # (void *) cast to avoid GCC erroring if the target type is
            # (partially) const-qualified. See ExpressionInitializer.assign_to
            out('memcpy((void *)&%s, (%s){%s}, sizeof %s);\n' % (
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
    def as_expr(self, typ):
        return CompoundLiteral(self.site, self, typ)
    def assign_to(self, dest, typ):
        '''output C statements to assign an lvalue'''
        assert isinstance(safe_realtype(typ),
                          (TExternStruct, TStruct, TArray, TEndianInt, TTrait,
                           THook))
        # (void *) cast to avoid GCC erroring if the target type is
        # (partially) const-qualified. See ExpressionInitializer.assign_to
        out('memset((void *)&%s, 0, sizeof(%s));\n'
            % (dest.read(), typ.declaration('')))

class CompoundLiteral(Expression):
    @auto_init
    def __init__(self, site, init, type):
        assert isinstance(init, (CompoundInitializer,
                                 DesignatedStructInitializer,
                                 MemsetInitializer))
    def __str__(self):
        return 'cast(%s, %s)' % (self.init, self.type)
    def read(self):
        return f'({self.type.declaration("")}) {self.init.read()}'

class StructDefinition(Statement):
    """A C struct definition appearing in a local scope, like
    'struct X { ... };'. This is used for anonymous structs; it is
    consistently referenced to as 'struct X', and the first reference
    is preceded by a StructDefinition."""
    @auto_init
    def __init__(self, site, structtype): pass
    def toc_stmt(self):
        raise ICE(self.site,
                  'StructDefinition.toc_stmt: Should never be called')
    def toc(self):
        self.structtype.resolve().print_struct_definition()
mkStructDefinition = StructDefinition

class Declaration(Statement):
    "A variable declaration"
    is_declaration = True
    def __init__(self, site, name, type, init = None, unused = False):
        assert site
        self.site = site
        self.name = name
        self.type = type
        if init:
            assert isinstance(init, Initializer)
        self.init = init

        if name.startswith("__"):
            assert unused == True
        self.unused = unused

    def toc_stmt(self):
        self.linemark()
        out('{\n')
        self.toc_inline()
        self.linemark()
        out('}\n')

    def toc(self):
        self.linemark()

        if (isinstance(self.init, MemsetInitializer)
            and not deep_const(self.type)):
            # ducks a potential GCC warning, and also serves to
            # zero-initialize VLAs
            self.type.print_declaration(self.name, unused = self.unused)
            site_linemark(self.init.site)
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

# FIXME(SIMICS-21719): symbol usage tracking is completely broken, and must
# be fixed (or rather, completely redone) for the sake of SIMICS-21585.
# Part of the requirements is that it must take into account things like
# constant folding and inline method calls (see test 1.4/methods/inline)
def sym_declaration(sym):
    assert not isinstance(sym, symtab.StaticSymbol)
    refcount = sym.refcount()

    # TODO: Variable declaration omission relies on the broken symbol usage
    # tracking. The only reason it (and so the larger refcount architecture)
    # has been kept is because proper impact analysis of its removal has not
    # been made. Any issues from it has historically been circumvented by
    # ad-hoc ensuring sym.stmt is True.
    if not sym.stmt and refcount == 0 and not possible_side_effect(sym.init):
        # dbg('ignoring %r (init = %r)' % (sym.value, sym.init))
        if sym.init:
            sym.init.decref()
        return None

    # This will prevent warnings from the C compiler
    # HACK: Always True to not rely on the broken symbol usage tracking
    #
    # unused = (refcount == 0) or sym.value.startswith("__")
    unused = True

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
            linemark(self.site.lineno, path)
        out(self.text)
        if not output.current().bol:
            out('\n')
        if dml.globals.linemarks:
            reset_line_directive()
        out('#undef %s\n' % (ident,))

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

def log_object(site, node, indices):
    return mkLit(site, crep.conf_object(site, node, indices),
                 TPtr(TNamed("conf_object_t")))

def log_statement(site, logobj, logtype, level, groups, fmt, *args):
    if logtype in {'warning', 'error', 'critical'}:
        lvl = []
    else:
        if level is None:
            level = mkIntegerLiteral(site, 1)
        lvl = [level]
    logfunc = "SIM_LOG_" + logtype.replace("-", "_").upper()
    if groups is None:
        groups = mkIntegerLiteral(site, 0)

    inargtypes = (([TInt(32, True)] if lvl else [])
                  + [TPtr(TNamed("conf_object_t")), TInt(64, False),
                     TPtr(TInt(8, True, const=True))])
    fun = mkLit(site, logfunc, TFunction(inargtypes, TVoid(), varargs=True))

    x = Apply(site, fun,
              lvl +
              [ logobj,
                groups,
                mkStringConstant(site, fmt) ] +
              list(args),
              fun.ctype())
    return mkExpressionStatement(site, x)
