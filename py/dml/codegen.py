# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import re
from abc import ABC, abstractmethod, abstractproperty
import operator
import contextlib

from functools import reduce
import itertools
import os
import math

from . import objects, crep, ctree, ast, int_register, logging, serialize
from . import dmlparse, output, compat
from .logging import *
from .expr import *
from .ctree import *
from .expr_util import *
from .symtab import *
from .messages import *
from .output import *
from .types import *
from .set import Set
from .slotsmeta import auto_init
import dml.globals

__all__ = (
    'mark_method_exported',
    'mark_method_referenced',
    'exported_methods',
    'statically_exported_methods',
    'method_queue',
    'saved_method_variables',
    'get_type_sequence_info',

    'eval_arraylen',
    'eval_type',
    'eval_method_inp',
    'eval_method_outp',
    'eval_initializer',
    'get_initializer',
    'codegen_expression',
    'codegen_expression_maybe_nonvalue',

    'NoFailure',
    'InitFailure',
    'LogFailure',
    'CatchFailure',
    'ReturnFailure',
    'IgnoreFailure',

    'c_rettype',
    'c_inargs',
    'method_instance',
    'require_fully_typed',
    'codegen_method_func',
    'codegen_method',
    'mkcall_method',
    'codegen_call',
    'codegen_call_byname',
    'codegen_call_expr',
    'codegen_call_traitmethod',
    'codegen_inline',
    'codegen_inline_byname',

    #'compound',

    'declarations',
)

class UnknownMethod(Exception):
    def __init__(self, obj, method):
        Exception.__init__(self)
        self.obj = obj
        self.method = method
    def __str__(self):
        return 'Unknown method in %s : %s' % (repr(self.obj), self.method)

gensym_counter = 0
def gensym(prefix = '_gensym'):
    global gensym_counter
    gensym_counter += 1
    return prefix + str(gensym_counter)

# Keep track of which methods are referenced, thus needing generated code.
referenced_methods = {}
method_queue = []
exported_methods = {}
statically_exported_methods = Set()

# Saved variables in methods, method->list of symbols
saved_method_variables = {}

class LoopContext:
    '''Class representing loop contexts within methods.
    LoopContext.current is the nearest enclosing loop context at any given
    point during code generation.'''
    current = None

    def __enter__(self):
        self.prev = LoopContext.current
        LoopContext.current = self
    def __exit__(self, exc_type, exc_val, exc_tb):
        assert LoopContext.current is self
        LoopContext.current = self.prev
    @abstractmethod
    def break_(self, site): pass
    def continue_(self, site):
        raise ECONTU(site)

class CLoopContext(LoopContext):
    '''DML loop context corresponding to a C loop'''
    def break_(self, site):
        return [mkBreak(site)]
    def continue_(self, site):
        return [mkContinue(site)]

class NoLoopContext(LoopContext):
    '''DML loop context corresponding to an inlined method call.
    This is used to delimit the loop stack between the method containing
    the inline method call and the method called.'''
    def break_(self, site):
        raise EBREAK(site)
    def continue_(self, site):
        raise ECONT(site)

class GotoLoopContext(LoopContext):
    '''DML loop context not directly corresponding to a single C loop
    statement. Uses of `break` is codegen:d as a goto past the loop.'''
    count = 0
    def __init__(self):
        self.used = False
        self.id = GotoLoopContext.count
        self.label = f'_abstractloopbreak{self.id}'
        GotoLoopContext.count += 1

    def break_(self, site):
        self.used = True
        return [mkGotoBreak(site, self.label)]

class ForeachSequenceLoopContext(GotoLoopContext):
    '''DML loop context corresponding to a foreach loop on an each-in sequence
    Uses of `break` is codegen:d as a goto past the outer loop, and `continue`
    is codegen:d as a `continue` of the inner loop'''
    def continue_(self, site):
        return [mkContinue(site)]

class Failure(ABC):
    '''Handle exceptions failure handling is supposed to handle the various kind of
    functions that are generated, with different ways of signaling
    success/failure.'''
    allowed = True
    fail_stack = []
    def __init__(self, site):
        self.site = site
    def __enter__(self):
        self.fail_stack.append(self)
    def __exit__(self, exc_type, exc_val, exc_tb):
        top = self.fail_stack.pop()
        assert top is self
    @abstractmethod
    def fail(self, site):
        '''Return code that is used to handle an exception'''

class NoFailure(Failure):
    '''Disallow exceptions to be thrown in compile time'''
    allowed = False
    def fail(self, site):
        raise ICE(site, "fail not allowed here")

class InitFailure(Failure):
    '''Returns NULL from init_object on exception'''
    def fail(self, site):
        return mkReturn(site, mkIntegerLiteral(site, 0))

class LogFailure(Failure):
    '''Log exceptions as errors, without aborting execution'''
    def __init__(self, site, node, indices):
        super(LogFailure, self).__init__(site)
        self.node = node
        self.indices = indices

    def fail(self, site):
        return log_statement(site, log_object(site, self.node, self.indices),
                             "error", mkIntegerLiteral(site, 1), None,
                             "Uncaught DML exception")

class ReturnFailure(Failure):
    '''Generate boolean return statements to signal success. False
    means success.'''
    def fail(self, site):
        return mkReturn(site, mkBoolConstant(site, True))
    def nofail(self):
        '''Return code that is used to leave the method successfully'''
        return mkReturn(self.site, mkBoolConstant(self.site, False))

class CatchFailure(Failure):
    '''Handle exceptions by re-throwing them, which in C means jumping to
    a label.'''
    def __init__(self, site, method_node):
        Failure.__init__(self, site)
        self.label = None
        self.method = method_node
    def fail(self, site):
        if not self.label:
            self.label = gensym('throw')
        return mkThrow(site, self.label)

class IgnoreFailure(Failure):
    '''Ignore exceptions'''
    def fail(self, site):
        return mkNull(site)

class ExitHandler(ABC):
    current = None

    def __enter__(self):
        self.prev = ExitHandler.current
        ExitHandler.current = self
    def __exit__(self, exc_type, exc_val, exc_tb):
        assert ExitHandler.current is self
        ExitHandler.current = self.prev

    @abstractmethod
    def codegen_exit(self, site, retvals):
        '''Return a statement for returning from the current method. retvals
        is None in DML 1.2, and a list of return values in DML 1.4.'''
        pass

def codegen_exit(site, retvals):
    return ExitHandler.current.codegen_exit(site, retvals)

class GotoExit(ExitHandler):
    count = 0
    def __init__(self):
        self.used = False
        GotoExit.count += 1
        self.label = 'exit%d' % (self.count,)

class GotoExit_dml12(GotoExit):
    def codegen_exit(self, site, retvals):
        assert retvals is None
        self.used = True
        return mkGoto(site, self.label)

class GotoExit_dml14(GotoExit):
    def __init__(self, outvars):
        self.outvars = outvars
        super(GotoExit_dml14, self).__init__()
    def codegen_exit(self, site, retvals):
        assert retvals is not None
        assert len(retvals) == len(self.outvars)
        self.used = True
        return mkCompound(
            site,
            [mkCopyData(site, val, out)
             for (out, val) in zip(self.outvars, retvals)]
            + [mkReturnFromInline(site, self.label)])

class ReturnExit(ExitHandler):
    def __init__(self, outp, throws):
        self.outp = outp
        self.throws = throws
    def codegen_exit(self, site, retvals):
        assert retvals is not None, 'dml 1.2/1.4 mixup'
        return codegen_return(site, self.outp, self.throws, retvals)

def memoized_return_failure_leave(site, make_ref, failed):
    ran = make_ref('ran', TInt(8, True))
    threw = make_ref('threw', TBool())
    stmts = [mkCopyData(site, mkIntegerLiteral(site, 1), ran),
             mkCopyData(site, mkBoolConstant(site, failed), threw),
             mkReturn(site, mkBoolConstant(site, failed))]
    return stmts

class MemoizedReturnFailure(Failure):
    '''Generate boolean return statements to signal success. False
    means success.'''

    def __init__(self, site, make_ref):
        self.site = site
        self.make_ref = make_ref

    def fail(self, site):
        return mkCompound(
            site,
            memoized_return_failure_leave(site, self.make_ref, True))
    def nofail(self):
        '''Return code that is used to leave the method successfully'''
        return mkCompound(
            self.site,
            memoized_return_failure_leave(self.site, self.make_ref, False))

class MemoizedReturnExit(ExitHandler):
    def __init__(self, site, outp, throws, make_ref):
        self.site = site
        self.outp = outp
        self.throws = throws
        self.make_ref = make_ref

    def codegen_exit(self, site, retvals):
        ran = self.make_ref('ran', TInt(8, True))
        stmts = [mkCopyData(site, mkIntegerLiteral(site, 1), ran)]
        if self.throws:
            threw = self.make_ref('threw', TBool())
            stmts.append(mkCopyData(site, mkBoolConstant(site, False), threw))
        targets = []
        for ((name, typ), val) in zip(self.outp, retvals):
            target = self.make_ref(f'p_{name}', typ)
            stmts.append(mkCopyData(site, val, target))
            targets.append(target)
        stmts.append(codegen_return(site, self.outp, self.throws, targets))
        return mkCompound(site, stmts)

class Memoization(ABC):
    @abstractmethod
    def prelude(self):
        pass

    @abstractmethod
    def exit_handler(self):
        pass

    @abstractmethod
    def fail_handler(self):
        pass

class IndependentMemoized(Memoization):
    def __init__(self, func):
        assert func.method.independent
        self.func = func
        self.method = func.method
        # SimpleSite wrapper to avoid linemarks being generated.
        self.site = SimpleSite(self.method.site.loc())

    def make_ref(self, ref, typ):
        indexing = ''.join([f'[_idx{i}]'
                            for i in range(self.method.dimensions)])
        return mkLit(self.site, f'_memo{indexing}.{ref}', typ)

    def prelude(self):
        struct_body = ''.join(
            f'{typ.declaration(name)}; '
            for (name, typ) in (
                    [('ran', TInt(8, True))]
                    + ([('threw', TBool())] if self.func.throws else [])
                    + [(f'p_{name}', typ) for (name, typ) in self.func.outp]))
        array_defs = ''.join([f'[{i}]' for i in self.method.dimsizes])
        struct_var_def = mkInline(
            self.site,
            f'static struct {{{struct_body}}} _memo{array_defs};')
        return [struct_var_def] + memoization_common_prelude(
            self.method.logname_anonymized(), self.site, self.func.outp,
            self.func.throws, self.make_ref)
    def exit_handler(self):
        return MemoizedReturnExit(self.method.site, self.func.outp,
                                  self.func.throws, self.make_ref)
    def fail_handler(self):
        return (MemoizedReturnFailure(self.method.rbrace_site, self.make_ref)
                if self.func.throws else NoFailure(self.method.site))

class SharedIndependentMemoized(Memoization):
    def __init__(self, method):
        assert method.independent
        self.method = method
        # SimpleSite wrapper to avoid linemarks being generated.
        self.site = SimpleSite(self.method.site.loc())
    def make_ref(self, ref, typ):
        traitname = cident(self.method.trait.name)
        return mkLit(self.site,
                     f'((struct _{traitname} *) _{traitname}.trait)'
                     + f'->_memo_outs_{self.method.name}'
                     + f'[_{traitname}.id.encoded_index].{ref}', typ)
    def prelude(self):
        return memoization_common_prelude(
            self.method.name, self.site, self.method.outp, self.method.throws,
            self.make_ref)
    def exit_handler(self):
        return MemoizedReturnExit(self.method.site, self.method.outp,
                                  self.method.throws, self.make_ref)
    def fail_handler(self):
        return (MemoizedReturnFailure(self.method.rbrace_site, self.make_ref)
                if self.method.throws else NoFailure(self.method.site))

def memoization_common_prelude(name, site, outp, throws, make_ref):
    has_run_stmts = []
    # Throwing is treated as a special kind of output parameter, stored through
    # 'threw'. When 'ran' indicates the method has been called before to
    # completion, 'threw' is retrieved to check whether the method threw or
    # not. If it did, then the call completes by throwing again; otherwise, the
    # cached return values are retrieved and returned.
    if throws:
        has_run_stmts.append(
            mkIf(site, make_ref('threw', TBool()),
                 mkReturn(site, mkBoolConstant(site, True))))
    has_run_stmts.append(
        codegen_return(site, outp, throws,
                       [make_ref(f'p_{pname}', ptype)
                        for (pname, ptype) in outp]))
    # 'ran' is used to check whether the function has been called or not:
    # - 0:  never been called before. Set to -1, and execute the body.
    #       Before any return, cache the results, and set 'ran' to 1.
    # - 1:  called to completion before. Fetch the cached results and return
    #       immediately without executing the rest of the body.
    # - -1: called before, but not to completion. This only happens due to an
    #       (indirect) recursive call. Raise critical error, and then proceed
    #       as though ran == 0. Hopefully things will turn out ok.
    ran = make_ref('ran', TInt(8, True))
    unrun   = [mkCase(site, mkIntegerLiteral(site, 0)),
               mkCopyData(site, mkIntegerConstant(site, -1, True), ran),
               mkBreak(site)]
    has_run = [mkCase(site, mkIntegerLiteral(site, 1))] + has_run_stmts
    running = [mkDefault(site),
               mkInline(site, f'_memoized_recursion("{name}");')]
    return [mkSwitch(site,
                     make_ref('ran', TInt(8, True)),
                     mkCompound(site, unrun + has_run + running))]

class TypeSequenceInfo:
    """Bookkeeping surrounding a realtyped unique type sequence"""
    def __init__(self, types, uniq):
        self.types = types
        self.uniq = uniq
        self.after_on_hooks = {}
        self.struct = (TStruct({f'comp{i}': typ
                                 for (i, typ) in enumerate(types)},
                                label=f'_typeseq_{self.uniq}')
                       if types else None)
        type_keys = ', '.join(f"'{typ.key()}'" for typ in types)
        self.string_key = f'({type_keys})'

    def get_after_on_hook(self, aoh_prim_key, param_to_msg_comp, no_params,
                          create_new=False):
        param_to_msg_comp_key = tuple(param_to_msg_comp.get(i)
                                      for i in range(no_params))
        try:
            return self.after_on_hooks[(aoh_prim_key, param_to_msg_comp_key)]
        except KeyError:
            if not create_new:
                raise
            info = after_on_hook_info_constructors[type(aoh_prim_key)](
                self, aoh_prim_key, param_to_msg_comp)
            self.after_on_hooks[(aoh_prim_key, param_to_msg_comp_key)] = info
            return info

def get_type_sequence_info(index, create_new=False):
    typeseq = TypeSequence(index)
    try:
        return dml.globals.type_sequence_infos[typeseq]
    except KeyError:
        if create_new:
            info = TypeSequenceInfo(typeseq.types,
                                    len(dml.globals.type_sequence_infos))
            dml.globals.type_sequence_infos[typeseq] = info
            return info
        else:
            return None

class AfterInfo(ABC):
    '''Information used to generate artifact corresponding to a unique usage of
    the after statement. After statements are considered unique in respect to
    the artifacts they need: two after statements that can share the same
    generated artifacts -- thus the same AfterInfo -- are considered identical
    usages of after.'''
    def __init__(self, key, dimsizes):
        self.key = key
        self.dimsizes = dimsizes

    @abstractproperty
    def cident_prefix(self):
        '''A prefix for C identifiers used for the naming of artifacts
        generated for the unique usage of 'after' '''

    @abstractproperty
    def args_type(self):
        '''A type that represents all information needed to execute the
        callback of the 'after', excluding indices and after domains.
        If no additional information is needed this should return None.
        '''

    @abstractproperty
    def types_to_declare(self):
        '''An iterable of the novel types used by the generated artifacts of
        the 'after', and which must be declared.'''

    @property
    def cident_callback(self):
        return self.cident_prefix + 'callback'

    @property
    def dimensions(self):
        return len(self.dimsizes)

class CheckpointedAfterInfo(AfterInfo):
    @abstractproperty
    def string_key(self):
        '''A key for the AfterInfo -- thus a key for the set of artifacts
        generated for the unique usage of 'after'. This must be suitable for
        use as a string in generated code -- in particular anonymization of
        confidential names must be done.'''

class AfterDelayInfo(CheckpointedAfterInfo):
    def __init__(self, key, dimsizes, uniq):
        self.uniq = uniq
        super().__init__(key, dimsizes)

    @abstractmethod
    def generate_callback_call(self, indices_lit, args_lit): pass

    @property
    def cident_prefix(self):
        return f'_simple_event_{self.uniq}_'

    @property
    def cident_get_value(self):
        if self.dimensions or self.args_type is not None:
            return self.cident_prefix + 'get_value'
        else:
            return '_simple_event_only_domains_get_value'

    @property
    def cident_set_value(self):
        if self.dimensions or self.args_type is not None:
            return self.cident_prefix + 'set_value'
        else:
            return '_simple_event_only_domains_set_value'


class AfterOnHookInfo(CheckpointedAfterInfo):
    def __init__(self, dimsizes, parent, typeseq_info, prim_key,
                 param_to_msg_comp, inp, has_serialized_args):
        self.typeseq_info = typeseq_info
        self.parent = parent
        # TODO drop?
        self.inp = inp
        self.has_serialized_args = has_serialized_args
        self.prim_key = prim_key
        self.param_to_msg_comp = param_to_msg_comp
        self.uniq = len(dml.globals.after_on_hook_infos)
        dml.globals.after_on_hook_infos.append(self)
        super().__init__((prim_key, self.param_to_msg_comp_key), dimsizes)

    @abstractmethod
    def generate_callback_call(self, indices_lit, args_lit, msg_lit): pass

    @abstractmethod
    def generate_args_serializer(self, site, args_expr, out_expr): pass

    @abstractmethod
    def generate_args_deserializer(self, site, val_expr, out_expr, error_out):
        pass

    @abstractproperty
    def string_prim_key(self):
        '''The AfterOnHookInfo key for the primary component -- the target
        callback -- of the hook-based after'''

    @property
    def param_to_msg_comp_key(self):
        return tuple(i if i in self.param_to_msg_comp else None
                     for i in range(len(self.inp)))

    @property
    def string_key(self):
        return str((self.string_prim_key, self.param_to_msg_comp_key))

    @property
    def cident_prefix(self):
        return f'_after_on_hook_{self.uniq}_'

    @property
    def cident_args_serializer(self):
        assert self.has_serialized_args
        return self.cident_prefix + 'args_serializer'

    @property
    def cident_args_deserializer(self):
        assert self.has_serialized_args
        return self.cident_prefix + 'args_deserializer'

class ImmediateAfterInfo(AfterInfo):
    def __init__(self, key, dimsizes, uniq):
        self.uniq = uniq
        super().__init__(key, dimsizes)

    @abstractmethod
    def generate_callback_call(self, indices_lit, args_lit): pass

    @property
    def cident_prefix(self):
        return f'_immediate_after_{self.uniq}_'

class AfterDelayIntoMethodInfo(AfterDelayInfo):
    def __init__(self, method, uniq):
        self.method = method
        super().__init__(method, method.dimsizes, uniq)
        self._args_type = (TStruct(dict(method.inp),
                                   label=f'_simple_event_{self.uniq}_args')
                           if method.inp else None)

    @property
    def args_type(self):
        return self._args_type

    @property
    def types_to_declare(self):
        return (self._args_type,) * (self._args_type is not None)

    @property
    def string_key(self):
        return self.method.logname_anonymized()

    def generate_callback_call(self, indices_lit, args_lit):
        site = self.method.site
        indices = tuple(mkLit(site, f'{indices_lit}[{i}]', TInt(32, False))
                        for i in range(self.method.dimensions))
        args = tuple(mkLit(site, f'{args_lit}->{pname}', ptype)
                     for (pname, ptype) in self.method.inp)
        with LogFailure(site, self.method, indices), \
             crep.DeviceInstanceContext():
            code = codegen_call(site, self.method, indices, args, ())
        code = mkCompound(site, [code])
        code.toc()

class AfterDelayIntoSendNowInfo(AfterDelayInfo):
    def __init__(self, typeseq_info, uniq):
        super().__init__(typeseq_info, [], uniq)
        self .typeseq_info = typeseq_info
        hookref_type = THook(typeseq_info.types, validated=True)
        self._args_type = (
            TStruct({'hookref': hookref_type,
                     'args': typeseq_info.struct},
                    label=f'_simple_event_{self.uniq}_args')
            if typeseq_info.types else hookref_type)

    @property
    def args_type(self):
        return self._args_type

    @property
    def types_to_declare(self):
        return (self._args_type,) * bool(self.typeseq_info.types)

    @property
    def string_key(self):
        return self.typeseq_info.string_key

    def generate_callback_call(self, indices_lit, args_lit):
        assert indices_lit is None
        has_args = bool(self.typeseq_info.types)
        hookref = f'{args_lit}->hookref' if has_args else f'*{args_lit}'
        args = f'&{args_lit}->args' if has_args else 'NULL'
        out('_DML_send_hook(&_dev->obj, &_dev->_detached_hook_queue_stack, '
            + f'_DML_resolve_hookref(_dev, _hook_aux_infos, {hookref}), '
            + f'{args});\n')

def get_after_delay(key):
    try:
        return dml.globals.after_delay_infos[key]
    except:
        uniq = len(dml.globals.after_delay_infos)
        info = after_delay_info_constructors[type(key)](key, uniq)
        dml.globals.after_delay_infos[key] = info
        return info


class AfterOnHookIntoMethodInfo(AfterOnHookInfo):
    def __init__(self, typeseq_info, method, param_to_msg_comp):
        self.method = method
        super().__init__(method.dimsizes, method.parent, typeseq_info, method,
                         param_to_msg_comp, method.inp, bool(self.method.inp))
        self._args_type = (
            TStruct({name: typ
                    for (i, (name, typ)) in enumerate(method.inp)
                     if i not in param_to_msg_comp},
                    label=f'_after_on_hook_{self.uniq}_args')
            if len(self.method.inp) > len(param_to_msg_comp) else None)

    def generate_callback_call(self, indices_lit, args_lit, msg_lit):
        site = self.method.site
        indices = tuple(mkLit(site, f'{indices_lit}[{i}]', TInt(32, False))
                        for i in range(self.method.dimensions))
        args = tuple(
            mkLit(site,
                  f'{msg_lit}->comp{self.param_to_msg_comp[i]}'
                  if i in self.param_to_msg_comp else f'{args_lit}->{pname}',
                  ptype)
            for (i, (pname, ptype)) in enumerate(self.method.inp))
        with LogFailure(site, self.method, indices), \
             crep.DeviceInstanceContext():
            code = codegen_call(site, self.method, indices, args, ())
        code = mkCompound(site, [code])
        code.toc()

    def generate_args_serializer(self, site, args_expr, out_expr):
        sources = tuple((ctree.mkSubRef(site, args_expr, name, "."),
                         safe_realtype(typ))
                        if i not in self.param_to_msg_comp else None
                        for (i, (name, typ)) in enumerate(self.method.inp))
        serialize.serialize_sources_to_list(site, sources, out_expr)

    def generate_args_deserializer(self, site, val_expr, out_expr, error_out):
        if self.args_type:
            tmp_out_decl, tmp_out_ref = serialize.declare_variable(
                site, '_tmp_out', self.args_type)
            tmp_out_decl.toc()
        else:
            tmp_out_ref = None
        targets = tuple((ctree.mkSubRef(site, tmp_out_ref, name, "."),
                         safe_realtype(typ))
                        if i not in self.param_to_msg_comp else None
                        for (i, (name, typ)) in enumerate(self.method.inp))

        def error_out_at_index(_i, exc, msg):
            return error_out(exc, msg)

        serialize.deserialize_list_to_targets(
            site, val_expr, targets, error_out_at_index,
            f'deserialization of arguments to {self.method.name}')
        if self.args_type:
            ctree.mkAssignStatement(site, out_expr,
                                    ctree.ExpressionInitializer(
                                        tmp_out_ref)).toc()

    @property
    def args_type(self):
        return self._args_type

    @property
    def types_to_declare(self):
        return (self._args_type,) * (self._args_type is not None)

    @property
    def string_prim_key(self):
        return self.method.logname_anonymized(("%u",) * self.method.dimensions)


class AfterOnHookIntoSendNowInfo(AfterOnHookInfo):
    def __init__(self, typeseq_info, sendnow_typeseq_info, param_to_msg_comp):
        self.sendnow_typeseq_info = sendnow_typeseq_info
        inp = [(f'comp{i}', typ)
               for (i, typ) in enumerate(sendnow_typeseq_info.types)]
        has_inner_args = len(inp) > len(param_to_msg_comp)
        super().__init__([], dml.globals.device, typeseq_info,
                         sendnow_typeseq_info, param_to_msg_comp, inp, True)
        sendnow_hookref_type = THook(sendnow_typeseq_info.types,
                                     validated=True)
        self.inner_args_type = (
            TStruct({name: typ
                    for (i, (name, typ)) in enumerate(inp)
                     if i not in param_to_msg_comp},
                    label=f'_after_on_hook_{self.uniq}_inner_args')
            if has_inner_args else None)
        self._args_type = (
            TStruct({'hookref': sendnow_hookref_type,
                     'args': self.inner_args_type},
                     label=f'_after_on_hook_{self.uniq}_args')
            if has_inner_args else sendnow_hookref_type)

    def generate_callback_call(self, indices_lit, args_lit, msg_lit):
        assert indices_lit is None
        has_inner_args = (len(self.sendnow_typeseq_info.types)
                          > len(self.param_to_msg_comp))
        hookref = f'{args_lit}->hookref' if has_inner_args else f'*{args_lit}'

        sendnow_msg_struct = self.sendnow_typeseq_info.struct
        args = (('&(%s_t) {%s}'
                % (sendnow_msg_struct.label,
                   ', '.join(
                       f'{msg_lit}->comp{self.param_to_msg_comp[i]}'
                       if i in self.param_to_msg_comp else
                       f'{args_lit}->args.comp{i}'
                       for i in range(len(self.sendnow_typeseq_info.types)))))
                if self.sendnow_typeseq_info.types else 'NULL')

        out('_DML_send_hook(&_dev->obj, &_dev->_detached_hook_queue_stack, '
            + f'_DML_resolve_hookref(_dev, _hook_aux_infos, {hookref}), '
            + f'{args});\n')

    def generate_args_serializer(self, site, args_expr, out_expr):
        has_inner_args = (len(self.sendnow_typeseq_info.types)
                          > len(self.param_to_msg_comp))
        hookref = (ctree.mkSubRef(site, args_expr, "hookref", ".")
                   if has_inner_args else args_expr)
        sources = [(hookref,
                    THook(self.sendnow_typeseq_info.types, validated=True))]
        if has_inner_args:
            inner_args_val_decl, inner_args_val = serialize.declare_variable(
                site, 'inner_args_val', TNamed('attr_value_t'))
            inner_args_val_decl.toc()
            inner_args = ctree.mkSubRef(site, args_expr, 'args', '.')
            inner_args_sources = (
                (ctree.mkSubRef(site, inner_args, f'comp{i}', '.'), typ)
                if i not in self.param_to_msg_comp else None
                for (i, typ) in enumerate(self.sendnow_typeseq_info.types))
            serialize.serialize_sources_to_list(site, inner_args_sources,
                                                inner_args_val)
            sources.append((inner_args_val, None))

        serialize.serialize_sources_to_list(site, sources, out_expr)

    def generate_args_deserializer(self, site, val_expr, out_expr, error_out):
        has_inner_args = (len(self.sendnow_typeseq_info.types)
                          > len(self.param_to_msg_comp))
        tmp_out_decl, tmp_out_ref = serialize.declare_variable(
            site, '_tmp_out', self.args_type)
        tmp_out_decl.toc()
        hookref = (ctree.mkSubRef(site, tmp_out_ref, 'hookref', '.')
                   if has_inner_args else tmp_out_ref)
        targets = [(hookref,
                    safe_realtype(THook(self.sendnow_typeseq_info.types,
                                        validated=True)))]
        if has_inner_args:
            inner_args_val_decl, inner_args_val = serialize.declare_variable(
                site, '_inner_args_val', TNamed('attr_value_t'))
            inner_args_val_decl.toc()
            targets.append((inner_args_val, None))


        def error_out_at_index(_i, exc, msg):
            return error_out(exc, msg)

        serialize.deserialize_list_to_targets(
            site, val_expr, targets, error_out_at_index,
            'deserialization of arguments to a send_now')

        if has_inner_args:
            inner_args = ctree.mkSubRef(site, tmp_out_ref, 'args', '.')
            inner_args_targets = (
                (ctree.mkSubRef(site, inner_args, f'comp{i}', '.'),
                 safe_realtype(typ))
                if i not in self.param_to_msg_comp else None
                for (i, typ) in enumerate(self.sendnow_typeseq_info.types))
            serialize.deserialize_list_to_targets(
                site, inner_args_val, inner_args_targets, error_out_at_index,
                'deserialization of arguments to a send_now')


        ctree.mkAssignStatement(site, out_expr,
                                ctree.ExpressionInitializer(tmp_out_ref)).toc()

    @property
    def args_type(self):
        return self._args_type

    @property
    def types_to_declare(self):
        return ((self.inner_args_type, self._args_type)
                * (self.inner_args_type is not None))
    @property
    def string_prim_key(self):
        return self.sendnow_typeseq_info.string_key

class ImmediateAfterIntoMethodInfo(ImmediateAfterInfo):
    def __init__(self, method, uniq):
        self.method = method
        super().__init__(method, method.dimsizes, uniq)
        self._args_type = (TStruct(dict(method.inp),
                                   label=f'_immediate_after_{self.uniq}_args')
                           if method.inp else None)

    @property
    def args_type(self):
        return self._args_type

    @property
    def types_to_declare(self):
        return (self._args_type,) * (self._args_type is not None)

    def generate_callback_call(self, indices_lit, args_lit):
        site = self.method.site
        indices = tuple(mkLit(site, f'{indices_lit}[{i}]', TInt(32, False))
                        for i in range(self.method.dimensions))
        args = tuple(mkLit(site, f'{args_lit}->{pname}', ptype)
                     for (pname, ptype) in self.method.inp)
        with LogFailure(site, self.method, indices), \
             crep.DeviceInstanceContext():
            code = codegen_call(site, self.method, indices, args, ())
        code = mkCompound(site, [code])
        code.toc()

class ImmediateAfterIntoSendNowInfo(ImmediateAfterInfo):
    def __init__(self, typeseq_info, uniq):
        super().__init__(typeseq_info, [], uniq)
        self.typeseq_info = typeseq_info
        hookref_type = THook(typeseq_info.types, validated=True)
        self._args_type = (
            TStruct({'hookref': hookref_type,
                     'args': typeseq_info.struct},
                    label=f'_immediate_after_{self.uniq}_args')
            if typeseq_info.types else hookref_type)

    @property
    def args_type(self):
        return self._args_type

    @property
    def types_to_declare(self):
        return (self._args_type,) * bool(self.typeseq_info.types)

    def generate_callback_call(self, indices_lit, args_lit):
        assert indices_lit is None
        has_args = bool(self.typeseq_info.types)
        hookref = f'{args_lit}->hookref' if has_args else f'*{args_lit}'
        args = f'&{args_lit}->args' if has_args else 'NULL'
        out('_DML_send_hook(&_dev->obj, &_dev->_detached_hook_queue_stack, '
            + f'_DML_resolve_hookref(_dev, _hook_aux_infos, {hookref}), '
            + f'{args});\n')

def get_immediate_after(key):
    try:
        return dml.globals.immediate_after_infos[key]
    except:
        uniq = len(dml.globals.immediate_after_infos)
        info = immediate_after_info_constructors[type(key)](key, uniq)
        dml.globals.immediate_after_infos[key] = info
        return info

after_delay_info_constructors = {
    objects.Method: AfterDelayIntoMethodInfo,
    TypeSequenceInfo: AfterDelayIntoSendNowInfo
}

after_on_hook_info_constructors = {
    objects.Method: AfterOnHookIntoMethodInfo,
    TypeSequenceInfo: AfterOnHookIntoSendNowInfo
}

immediate_after_info_constructors = {
    objects.Method: ImmediateAfterIntoMethodInfo,
    TypeSequenceInfo: ImmediateAfterIntoSendNowInfo
}

class AfterArgsInit:
    @abstractmethod
    def args_init(self): pass

class AfterIntoSendNowArgsInit(AfterArgsInit):
    def __init__(self, inargs, hookref):
        self.inargs = inargs
        self.hookref = hookref

    def args_init(self):
        if self.inargs:
            return ('{ %s, { %s } }'
                    % (self.hookref.read(),
                       ', '.join(inarg.read() for inarg in self.inargs)))
        else:
            return self.hookref.read()

class AfterIntoMethodArgsInit(AfterArgsInit):
    def __init__(self, inargs):
        self.inargs = inargs

    def args_init(self):
        assert self.inargs
        return f'{{ {", ".join(inarg.read() for inarg in self.inargs)} }}'

def declarations(scope):
    "Get all local declarations in a scope as a list of Declaration objects"
    decls = []
    for sym in scope.symbols():
        if sym.pseudo:
            # dbg("declarations(%s): skipping %r" % (scope.id, sym))
            continue
        if sym.stmt:
            continue
        decl = sym_declaration(sym)
        if decl:
            decls.append(decl)

    return decls

# Expression dispatch

expression_dispatcher = ast.astdispatcher('expr_')
codegen_expression_maybe_nonvalue = expression_dispatcher.dispatch
def codegen_expression(ast, location, scope):
    expr = codegen_expression_maybe_nonvalue(ast, location, scope)
    if isinstance(expr, NonValue):
        raise expr.exc()
    return expr

@expression_dispatcher
def expr_set(tree, location, scope):
    [target, source] = tree.args
    return mkAssignOp(tree.site,
                      codegen_expression(target, location, scope),
                      codegen_expression(source, location, scope))

@expression_dispatcher
def expr_conditional(tree, location, scope):
    [cond, texpr, fexpr] = tree.args
    cond = codegen_expression(cond, location, scope)
    if cond.constant and dml.globals.dml_version == (1, 2):
        # Constant propagate
        live_ast = texpr if cond.value else fexpr
        live_expr = codegen_expression_maybe_nonvalue(live_ast, location, scope)

        # Skip code generation for dead branch, but only in 1.2
        if (logging.show_porting
            and not tree.site.filename().endswith('dml-builtins.dml')):
            # If any branch contains an error or a non-value, then
            # it must be converted to '#?'.
            with logging.suppress_errors() as errors:
                dead_ast = fexpr if cond.value else texpr
                try:
                    codegen_expression(dead_ast, location, scope)
                except DMLError as e:
                    errors.append(e)
                if errors or isinstance(live_expr, NonValue):
                    report(PHASH(tree.site))
                    report(PHASHELSE(dmlparse.end_site(texpr.site), ':'))
        return live_expr
    return mkIfExpr(tree.site,
                    cond,
                    codegen_expression(texpr, location, scope),
                    codegen_expression(fexpr, location, scope))

@expression_dispatcher
def expr_hashcond(tree, location, scope):
    [cond, texpr, fexpr] = tree.args
    cond = as_bool(codegen_expression(cond, location, scope))
    if not cond.constant:
        raise ENCONST(tree.site, cond)
    live_ast = texpr if cond.value else fexpr
    return codegen_expression_maybe_nonvalue(live_ast, location, scope)

arith_binops = {
    '<':  mkLessThan,
    '<=': mkLessThanOrEquals,
    '>':  mkGreaterThan,
    '>=': mkGreaterThanOrEquals,
    '==': mkEquals,
    '!=': mkNotEquals,
    '&':  mkBitAnd,
    '|':  mkBitOr,
    '^':  mkBitXOr,
    '<<': mkShL,
    '>>': mkShR,
    '*':  mkMult,
    '/':  mkDiv,
    '%':  mkMod,
    '+':  mkAdd,
    '-':  mkSubtract,
    '&&': mkAnd,
    '||': mkOr,
}

@expression_dispatcher
def expr_binop(tree, location, scope):
    [lh, op, rh] = tree.args
    if op not in arith_binops:
        raise ICE(tree.site, 'Unknown binary operation: %s %s %s'
                  % (repr(lh), repr(op), repr(rh)))
    lh = codegen_expression(lh, location, scope)

    if op in {'&&', '||'}:
        lh = as_bool(lh)
        if lh.constant and bool(lh.value) == (op == '||'):
            if tree.site.dml_version() == (1, 2):
                if logging.show_porting:
                    # if RH contains errors, we must convert it to #? #:
                    with logging.suppress_errors() as errors:
                        as_bool(codegen_expression(rh, location, scope))
                    if errors:
                        if op == '||':
                            report(PANDOR(tree.site, dmlparse.start_site(tree.site), dmlparse.end_site(tree.site), '||', '#? true #:', ''))
                        else:
                            report(PANDOR(tree.site, dmlparse.start_site(tree.site), dmlparse.end_site(tree.site), '&&', '#?', ' #: false'))
            else:
                as_bool(codegen_expression(rh, location, scope))
            return lh
        rh = as_bool(codegen_expression(rh, location, scope))
    else:
        rh = codegen_expression(rh, location, scope)
    return arith_binops[op](tree.site, lh, rh)

def codegen_sizeof(site, expr):
    fun = mkLit(site, 'sizeof',
                TFunction([], TNamed('size_t'),
                          varargs = True))
    return Apply(site, fun, [expr], fun.ctype())

def flatten(x):
    '''Recursively flatten lists and tuples'''
    return ([item for y in x for item in flatten(y)]
            if isinstance(x, (list, tuple)) and not isinstance(x, ast.AST)
            else [x])

def subast_has_dollar(expr_ast):
    if expr_ast.kind == 'objectref':
        return True
    else:
        return any(subast_has_dollar(sub) for sub in flatten(expr_ast.args)
                   if isinstance(sub, ast.AST))

@expression_dispatcher
def expr_unop(tree, location, scope):
    [op, rh_ast] = tree.args
    if (compat.dml12_misc in dml.globals.enabled_compat
        and op == 'sizeof' and rh_ast.kind == 'variable_dml12'):
        var = rh_ast.args[0]
        if var in typedefs and scope.lookup(var) is None:
            report(WSIZEOFTYPE(tree.site))
            return codegen_sizeof(
                tree.site, mkLit(tree.site, cident(var), None))
    try:
        rh = codegen_expression_maybe_nonvalue(rh_ast, location, scope)
    except EIDENT as e:
        if op == 'sizeof':
            is_primitive_type = not isinstance(parse_type(e.identifier),
                                               TNamed)
            if is_primitive_type or e.identifier in typedefs:
                raise EIDENTSIZEOF(e.site, e.identifier)
        raise

    if isinstance(rh, NonValue):
        if op == 'defined':
            if undefined(rh):
                return mkBoolConstant(tree.site, False)
            if isinstance(rh, (NodeRef, NonValueArrayRef, AbstractList)):
                return mkBoolConstant(tree.site, True)
        if op == '!' and isinstance(rh, InterfaceMethodRef):
            # see SIMICS-9868
            return mkNot(tree.site, mkMethodPresent(tree.site, rh))
        if (op == '&' and isinstance(rh, NodeRef)
            and tree.site.dml_version() != (1, 2)):
            (method, indices) = rh.get_ref()
            if method.objtype == 'method':
                if (indices or not method.fully_typed or method.throws
                    or len(method.outp) > 1):
                    raise ESTATICEXPORT(method.site, tree.site)
                else:
                    func = method_instance(method)
                    mark_method_referenced(func)
                    if not func.independent:
                        mark_method_statically_exported(func)
                    return ctree.AddressOfMethod(tree.site, func)
        raise rh.exc()
    if   op == '!':
        if compat.dml12_not in dml.globals.enabled_compat:
            t = rh.ctype()
            if isinstance(safe_realtype(t), TInt) and subast_has_dollar(rh_ast):
                # A previous bug caused DMLC to permit expressions on
                # the form '!$reg'. This pattern was fairly common;
                # this hack is an attempt to reduce the short-term
                # need to update existing models. See also bug 24248.
                if logging.show_porting:
                    # triggers PBITNEQ
                    as_bool(rh)
                return mkEquals(tree.site, rh, mkIntegerLiteral(tree.site, 0))
        return mkNot(tree.site, as_bool(rh))
    elif op == '~':  return mkBitNot(tree.site, rh)
    elif op == '-':  return mkUnaryMinus(tree.site, rh)
    elif op == '+':  return mkUnaryPlus(tree.site, rh)
    elif op == '&':  return mkAddressOf(tree.site, rh)
    elif op == '*':  return mkDereference(tree.site, rh)
    elif op == '++':  return mkPreInc(tree.site, rh)
    elif op == '--':  return mkPreDec(tree.site, rh)
    elif op == 'post++':  return mkPostInc(tree.site, rh)
    elif op == 'post--':  return mkPostDec(tree.site, rh)
    elif op == 'sizeof':
        if (compat.dml12_misc not in dml.globals.enabled_compat
            and not isinstance(rh, ctree.LValue)):
            raise ERVAL(rh.site, 'sizeof')
        return codegen_sizeof(tree.site, rh)
    elif op == 'defined': return mkBoolConstant(tree.site, True)
    elif op == 'stringify':
        if not rh.constant:
            raise ENCONST(rh, rh)
        return mkStringConstant(tree.site, str(rh))
    else:
        raise Exception('Unknown unary operation: %s %s'
                        % (repr(op), repr(rh)))

@expression_dispatcher
def expr_typeop(tree, location, scope):
    [t] = tree.args
    (struct_defs, t) = eval_type(t, tree.site, location, scope)
    for (site, _) in struct_defs:
        report(EANONSTRUCT(site, "'sizeoftype' expression"))
    return codegen_sizeof(tree.site, mkLit(tree.site, t.declaration(''), None))

@expression_dispatcher
def expr_new(tree, location, scope):
    [t, count] = tree.args
    (struct_defs, t) = eval_type(t, tree.site, location, scope)
    for (site, _) in struct_defs:
        report(EANONSTRUCT(site, "'new' expression"))
    if count:
        count = codegen_expression(count, location, scope)
    return mkNew(tree.site, t, count)

@expression_dispatcher
def expr_apply(tree, location, scope):
    [fun, arg_inits] = tree.args
    fun = codegen_expression_maybe_nonvalue(fun, location, scope)
    # will report errors for non-callable non-values
    return fun.apply(arg_inits, location, scope)

@expression_dispatcher
def expr_variable_dml12(tree, location, scope):
    [name] = tree.args
    e = lookup_var(tree.site, scope, name)
    if e is None:
        raise EIDENT(tree.site, name)
    return e

@expression_dispatcher
def expr_variable(tree, location, scope):
    [name] = tree.args
    e = lookup_var(tree.site, scope, name)
    if scope.lookup(name) is global_scope.lookup(name) and location:
        # Hack: Object hierarchy is shoehorned between global scope and any
        # local scope
        # TODO: we should move symbols from global scope into device
        # scope instead. And location + scope args should be unified.
        in_dev_tree = ctree.lookup_component(
            tree.site, location.node, location.indices, name, False)
        if in_dev_tree:
            e = in_dev_tree
    if e is None:
        raise EIDENT(tree.site, name)
    return e

@expression_dispatcher
def expr_objectref(tree, location, scope):
    [name] = tree.args
    if not location:
        # This happens when invoked from mkglobals
        raise ENCONST(tree.site, dollar(tree.site)+name)
    e = ctree.lookup_component(
        tree.site, location.node, location.indices, name, False)
    if not e:
        raise EREF(tree.site, name)
    assert dml.globals.dml_version == (1, 2)
    if logging.show_porting:
        if (scope.lookup(name)
            and scope.lookup(name) != global_scope.lookup(name)):
            this = location.node
            if this.objtype == 'method':
                this = this.parent
            node = this
            while not node.get_component(name):
                node = node.parent
                assert node
            if not node.parent:
                prefix = 'dev.'
            elif node is this:
                prefix = 'this.'
            elif node.objtype == 'bank' and not scope.lookup('bank'):
                prefix = 'bank.'
            else:
                prefix = 'dev.%s.' % (node.logname(
                        tuple(e.read() for e in location.indices)),)
            if not tree.site.filename().endswith('dml-builtins.dml'):
                report(PDOLLAR_QUALIFY(
                    dmlparse.start_site(tree.site), '', prefix))
    return e

@expression_dispatcher
def expr_member(tree, location, scope):
    [lh, op, rh] = tree.args
    lh = codegen_expression_maybe_nonvalue(lh, location, scope)

    return mkSubRef(tree.site, lh, rh, op)

@expression_dispatcher
def expr_string(tree, location, scope):
    [val] = tree.args
    return mkStringConstant(tree.site, val)

@expression_dispatcher
def expr_int(tree, location, scope):
    [val] = tree.args
    return mkIntegerLiteral(tree.site, val)

@expression_dispatcher
def expr_float(tree, location, scope):
    [val] = tree.args
    return mkFloatConstant(tree.site, val)

@expression_dispatcher
def expr_index(tree, location, scope):
    [expr, index, bitorder] = tree.args
    expr = codegen_expression_maybe_nonvalue(expr, location, scope)
    index = codegen_expression_maybe_nonvalue(index, location, scope)
    return mkIndex(tree.site, expr, index)

@expression_dispatcher
def expr_slice(tree, location, scope):
    [expr, msb, lsb, bitorder] = tree.args
    expr = codegen_expression(expr, location, scope)
    msb = codegen_expression(msb, location, scope)
    if lsb is not None:
        lsb = codegen_expression(lsb, location, scope)
    return mkBitSlice(tree.site, expr, msb, lsb, bitorder)

@expression_dispatcher
def expr_list(tree, location, scope):
    [elts] = tree.args
    values = [codegen_expression_maybe_nonvalue(elt, location, scope)
              for elt in elts]
    return mkList(tree.site, values)

@expression_dispatcher
def expr_cast(tree, location, scope):
    [expr_ast, casttype] = tree.args
    expr = codegen_expression_maybe_nonvalue(expr_ast, location, scope)
    (struct_defs, type) = eval_type(casttype, tree.site, location, scope)
    for (site, _) in struct_defs:
        report(EANONSTRUCT(site, "'cast' expression"))

    return mkCast(tree.site, expr, type)

@expression_dispatcher
def expr_undefined(tree, location, scope):
    return mkUndefined(tree.site)

percent_matcher = re.compile("%")

fmt_matcher = re.compile(r"""
    %
    (?P<flags>      [-#0 +'I]*)
    (?P<width>      [1-9][0-9]*|\*|)
    (?P<precision>  \.([0-9]+|\*)|)
    (?P<length>     (h|H|ll?|L|q|j|Z|z|R|L|P|B(8|16|32|64)*|))
    (?P<conversion> [boudipxXscaAeEfgG%])
    """, re.X)

# Make the printf directives match argument sizes and inline some constant
def fix_printf(fmt, args, argsites, site):
    filtered_fmt = ""
    filtered_args = []
    argi = 0

    last_end = 0
    while True:
        m = percent_matcher.search(fmt, last_end)
        if not m:
            filtered_fmt += fmt[last_end:]
            break

        start = m.start()
        m = fmt_matcher.match(fmt, start)
        if not m:
            raise EFORMAT(site, start+1)

        filtered_fmt += fmt[last_end:m.start()]
        last_end = m.end()

        flags      = m.group('flags')
        width      = m.group('width')
        precision  = m.group('precision')
        length     = m.group('length')
        conversion = m.group('conversion')

        if conversion == '%':
            # printf allows flags and stuff here, but ignores it, but
            # let's copy it just in case.
            filtered_fmt += ("%" + flags + width + precision + length
                             + conversion)
            continue

        if argi == len(args):
            raise EFMTARGN(site)

        if width == '*':
            filtered_args.append(mkCast(args[argi].site,
                                        ctree.as_int(args[argi]),
                                        TInt(32, True)))
            argi += 1

        if precision == '.*':
            filtered_args.append(ctree.as_int(args[argi]))
            argi += 1

        arg = args[argi]
        if conversion in "boudixX":
            # GCC emits warnings about ll vs l mismatches, even
            # though ll and l are both 64-bit
            # integers. Unfortunately, DMLC does not know the
            # difference between 'long' and 'long long'; uint64 is
            # long long while e.g. size_t is long on linux64. For
            # purposes of logging, it is a sufficient workaround
            # to unconditionally cast to long long.
            length = "ll"
            arg = mkCast(arg.site, as_int(args[argi]), TInt(64, False))

        elif conversion in "p":
            argtype = safe_realtype(arg.ctype())

            if not isinstance(argtype, TPtr):
                raise EFMTARGT(argsites[argi], arg,
                                argi+1, "pointer")

        elif conversion == 's':
            argtype = arg.ctype()
            if isinstance(arg, (QName, HiddenName, HiddenQName)):
                qfmt, qargs = arg.fmt()
                filtered_fmt += qfmt
                assumed_type = TInt(32, False)
                for qarg in qargs:
                    filtered_args.append(mkCast(qarg.site, qarg, assumed_type))
                argi += 1
                continue
            elif isinstance(argtype, TNamed) and argtype.c == 'strbuf_t':
                arg = mkApply(site,
                              mkLit(site, 'sb_str',
                                    TFunction([TPtr(argtype)],
                                              TPtr(TNamed('char',
                                                          const=True)))),
                              [mkAddressOf(site, arg)])

        filtered_fmt += "%" + flags + width + precision + length + conversion
        filtered_args.append(arg)
        argi += 1

    if argi < len(args):
        raise EFMTARGN(site)

    return filtered_fmt, filtered_args

def eval_arraylen(size_ast, parent_scope):
    size = codegen_expression(size_ast, parent_scope, global_scope)
    if not size.constant:
        raise EASZVAR(size.site, size)
    if not isinstance(size.value, int):
        report(EBTYPE(size.site, size, "integer"))
        return 2  # arbitrary nonzero integer
    if size.value < 1:
        raise EASZR(size.site)
    return size.value

def eval_type(asttype, site, location, scope, extern=False, typename=None,
              allow_void=False):
    '''Interpret a type AST.
    The return value is a pair (struct_defs, type), where type is the DMLType
    instance, and struct_defs is a list of StructType statements required by
    C to interpret a declaration that uses the type.
    'extern' is true inside 'extern typedef' declarations.
    'typename' is used as hint for a good struct label, e.g.
    typedef struct { ... } foo_t; gives the label 'foo_t' which allows
    nicer error messages'''
    assert location is None or isinstance(location, Location)

    assert asttype

    struct_defs = []
    etype = None
    if isinstance(asttype[0], tuple):
        tag, info = asttype[0]
        if tag == 'struct':
            members = {}
            for (_, msite, name, type_ast) in info:
                (member_struct_defs, member_type) = eval_type(
                    type_ast, msite, location, scope, extern)
                if isinstance(member_type, TFunction):
                    if (compat.function_in_extern_struct
                        in dml.globals.enabled_compat
                        and extern):
                        member_type = TPtr(member_type)
                    else:
                        raise EFUNSTRUCT(msite)
                members[name] = member_type
                struct_defs.extend(member_struct_defs)
            if extern:
                id = typename or TExternStruct.unique_id()
                etype = TExternStruct(members, id, typename=typename)
            elif members:
                etype = TStruct(members, label=typename)
                struct_defs.append((site, etype))
            else:
                if site.dml_version() == (1, 2):
                    etype = TVoid()
                else:
                    raise EEMPTYSTRUCT(site)
        elif tag == 'layout':
            if extern:
                raise ELAYOUT(site, "extern layout not permitted,"
                              + " use 'struct { }' instead")
            endian, fields = info
            members = {}
            for (_, msite, name, type_ast) in fields:
                (member_struct_defs, member_type) = eval_type(
                    type_ast, msite, location, scope, False)
                if isinstance(member_type, TFunction):
                    raise EFUNSTRUCT(msite)
                members[name] = (msite, member_type)
                struct_defs.extend(member_struct_defs)
            if not members:
                raise EEMPTYSTRUCT(site)
            etype = TLayout(endian, members, label=typename)
            struct_defs.append((site, etype))
        elif tag == 'bitfields':
            width, fields = info
            if width > 64:
                raise EBFLD(site, "bitfields width is > 64")
            members = {}
            for ((_, fsite, name, t), astmsb, astlsb) in fields:
                msb = expr_intval(codegen_expression(astmsb, location, scope))

                lsb = expr_intval(codegen_expression(astlsb, location, scope))

                (member_struct_defs, mtype) = eval_type(
                    t, site, location, scope, extern)
                # guaranteed by parser
                assert not member_struct_defs
                if not mtype.is_int:
                    raise EBFLD(fsite, "non-integer field")
                if mtype.bits != msb - lsb + 1:
                    raise EBFLD(fsite, "field %s has wrong size" % name)

                members[name] = (mtype, msb, lsb)
            etype = TInt(width, False, members, label=typename)
        elif tag == 'typeof':
            expr = codegen_expression_maybe_nonvalue(info, location, scope)
            if isinstance(expr, NonValue):
                # for compatibility with dml-builtins, using 1.2
                if isinstance(expr, (ctree.NoallocNodeRef,
                                     ctree.RegisterWithFields,
                                     ctree.IncompleteNodeRefWithStorage)):
                    etype = expr.node_type
                else:
                    raise expr.exc()
            elif (not isinstance(expr, ctree.LValue)
                  and compat.dml12_misc not in dml.globals.enabled_compat):
                raise ERVAL(expr.site, 'typeof')
            else:
                etype = expr.ctype().clone()
            if not etype:
                raise ICE(site, "No type for expression: %s (%r)"
                           % (expr, expr))
        elif tag == 'sequence':
            etype = TTraitList(info)
        elif tag == 'hook':
            msg_comp_types = []
            for (_, tsite, _, type_ast) in info:
                (msg_comp_struct_defs, msg_comp_type) = eval_type(
                    type_ast, tsite, location, scope, extern)
                msg_comp_types.append(msg_comp_type)
                struct_defs.extend(msg_comp_struct_defs)
            etype = THook(msg_comp_types)
        else:
            raise ICE(site, "Strange type")
    elif isinstance(asttype[0], str):
        etype = parse_type(asttype[0])
    else:
        raise ICE(site, "Stranger type")

    etype.declaration_site = site

    asttype = asttype[1:]
    while asttype:
        if asttype[0] == 'const':
            if isinstance(etype, TFunction):
                raise ECONSTFUN(site)
            etype.const = True
            asttype = asttype[1:]
        elif asttype[0] == 'pointer':
            if (etype.is_int
                and not etype.is_endian
                and etype.bits not in {8, 16, 32, 64}):
                raise EINTPTRTYPE(site, TPtr(etype))
            etype = TPtr(etype)
            asttype = asttype[1:]
        elif asttype[0] == 'vect':
            if etype.void:
                raise EVOID(site)
            etype = TVector(etype)
            asttype = asttype[1:]
        elif asttype[0] == 'array':
            if etype.void:
                raise EVOID(site)
            if isinstance(etype, TFunction):
                raise EFUNARRAY(site)
            alen = codegen_expression(asttype[1], location, scope)
            etype = TArray(etype, as_int(alen))
            asttype = asttype[2:]
        elif asttype[0] == 'funcall':
            if struct_defs:
                (site, _) = struct_defs[0]
                raise EANONSTRUCT(site, "function return type")

            arg_struct_defs = []
            inarg_asts = asttype[1]
            if inarg_asts and inarg_asts[-1] == '...':
                varargs = True
                inarg_asts = inarg_asts[:-1]
            else:
                varargs = False
            inargs = []
            for (_, tsite, name, type_ast) in inarg_asts:
                (arg_struct_defs, argt) = eval_type(
                    type_ast, tsite, location, scope, arg_struct_defs, extern,
                    allow_void=True)
                if arg_struct_defs:
                    (site, _) = arg_struct_defs[0]
                    raise EANONSTRUCT(site, "function argument type")
                if argt.void:
                    if len(inarg_asts) == 1 and not name:
                        # C compatibility
                        continue
                    else:
                        raise EVOID(tsite)
                inargs.append(argt)

            # Function parameters that are declared as arrays are
            # interpreted as pointers
            for i, arg in enumerate(inargs):
                if isinstance(arg, TArray):
                    # C99 has a syntax for specifying that the array
                    # should be converted to a const pointer, but DML
                    # doesn't support that syntax.
                    inargs[i] = TPtr(arg.base, False)
                if arg.is_int and arg.is_endian:
                    raise EEARG(site)

            if etype.is_int and etype.is_endian:
                raise EEARG(site)
            etype = TFunction(inargs, etype, varargs)
            asttype = asttype[2:]
        else:
            raise ICE(site, "weird type info: " + repr(asttype))

        etype.declaration_site = site

    if etype.void and not allow_void:
        raise EVOID(site)

    return (struct_defs, etype)

def eval_method_inp(inp_asts, location, scope):
    '''evaluate the inarg ASTs of a method declaration'''
    inp = []
    for (_, tsite, argname, type_ast) in inp_asts:
        if type_ast:
            (struct_defs, t) = eval_type(type_ast, tsite, location, scope)
            for (site, _) in struct_defs:
                report(EANONSTRUCT(site, "method argument"))
        else:
            t = None
        inp.append((argname, t))
    return inp

def eval_method_outp(outp_asts, location, scope):
    '''evaluate the outarg ASTs of a method declaration'''
    if not outp_asts:
        return []
    outp = []
    if outp_asts[0].site.dml_version() == (1, 2):
        for (_, tsite, argname, type_ast) in outp_asts:
            if type_ast:
                (struct_defs, t) = eval_type(type_ast, tsite, location, scope)
                for (site, _) in struct_defs:
                    report(EANONSTRUCT(site, "method out argument"))
            else:
                t = None
            outp.append((argname, t))
    else:
        for (i, (_, tsite, _, type_ast)) in enumerate(outp_asts):
            assert type_ast
            (struct_defs, t) = eval_type(type_ast, tsite, location, scope)
            for (site, _) in struct_defs:
                report(EANONSTRUCT(site, "method return type"))
            # In 1.4, output arguments are not user-visible, but _outN is used
            # by the generated C function if needed.
            outp.append(('_out%d' % (i,), t))
    return outp

def check_designated_initializers(site, etype, init_asts, allow_partial):
    shallow_real_etype = safe_realtype_shallow(etype)
    duplicates = set()
    bad_fields = set()
    remaining = set(shallow_real_etype.members)
    for (field, init) in init_asts:
        if field not in shallow_real_etype.members:
            bad_fields.add(field)
        elif field not in remaining:
            duplicates.add(field)
        else:
            remaining.remove(field)
    if (duplicates or bad_fields or (not allow_partial and remaining)):
        raise EDATAINIT(site,
                        ''.join("\nduplicate initializer for "
                                + f"member '{field}'"
                                for field in duplicates)
                        + ''.join(f"\n'{field}' is not a member of "
                                  + f"{etype}"
                                  for field in bad_fields)
                        + ("\nmissing initializer(s) for member(s) "
                           + ", ".join(f"'{field}'"
                                       for field in remaining)
                           + "\n(partial initialization must be explicit "
                           + "through trailing '...', e.g. '{.a = a, ...}')"
                           if not allow_partial and remaining else '')
                        )

def loss_on_truncation(v, bits, signed):
    if not math.isfinite(v) or int(v) != v:
        return True
    # Special-case: casting negative floats to unsigned target is UB
    if not signed and isinstance(v, float) and v < 0:
        return True
    v = int(v)
    if signed:
        if bits == 0:
            # Only accept 0 for assignments to a theoretical int0
            return v != 0
        # If the bit width is == 1, bump up the upper range by one.
        # This allows assignments of 1 to int1 without indicating
        # truncation loss.
        return not ((-1 << (bits - 1)) <= v < (1 << (bits - 1)) + (bits == 1))
    else:
        # loss_on_truncation is more lenient with unsigned targets: it's
        # designed to permit bitwise not of any integer within the
        # representable range. E.g. for uint8 the min permitted constant
        # without indicating loss on truncation is ~255 == -256.
        return not ((-1 << bits) <= v < (1 << bits))

def mk_bitfield_compound_initializer_expr(site, etype, inits, location, scope,
                                          static):
    bitfields_expr = mkIntegerLiteral(site, 0)
    for ((ft, msb, lsb), e) in inits:
        real_ft = safe_realtype(ft)

        if e.kind == 'initializer_scalar':
            e = e.args[0]
            og_expr = codegen_expression(e, location, scope)
            if static and not og_expr.constant:
                raise EDATAINIT(e.site, 'non-constant expression')
            expr = source_for_assignment(e.site, ft, og_expr)

            # TODO this warning and check could be generalized and moved to
            # 'source_for_assignment' -- however, currently
            # 'source_for_assignment' has no clearly defined semantics and is
            # used all over the place, leading to unexpected WASTRUNCs in
            # certain corner cases.
            # Once these issues with 'source_for_assignment' have been
            # addressed, this warning and check should be moved there.
            if (isinstance(og_expr, (FloatConstant, IntegerConstant))
                and loss_on_truncation(og_expr.value, msb - lsb + 1,
                                       real_ft.signed)):
                report(WASTRUNC(e.site, ft))
        else:
            if not real_ft.is_bitfields:
                designated = e.kind == 'initializer_designated_struct'
                raise EDATAINIT(site,
                                f'{"designated " * designated}compound '
                                + 'initializer not supported for type '
                                + str(ft))
            init_asts = e.args[0]
            if e.kind == 'initializer_compound':
                if len(real_ft.members) != len(init_asts):
                    raise EDATAINIT(e.site, 'mismatched number of fields')
                inits = zip((t[1:] for t in real_ft.members_qualified),
                            init_asts)
            else:
                assert e.kind == 'initializer_designated_struct'
                check_designated_initializers(e.site, ft, init_asts, e.args[1])
                inits = ((real_ft.get_member_qualified(field), init)
                         for (field, init) in init_asts)

            expr = mk_bitfield_compound_initializer_expr(
                e.site, ft, inits, location, scope, static)

        # source_for_assignment doesn't always truncate when compat_dml12_int
        # is in play
        if dml.globals.compat_dml12_int(e.site):
            expr = mkCast(e.site, expr, ft)

        shifted = mkShL(e.site, expr, mkIntegerLiteral(e.site, lsb))
        bitfields_expr = mkBitOr(e.site, bitfields_expr, shifted)

    return source_for_assignment(site, etype, bitfields_expr)

def eval_initializer(site, etype, astinit, location, scope, static):
    """Deconstruct an AST for an initializer, and return a
       corresponding initializer object. Report EDATAINIT errors upon
       invalid initializers.
       
       Initializers are required to be constant for data objects and
       static variables. Local variables can be initialized with
       non-constant expressions. However, initializers for local
       variables of struct or bitfield types, in the form of
       brace-enclosed lists, are required to be constant expressions
       member-wise. Also, variable length arrays cannot have
       initializers. These rules match closely with the C language,
       except that array size must be explicitly specified and the
       number of initializers must match the number of elements of
       compound data types."""
    def do_eval(etype, astinit):
        shallow_real_etype = safe_realtype_shallow(etype)
        if astinit.kind == 'initializer_scalar':
            expr = codegen_expression(astinit.args[0], location, scope)
            if static and not expr.constant:
                raise EDATAINIT(astinit.site, 'non-constant expression')

            return ExpressionInitializer(
                source_for_assignment(astinit.site, etype, expr))
        elif astinit.kind == 'initializer_designated_struct':
            (init_asts, allow_partial) = astinit.args
            if isinstance(shallow_real_etype, StructType):
                check_designated_initializers(astinit.site, etype, init_asts,
                                              allow_partial)
                inits = {field: do_eval(shallow_real_etype
                                        .get_member_qualified(field),
                                        init)
                         for (field, init) in init_asts}
                return DesignatedStructInitializer(site, inits)
            elif shallow_real_etype.is_int and shallow_real_etype.is_bitfields:
                check_designated_initializers(astinit.site, etype, init_asts,
                                              allow_partial)
                return ExpressionInitializer(
                    mk_bitfield_compound_initializer_expr(
                        astinit.site, etype,
                        ((shallow_real_etype.get_member_qualified(field), init)
                         for (field, init) in init_asts),
                        location, scope, static))
            else:
                raise EDATAINIT(site,
                                'designated compound initializer not '
                                + f'supported for type {etype}')

        assert astinit.kind == 'initializer_compound'
        init_asts = astinit.args[0]
        if isinstance(etype, TArray):
            assert isinstance(etype.size, Expression)
            if etype.size.constant:
                alen = etype.size.value
            else:
                raise EDATAINIT(site, 'variable length array')
            if alen != len(init_asts):
                raise EDATAINIT(site, 'mismatched array size')
            init = tuple(do_eval(etype.base, e) for e in init_asts)
            return CompoundInitializer(site, init)
        elif isinstance(etype, TStruct):
            if len(etype.members) != len(init_asts):
                raise EDATAINIT(site, 'mismatched number of fields')
            init = tuple(do_eval(mt, e)
                         for ((_, mt), e) in zip(etype.members_qualified,
                                                 init_asts))
            return CompoundInitializer(site, init)
        elif isinstance(etype, TExternStruct):
            if len(etype.members) != len(init_asts):
                raise EDATAINIT(site, 'mismatched number of fields')
            init = {mn: do_eval(mt, e)
                    for ((mn, mt), e) in zip(etype.members_qualified,
                                             init_asts)}
            return DesignatedStructInitializer(site, init)
        elif etype.is_int and etype.is_bitfields:
            if len(etype.members) != len(init_asts):
                raise EDATAINIT(site, 'mismatched number of fields')
            return ExpressionInitializer(
                mk_bitfield_compound_initializer_expr(
                    site, etype,
                    zip((t[1:] for t in etype.members_qualified), init_asts),
                    location, scope, static))
        elif isinstance(etype, TNamed):
            return do_eval(safe_realtype(etype), astinit)
        else:
            raise EDATAINIT(site,
                'compound initializer not supported for type %s' % etype)
    return do_eval(etype, astinit)

def get_initializer(site, etype, astinit, location, scope):
    """Return an expression to use as initializer for a local variable.
    The 'init' is the ast for a single initializer given in the source, or
    None. This also checks for an invalid 'etype'."""
    # Check that the type is defined
    try:
        typ = realtype(etype)
    except DMLUnknownType:
        raise ETYPE(site, etype)

    if astinit:
        try:
            return eval_initializer(
                site, etype, astinit, location, scope, False)
        except DMLError as e:
            report(e)
    # This isn't really part of the spec for DML 1.0 and 1.2, but to
    # avoid C compiler warnings it's best to do it anyway.
    if typ.is_int:
        if typ.is_endian:
            return MemsetInitializer(site)
        else:
            return ExpressionInitializer(mkIntegerLiteral(site, 0))
    elif isinstance(typ, TBool):
        return ExpressionInitializer(mkBoolConstant(site, False))
    elif typ.is_float:
        return ExpressionInitializer(mkFloatConstant(site, 0.0))
    elif isinstance(typ, (TStruct, TExternStruct, TArray, TTrait, THook)):
        return MemsetInitializer(site)
    elif isinstance(typ, TPtr):
        return ExpressionInitializer(mkLit(site, 'NULL', typ))
    elif isinstance(typ, TVector):
        return ExpressionInitializer(mkLit(site, 'VNULL', typ))
    elif isinstance(typ, TFunction):
        raise EVARTYPE(site, etype.describe())
    elif isinstance(typ, TTraitList):
        return ExpressionInitializer(mkLit(site, '{NULL, 0, 0, 0, 0}', typ))
    raise ICE(site, "No initializer for %r" % (etype,))

statement_dispatcher = ast.astdispatcher('stmt_')

def codegen_statements(trees, *args):
    stmts = []
    for tree in trees:
        try:
            stmts.extend(statement_dispatcher.dispatch(tree, *args))
        except DMLError as e:
            report(e)
    return stmts

def codegen_statement(tree, *args):
    rbrace_site = tree.args[1] if tree.kind == 'compound' else None
    return mkCompound(tree.site, codegen_statements([tree], *args),
                      rbrace_site)

@statement_dispatcher
def stmt_compound(stmt, location, scope):
    [stmt_asts, rbrace_site] = stmt.args
    if (logging.show_porting and stmt.site.dml_version() == (1, 2)
        and not stmt.site.filename().endswith('dml-builtins.dml')):
        method = location.method()
        if method is not None and len(method.outp) == 1 and len(stmt_asts) >= 2:
            [(outarg, _)] = method.outp
            (assign, ret) = stmt_asts[-2:]
            if (ret.kind == 'return_dml12'
                and assign.kind == 'expression'
                and assign.args[0].kind == 'set'):
                (lh, rh) = assign.args[0].args
                if lh.kind == 'variable_dml12' and lh.args[0] == outarg:
                    report(POUTARGRETURN(lh.site,
                                         dmlparse.start_site(rh.site),
                                         ret.site))
    lscope = Symtab(scope)
    statements = codegen_statements(stmt_asts, location, lscope)
    return [mkCompound(stmt.site, declarations(lscope) + statements,
                       rbrace_site)]

def check_shadowing(scope, name, site):
    if (dml.globals.dml_version == (1, 2)
        and isinstance(scope.parent, MethodParamScope)):
        if scope.parent.lookup(name, local = True):
            report(WDEPRECATED(site,
                    'Variable %s in top-level method scope shadows parameter'
                    % name))

    sym = scope.lookup(name, local = True)
    if sym:
        raise EDVAR(site, sym.site, name)

def check_varname(site, name):
    if name in {'char', 'double', 'float', 'int', 'long', 'short',
                'signed', 'unsigned', 'void', 'register'}:
        report(ESYNTAX(site, name, 'type name used as variable name'))

@statement_dispatcher
def stmt_local(stmt, location, scope):
    # This doesn't occur in DML 1.0
    [decls, inits] = stmt.args
    method_call_init = False
    if inits is None:
        inits = [None] * len(decls)
    elif (len(inits) == 1 and inits[0].kind == 'initializer_scalar'
          and inits[0].args[0].kind == 'apply'):
        method_call_init = True
    elif len(decls) != len(inits):
        raise ESYNTAX(stmt.site, None,
                      'wrong number of initializers:\n'
                      + f'{len(decls)} variables declared\n'
                      + f'{len(inits)} initializers specified')
    stmts = []

    def convert_decl(decl_ast):
        (name, asttype) = decl_ast.args
        if (dml.globals.dml_version == (1, 2)
            and compat.dml12_misc not in dml.globals.enabled_compat):
            check_varname(stmt.site, name)
        (struct_decls, etype) = eval_type(asttype, stmt.site, location, scope)
        stmts.extend(mkStructDefinition(site, t) for (site, t) in struct_decls)
        etype = etype.resolve()
        rt = safe_realtype_shallow(etype)
        if isinstance(rt, TArray) and not rt.size.constant and deep_const(rt):
            raise EVLACONST(stmt.site)
        check_shadowing(scope, name, stmt.site)
        return (name, etype)

    decls = list(map(convert_decl, decls))

    def mk_sym(name, typ, mkunique=not dml.globals.debuggable):
        cname = scope.unique_cname(name) if mkunique else name
        return LocalSymbol(name, cname, type=typ, site=stmt.site, stmt=True)

    if method_call_init:
        syms_to_add = []
        tgt_syms = []
        late_declared_syms = []

        for (name, typ) in decls:
            sym = mk_sym(name, typ)
            tgt_typ = safe_realtype_shallow(typ)
            if shallow_const(tgt_typ):
                nonconst_typ = safe_realtype_unconst(tgt_typ)
                tgt_sym = mk_sym('_tmp_' + name, nonconst_typ, True)
                sym.init = ExpressionInitializer(mkLocalVariable(stmt.site,
                                                                 tgt_sym))
                late_declared_syms.append(sym)
            else:
                tgt_sym = sym
            syms_to_add.append(sym)
            tgt_syms.append(tgt_sym)

        tgts = [mkLocalVariable(stmt.site, sym) for sym in tgt_syms]
        method_invocation = try_codegen_invocation(stmt.site,
                                                   inits,
                                                   tgts, location, scope)
        if method_invocation is not None and stmt.site.dml_version != (1, 2):
            for sym in syms_to_add:
                scope.add(sym)
            stmts.extend(sym_declaration(sym) for sym in tgt_syms)
            stmts.append(method_invocation)
            stmts.extend(sym_declaration(sym)
                         for sym in late_declared_syms)
        else:
            if len(tgts) != 1:
                report(ERETLVALS(stmt.site, 1, len(tgts)))
            else:
                sym = syms_to_add[0]
                sym.init = eval_initializer(
                    inits[0].site, sym.type, inits[0], location, scope, False)
                scope.add(sym)
                stmts.append(sym_declaration(sym))
    else:
        # Initializer evaluation and variable declarations are done in separate
        # passes in order to prevent the newly declared variables from being in
        # scope when the initializers are evaluated
        inits = [get_initializer(stmt.site, typ, init, location, scope)
                 for ((_, typ), init) in zip(decls, inits)]
        for ((name, typ), init) in zip(decls, inits):
            sym = scope.add_variable(
                name, type = typ, site = stmt.site, init = init, stmt = True,
                make_unique=not dml.globals.debuggable)
            stmts.append(sym_declaration(sym))

    return stmts

@statement_dispatcher
def stmt_session(stmt, location, scope):
    [decls, inits] = stmt.args
    if inits is None:
        inits = itertools.cycle([None])
    elif len(decls) != len(inits):
        raise ESYNTAX(stmt.site, None,
                      'wrong number of initializers:\n'
                      + f'{len(decls)} variables declared\n'
                      + f'{len(inits)} initializers specified')
    if location.method() is None:
        # Removing this error would make 'session' compile fine in
        # traits, but it would not work as expected: different
        # instances of one trait would share the same variable
        # instance.  TODO: We should either forbid session explicitly
        # (replacing the ICE with a proper error message), or decide
        # and implement some sensible semantics for it.
        raise ICE(stmt.site, "'session' declaration inside a shared method is "
                  + "not yet allowed")
    elif (not dml.globals.dml_version == (1, 2)
          and not location.method().fully_typed):
        raise ESTOREDINLINE(stmt.site, 'session')
    for (decl_ast, init) in zip(decls, inits):
        (name, asttype) = decl_ast.args
        (struct_decls, etype) = eval_type(asttype, stmt.site, location,
                                          global_scope)
        etype = etype.resolve()
        add_late_global_struct_defs(struct_decls)
        if init:
            try:
                init = eval_initializer(
                    stmt.site, etype, init, location, global_scope, True)
            except DMLError as e:
                report(e)
                init = None
        check_shadowing(scope, name, stmt.site)

        static_var_expr = make_static_var(stmt.site, location, etype, name,
                                          init, stmt)
        local_sym = ExpressionSymbol(name, static_var_expr, stmt.site)
        scope.add(local_sym)

    return []

def make_static_var(site, location, static_sym_type, name, init=None,
                    stmt=False, saved=False):
    # generate a nested array of variables, indexed into by
    # the dimensions of the method dimensions
    for dimsize in reversed(location.method().dimsizes):
        static_sym_type = TArray(static_sym_type,
                                 mkIntegerConstant(site, dimsize, False))
        # initializer in methods cannot currently depend on indices
        # so we can replicate the same initializer for all
        # slots in the array.
        # TODO: it should be possible to support
        # index-dependent initialization now, though
        if init is not None:
            init = CompoundInitializer(site, [init] * dimsize)

    static_sym_name = f'static{len(dml.globals.static_vars)}_{name}'
    static_sym = StaticSymbol(static_sym_name, static_sym_name,
                              static_sym_type, site, init, stmt)
    static_var_expr = mkStaticVariable(site, static_sym)
    for idx in location.indices:
        static_var_expr = mkIndex(site, static_var_expr, idx)

    if init is not None:
        assert isinstance(init, Initializer)
        init_code = output.StrOutput()
        with init_code:
            if deep_const(static_sym_type):
                coverity_marker('store_writes_const_field', 'FALSE')
            init.assign_to(mkStaticVariable(site, static_sym),
                           static_sym_type)
        c_init = init_code.buf
    else:
        c_init = None
    dml.globals.static_vars.append((static_sym, c_init))
    if saved:
        saved_method_variables.setdefault(location.method(), []).append(
            (static_sym, name))
    return static_var_expr

@statement_dispatcher
def stmt_saved(stmt, location, scope):
    [decls, inits] = stmt.args
    if inits is None:
        inits = itertools.cycle([None])
    elif len(decls) != len(inits):
        raise ESYNTAX(stmt.site, None,
                      'wrong number of initializers:\n'
                      + f'{len(decls)} variables declared\n'
                      + f'{len(inits)} initializers specified')

    # guaranteed by parser
    assert dml.globals.dml_version != (1, 2)

    if location.method() is None:
        # Removing this error would make 'saved' compile fine in
        # traits, but it would not work as expected: different
        # instances of one trait would share the same variable
        # instance.  TODO: We should either forbid session/saved explicitly
        # (replacing the ICE with a proper error message), or decide
        # and implement some sensible semantics for it.
        raise ICE(stmt.site, "'saved' declaration inside a shared method is "
                  + "not yet allowed")
    elif not location.method().fully_typed:
        raise ESTOREDINLINE(stmt.site, 'saved')

    for (decl_ast, init) in zip(decls, inits):
        (name, asttype) = decl_ast.args
        (struct_decls, etype) = eval_type(asttype, stmt.site, location, scope)
        add_late_global_struct_defs(struct_decls)
        etype.resolve()
        if init:
            try:
                init = eval_initializer(
                    stmt.site, etype, init, location, scope, True)
            except DMLError as e:
                report(e)
                init = None
        check_shadowing(scope, name, stmt.site)

        serialize.mark_for_serialization(stmt.site, etype)

        # acquire better name
        node = location.node
        cname = name
        while node.objtype != "device":
            cname = node.name + "_" + cname
            node = node.parent

        static_var_expr = make_static_var(stmt.site, location, etype, name,
                                          init, stmt, True)
        local_sym = ExpressionSymbol(name, static_var_expr, stmt.site)
        scope.add(local_sym)

    return []

@statement_dispatcher
def stmt_null(stmt, location, scope):
    return []

@statement_dispatcher
def stmt_if(stmt, location, scope):
    [cond_ast, truebranch, falsebranch, else_site] = stmt.args
    cond = as_bool(codegen_expression(cond_ast, location, scope))
    if cond.constant and stmt.site.dml_version() == (1, 2):
        if (logging.show_porting
            and not stmt.site.filename().endswith('dml-builtins.dml')):
            # If the dead branch contains an error, then it must be
            # converted to '#if'.
            with logging.suppress_errors() as errors:
                if not cond.value:
                    codegen_statement(truebranch, location, scope)
                elif falsebranch:
                    codegen_statement(falsebranch, location, scope)
            if errors:
                report(PHASH(stmt.site))
                if falsebranch:
                    report(PHASHELSE(else_site, 'else'))
            if (not falsebranch and cond_ast.kind == 'binop'
                and cond_ast.args[1] == '&&'):
                lh = as_bool(codegen_expression(cond_ast.args[0], location, scope))
                with logging.suppress_errors() as errors:
                    as_bool(codegen_expression(cond_ast.args[2], location, scope))
                if lh.constant and not lh.value and errors:
                    report(PIFAND(cond_ast.site, stmt.site, dmlparse.end_site(truebranch.site)))
        if cond.value:
            return codegen_statements([truebranch], location, scope)
        elif falsebranch:
            return codegen_statements([falsebranch], location, scope)
        else:
            return []
    else:
        # print 'IF', 'NONCONST', cond
        tbranch = codegen_statement(truebranch, location, scope)
        if falsebranch:
            fbranch = codegen_statement(falsebranch, location, scope)
        else:
            fbranch = None
        return [ctree.If(stmt.site, cond, tbranch, fbranch, else_site)]

@statement_dispatcher
def stmt_hashif(stmt, location, scope):
    [cond_ast, truebranch, falsebranch] = stmt.args
    cond = as_bool(codegen_expression(cond_ast, location, scope))
    if not cond.constant:
        raise ENCONST(cond_ast.site, cond)
    if cond.value:
        return [codegen_statement(truebranch, location, scope)]
    elif falsebranch:
        return [codegen_statement(falsebranch, location, scope)]
    else:
        return []

def try_codegen_invocation(site, init_ast, outargs, location, scope):
    '''Generate a method call statement if the initializer init_ast is a
    single method call, otherwise None'''
    if len(init_ast) > 1:
        # multi initializer
        return None
    apply_ast = init_ast[0]
    if apply_ast.kind != 'initializer_scalar':
        # compound initializer
        return None
    if apply_ast.args[0].kind != 'apply':
        return None
    # possibly a method invocation
    (meth_ast, inarg_asts) = apply_ast.args[0].args
    meth_expr = codegen_expression_maybe_nonvalue(meth_ast, location, scope)
    if (isinstance(meth_expr, NonValue)
        and not isinstance(meth_expr, (
            TraitMethodRef, NodeRef, InterfaceMethodRef, HookSendNowRef,
            HookSendRef))):
        raise meth_expr.exc()
    if isinstance(meth_expr, TraitMethodRef):
        if not meth_expr.throws and len(meth_expr.outp) <= 1:
            # let the caller represent the method invocation as an
            # expression instead
            return None
        if (dml.globals.dml_version == (1, 2)
            and not in_try_block(location) and meth_expr.throws):
            # Shared methods marked as 'throws' count as
            # unconditionally throwing
            EBADFAIL_dml12.throwing_methods[location.method()] = site
        inargs = typecheck_inarg_inits(meth_expr.site, inarg_asts,
                                       meth_expr.inp, location, scope,
                                       'method')
        return codegen_call_traitmethod(site, meth_expr, inargs, outargs)
    elif not isinstance(meth_expr, NodeRef):
        return None
    # indeed a method invocation
    (meth_node, indices) = meth_expr.get_ref()
    if meth_node.objtype != 'method':
        return None
    if (meth_node.fully_typed and not meth_node.throws
        and len(meth_node.outp) <= 1):
        # let the caller represent the method invocation as an
        # expression instead
        return None
    if dml.globals.dml_version == (1, 2):
        # some methods in the 1.2 lib (e.g. register.read_access) require
        # args to be undefined, so we must permit this when calling
        # the default implementation
        inargs = typecheck_inarg_inits(meth_expr.site, inarg_asts,
                                       meth_node.inp, location, scope,
                                       'method', allow_undefined_args=True)

        if (site.dml_version() == (1, 2)
            and not in_try_block(location)
            and meth_node.throws):
            mark_method_invocation(site, meth_node, location)
        if (site.dml_version() == (1, 4)
            and not in_try_block(location)
            and not location.method().throws
            and meth_node.site.dml_version() == (1, 2)
            and meth_node.throws):
            if dml12_method_throws_in_dml14(meth_node):
                report(EBADFAIL_dml12(site, [(meth_node.site, meth_node)], []))
            EBADFAIL_dml12.protected_calls.setdefault(
                meth_node, []).append((site, location.method()))
            f = CatchFailure(scope, location.method())
            with f:
                call = (codegen_call(site, meth_node, indices,
                                    inargs, outargs)
                        if meth_node.fully_typed
                        else common_inline(site, meth_node, indices,
                                           inargs, outargs))
            return mkTryCatch(site, f.label, call,
                              mkAssert(site, mkBoolConstant(site, False)))

    else:
        inargs = typecheck_inarg_inits(meth_expr.site, inarg_asts,
                                       meth_node.inp, location, scope,
                                       'method')
    if meth_node.fully_typed:
        return codegen_call(site, meth_node, indices, inargs, outargs)
    else:
        return common_inline(site, meth_node, indices, inargs, outargs)

@statement_dispatcher
def stmt_assign(stmt, location, scope):
    (_, site, tgt_ast, src_asts) = stmt
    assert tgt_ast.kind in {'assign_target_chain', 'assign_target_tuple'}
    tgts = [codegen_expression(ast, location, scope)
            for ast in tgt_ast.args[0]]
    for tgt in tgts:
        if deep_const(tgt.ctype()):
            raise ECONST(tgt.site)
    if tgt_ast.kind == 'assign_target_chain':
        method_tgts = [tgts[0]]
    else:
        method_tgts = tgts

    # TODO support multiple assign sources. It should be generalized.
    method_invocation = try_codegen_invocation(site, src_asts, method_tgts,
                                               location, scope)
    if method_invocation:
        if tgt_ast.kind == 'assign_target_chain' and len(tgts) != 1:
            report(ESYNTAX(
                tgt_ast.args[0][1].site, '=',
                'assignment chain not allowed as method invocation target'))
        return [method_invocation]
    if tgt_ast.kind == 'assign_target_chain':
        if len(src_asts) > 1:
            report(ESYNTAX(site, '(',
                           'wrong number of simultaneous assign targets for '
                           + f'initializer: Expected {src_asts}, got 1'))
            return []

        stmts = []
        lscope = Symtab(scope)
        init = eval_initializer(
            site, tgts[-1].ctype(), src_asts[0], location, scope, False)

        for (i, tgt) in enumerate(reversed(tgts[1:])):
            name = 'tmp%d' % (i,)
            sym = lscope.add_variable(
                name, type=tgt.ctype(), site=tgt.site, init=init, stmt=True)
            init = ExpressionInitializer(mkLocalVariable(tgt.site, sym))
            stmts.extend([sym_declaration(sym),
                          mkAssignStatement(tgt.site, tgt, init)])
        return stmts + [mkAssignStatement(tgts[0].site, tgts[0], init)]
    else:
        # Guaranteed by grammar
        assert tgt_ast.kind == 'assign_target_tuple' and len(tgts) > 1
        if (len(src_asts) == 1 and src_asts[0].kind == 'initializer_scalar'
            and src_asts[0].args[0].kind == 'apply'):
            report(ERETLVALS(site, 1, len(tgts)))
            return []
        elif len(src_asts) != len(tgts):
            report(ESYNTAX(site, '(',
                           'wrong number of simultaneous assign targets for '
                           + f'initializer: Expected {src_asts}, got '
                           + str(tgts)))
            return []

        stmts = []
        lscope = Symtab(scope)
        syms = []
        for (i, (tgt, src_ast)) in enumerate(zip(tgts, src_asts)):
            init = eval_initializer(site, tgt.ctype(), src_ast, location,
                                    scope, False)
            name = 'tmp%d' % (i,)
            sym = lscope.add_variable(
                    name, type=tgt.ctype(), site=tgt.site, init=init,
                    stmt=True)
            syms.append(sym)

        stmts.extend(map(sym_declaration, syms))
        stmts.extend(
            mkAssignStatement(
                tgt.site, tgt, ExpressionInitializer(mkLocalVariable(tgt.site,
                                                                     sym)))
            for (tgt, sym) in zip(tgts, syms))
        return stmts

@statement_dispatcher
def stmt_assignop(stmt, location, scope):
    (kind, site, tgt_ast, op, src_ast) = stmt

    tgt = codegen_expression(tgt_ast, location, scope)
    if deep_const(tgt.ctype()):
        raise ECONST(tgt.site)
    if isinstance(tgt, ctree.BitSlice):
        # destructive hack
        return stmt_assign(
            ast.assign(site, ast.assign_target_chain(site, [tgt_ast]),
                       [ast.initializer_scalar(
                           site,
                           ast.binop(site, tgt_ast, op[:-1], src_ast))]),
            location, scope)
    src = codegen_expression(src_ast, location, scope)
    ttype = tgt.ctype()
    lscope = Symtab(scope)
    sym = lscope.add_variable(
        'tmp', type = TPtr(ttype), site = tgt.site,
        init = ExpressionInitializer(mkAddressOf(tgt.site, tgt)), stmt=True)
    # Side-Effect Free representation of the tgt lvalue
    tgt_sef = mkDereference(site, mkLocalVariable(tgt.site, sym))
    return [
        sym_declaration(sym), mkExpressionStatement(
        site,
            mkAssignOp(site, tgt_sef, arith_binops[op[:-1]](
                site, tgt_sef, src)))]

@statement_dispatcher
def stmt_expression(stmt, location, scope):
    [expr] = stmt.args
    # a method invocation with no return value looks like an
    # expression statement to the grammar
    invocation = try_codegen_invocation(stmt.site,
                                        [ast.initializer_scalar(stmt.site,
                                                                expr)],
                                        [], location, scope)
    if invocation:
        return [invocation]
    return [mkExpressionStatement(stmt.site,
                                  codegen_expression(expr, location, scope))]

@statement_dispatcher
def stmt_throw(stmt, location, scope):
    handler = Failure.fail_stack[-1]
    if not handler.allowed:
        raise EBADFAIL(stmt.site)
    if dml.globals.dml_version == (1, 2) and not in_try_block(location):
        EBADFAIL_dml12.throwing_methods[location.method()] = stmt.site
    return [handler.fail(stmt.site)]

@statement_dispatcher
def stmt_error(stmt, location, scope):
    [msg] = stmt.args
    raise EERRSTMT(stmt.site, "forced compilation error in source code"
                   if msg is None else msg)

@statement_dispatcher
def stmt_warning(stmt, location, scope):
    [msg] = stmt.args
    report(WWRNSTMT(stmt.site, msg))
    return []

@statement_dispatcher
def stmt_return_dml12(stmt, location, scope):
    if logging.show_porting:
        m = location.method()
        if m and m.outp:
            report(PRETURNARGS(stmt.site, [name for (name, _) in m.outp]))
    [args] = stmt.args
    assert not args # ensured by parser
    return [codegen_exit(stmt.site, None)]

@statement_dispatcher
def stmt_return(stmt, location, scope):
    [inits] = stmt.args
    if isinstance(ExitHandler.current, GotoExit_dml14):
        outp_typs = [var.ctype() for var in ExitHandler.current.outvars]
    elif isinstance(ExitHandler.current, (ReturnExit, MemoizedReturnExit)):
        outp_typs = [typ for (_, typ) in ExitHandler.current.outp]
    else:
        raise ICE(stmt.site, ("Unexpected ExitHandler "
                              + f"'{type(ExitHandler.current).__name__}'"))

    if (len(inits) == 1 and inits[0].kind == 'initializer_scalar'
        and inits[0].args[0].kind == 'apply'):
        outp = None
        meth_expr = codegen_expression_maybe_nonvalue(
            inits[0].args[0].args[0], location, scope)
        site = meth_expr.site
        if isinstance(meth_expr, TraitMethodRef):
            outp = meth_expr.outp
        elif isinstance(meth_expr, NodeRef):
            (meth_node, indices) = meth_expr.get_ref()
            if meth_node.objtype == 'method':
                outp = meth_node.outp
        if outp is not None:
            lscope = Symtab(scope)
            outarg_syms = [
                lscope.add_variable(f'tmp{i}', type=typ, site=site, stmt=True)
                for (i, (_, typ)) in enumerate(outp)]
            outargs = [mkLocalVariable(site, sym) for sym in outarg_syms]
            method_invocation = try_codegen_invocation(site, inits, outargs,
                                                       location, scope)
            if method_invocation is not None:
                if len(outargs) != len(outp_typs):
                    report(ERETARGS(stmt.site, len(outp_typs), len(outargs)))
                    # avoid control flow errors by falling back to statement
                    # with no fall-through
                    return [mkAssert(stmt.site,
                                     mkBoolConstant(stmt.site, False))]

                return ([sym_declaration(sym) for sym in outarg_syms]
                        + [method_invocation,
                           codegen_exit(stmt.site, outargs)])

    if len(inits) != len(outp_typs):
        report(ERETARGS(stmt.site, len(outp_typs), len(inits)))
        return [mkAssert(stmt.site, mkBoolConstant(stmt.site, False))]

    return [codegen_exit(stmt.site,
                         [eval_initializer(stmt.site, typ, init, location,
                                           scope, False).as_expr(typ)
                          for (init, typ) in zip(inits, outp_typs)])]

@statement_dispatcher
def stmt_assert(stmt, location, scope):
    [expr] = stmt.args
    expr = codegen_expression(expr, location, scope)
    return [mkAssert(stmt.site, as_bool(expr))]
@statement_dispatcher
def stmt_goto(stmt, location, scope):
    [label] = stmt.args
    if compat.dml12_goto not in dml.globals.enabled_compat:
        report(ESYNTAX(stmt.site, 'goto', 'goto statement not allowed'))
    return [mkGoto(stmt.site, label)]

@statement_dispatcher
def stmt_label(stmt, location, scope):
    [label, statement] = stmt.args
    return [mkLabel(stmt.site, label),
            codegen_statement(statement, location, scope)]
@statement_dispatcher
def stmt_case_dml12(stmt, location, scope):
    [expr_ast, statement] = stmt.args
    expr = codegen_expression(expr_ast, location, scope)
    return [mkCase(stmt.site, expr),
            codegen_statement(statement, location, scope)]

@statement_dispatcher
def stmt_default_dml12(stmt, location, scope):
    [statement] = stmt.args
    return [mkDefault(stmt.site), codegen_statement(statement, location, scope)]

@statement_dispatcher
def stmt_case(stmt, location, scope):
    [expr_ast] = stmt.args
    expr = codegen_expression(expr_ast, location, scope)
    return [mkCase(stmt.site, expr)]

@statement_dispatcher
def stmt_default(stmt, location, scope):
    assert not stmt.args
    return [mkDefault(stmt.site)]

@statement_dispatcher
def stmt_delete(stmt, location, scope):
    [expr] = stmt.args
    expr = codegen_expression(expr, location, scope)
    return [mkDelete(stmt.site, expr)]

def probable_loggroups_specification(expr):
    subexprs = [expr]
    while subexprs:
        expr = subexprs.pop()

        if (isinstance(expr, LogGroup)
            or (expr.constant and expr.value == 0)):
            return True

        if isinstance(expr, Cast):
            subexprs.append(expr.expr)
        elif isinstance(expr, (BitOr, BitOr_dml12)):
            subexprs.extend((expr.lh, expr.rh))
        elif isinstance(expr, IfExpr):
            subexprs.extend((expr.texpr, expr.fexpr))

    return False

def probable_loglevel_specification(expr):
    subexprs = [expr]
    probable = False
    while subexprs:
        expr = subexprs.pop()

        if (isinstance(expr, LogGroup)
            or (expr.constant and expr.value == 0)):
            return False

        if expr.constant and 1 <= expr.value <= 5:
            probable = True
        elif isinstance(expr, Cast):
            subexprs.append(expr.expr)
        elif isinstance(expr, IfExpr):
            subexprs.extend((expr.texpr, expr.fexpr))

    return probable

log_index = 0
@statement_dispatcher
def stmt_log(stmt, location, scope):
    [logkind, level, later_level, groups, fmt, args] = stmt.args
    argsites = [arg.site for arg in args]
    args = [ codegen_expression(arg, location, scope)
             for arg in args ]

    site = stmt.site

    warn_mixup = False

    error_logkind = logkind in {'error', 'critical', 'warning'}
    adjusted_level = level = ctree.as_int(
        codegen_expression(level, location, scope))
    bad_error_level = (error_logkind
                       and (not level.constant or level.value != 1))

    # This correction must be done independently of
    # compat.meaningless_log_levels, otherwise existing usages of
    # e.g. log error, 2: "..." will become noops
    if bad_error_level:
        adjusted_level = mkIntegerLiteral(site, 1)
    if compat.meaningless_log_levels not in dml.globals.enabled_compat:
        if bad_error_level:
            report(ELLEV(level.site, "1"))
    elif level.constant and not (1 <= level.value <= 4):
        report(ELLEV(level.site, "an integer between 1 and 4"))
        adjusted_level = mkIntegerLiteral(site, 1)
    else:
        warn_mixup = probable_loggroups_specification(level)

    # Acquire a subsequent log key and the logging object based on obj or trait
    # identity
    if location.method():
        identity = ObjIdentity(site, location.node.parent, location.indices)
        logobj = log_object(site, location.node, location.indices)
    else:
        identity = TraitObjIdentity(site, lookup_var(site, scope, "this"))
        logobj = (log_object(site, dml.globals.device, ())
                  if compat.shared_logs_on_device in dml.globals.enabled_compat
                  else PortObjectFromObjIdentity(site, identity))

    log_wrapper = lambda stmt: stmt

    if later_level is not None:
        adjusted_later_level = later_level = ctree.as_int(codegen_expression(
            later_level, location, scope))
        if (later_level.constant and level.constant and
            later_level.value == level.value):
            report(WREDUNDANTLEVEL(site))
        if (error_logkind
            and (compat.meaningless_log_levels
                 not in dml.globals.enabled_compat)):
            if not later_level.constant or later_level.value not in {1, 5}:
                report(ELLEV(later_level.site, "a 1 or 5 constant"))
                adjusted_later_level = mkIntegerLiteral(site, 1)
        elif (later_level.constant and not (1 <= later_level.value <= 5)):
            report(ELLEV(later_level.site, "an integer between 1 and 5"))
            adjusted_later_level = mkIntegerLiteral(site, 4)
        elif not warn_mixup:
            warn_mixup = probable_loggroups_specification(later_level)
        global log_index
        table_ptr = TPtr(TNamed("ht_int_table_t"))
        table = mkLit(site, '&(_dev->_subsequent_log_ht)', table_ptr)
        key = mkApply(site,
                      mkLit(site, "_identity_to_key",
                            TFunction([TNamed('_identity_t')],
                                      TInt(64, False))),
                      [identity])

        once_lookup = mkLit(
            site, "_select_log_level",
            TFunction([table_ptr, TInt(64, False), TInt(64, False),
                       TInt(64, False), TInt(64, False)],
                      TInt(64, False)))
        level_expr = mkApply(site, once_lookup,
                             [table, key, mkIntegerLiteral(site, log_index),
                              adjusted_level, adjusted_later_level])
        log_index += 1
        pre_statements = [mkDeclaration(site, "_calculated_level",
                                        TInt(64, False),
                                        ExpressionInitializer(level_expr))]
        adjusted_level = mkLocalVariable(site, LocalSymbol("_calculated_level",
                                                           "_calculated_level",
                                                           TInt(64, False),
                                                           site=site))
        if error_logkind:
            log_wrapper = lambda stmt: mkIf(
                site,
                mkEquals(site, adjusted_level, mkIntegerLiteral(site, 1)),
                stmt)
    else:
        pre_statements = []

    groups = ctree.as_int(codegen_expression(groups, location, scope))
    warn_mixup = warn_mixup or probable_loglevel_specification(groups)
    if warn_mixup:
        report(WLOGMIXUP(site, logkind, level, later_level, groups))
    fmt, args = fix_printf(fmt, args, argsites, site)
    return [mkCompound(site, pre_statements + [
        log_wrapper(log_statement(site, logobj, logkind, adjusted_level,
                                  groups, fmt, *args))])]
@statement_dispatcher
def stmt_try(stmt, location, scope):
    [tryblock, excblock] = stmt.args

    f = CatchFailure(scope, location.method())
    with f:
        tryblock = codegen_statement(tryblock, location, scope)
    if dml.globals.dml_version == (1, 2) and not f.label:
        return [tryblock]
    excblock = codegen_statement(excblock, location, scope)
    return [mkTryCatch(stmt.site, f.label, tryblock, excblock)]

@statement_dispatcher
def stmt_after(stmt, location, scope):
    [unit, delay, callexpr] = stmt.args
    site = stmt.site

    if callexpr[0] == 'apply':
        method_ast = callexpr[2]
        inargs = callexpr[3]
    else:
        if dml.globals.dml_version == (1, 2):
            method_ast = callexpr
            inargs = []
        else:
            raise ESYNTAX(site, None,
                          'callback expression to after statement must be a '
                          + 'function application')

    delay = codegen_expression(delay, location, scope)
    old_delay_type = delay.ctype()
    if unit == 's':
        clock = 'SIM_object_clock(&_dev->obj)'
        api_unit = 'time'
        unit_type = TFloat('double')
    elif unit == 'cycles':
        clock = 'SIM_object_clock(&_dev->obj)'
        api_unit = 'cycle'
        unit_type = TInt(64, True)
    elif unit == 'ps':
        clock = 'SIM_picosecond_clock(&_dev->obj)'
        api_unit = 'cycle'
        unit_type = TInt(64, True)
    else:
        raise ICE(site, f"Unsupported unit of time: '{unit}'")

    try:
        delay = source_for_assignment(site, unit_type, delay)
    except EASTYPE:
        raise EBTYPE(site, old_delay_type, unit_type)

    if unit in {'cycles', 'ps'} and not safe_realtype(old_delay_type).is_int:
        report(WTTYPEC(site, old_delay_type, unit_type, unit))

    # TODO after statement should be extended to allow the user to explicitly
    # give the domains
    if location.method():
        domains = [ObjIdentity(site, location.node.parent, location.indices)]
    else:
        domains = [TraitObjIdentity(site, lookup_var(site, scope, "this"))]

    methodref = codegen_expression_maybe_nonvalue(method_ast, location, scope)

    if isinstance(methodref, NodeRef) and methodref.node.objtype == 'method':
        method, indices = methodref.get_ref()

        if len(method.outp) > 0:
            raise EAFTER(site, None, method, None)

        require_fully_typed(site, method)
        func = method_instance(method)
        inp = func.inp
        kind = 'method'
    elif isinstance(methodref, HookSendNowRef):
        indices = ()
        send_now_hookref = methodref.hookref_expr
        msg_types = safe_realtype_shallow(send_now_hookref.ctype()).msg_types
        inp = [(f'comp{i}', typ) for (i, typ) in enumerate(msg_types)]
        kind = 'send_now'
    else:
        raise ENMETH(site, methodref)

    inargs = typecheck_inarg_inits(site, inargs, inp, location, scope, kind)

    # After-call is only possible for methods with serializable parameters
    unserializable = []
    for (pname, ptype) in inp:
        try:
            serialize.mark_for_serialization(site, ptype)
        except ESERIALIZE:
            unserializable.append((pname, ptype))

    if kind == 'method':
        if len(unserializable) > 0:
            raise EAFTER(site, None, method, unserializable)
        else:
            mark_method_referenced(func)
            after_info = get_after_delay(method)
            args_init = AfterIntoMethodArgsInit(inargs)
    else:
        assert kind == 'send_now'
        if len(unserializable) > 0:
            raise EAFTERSENDNOW(site, None, methodref.hookref_expr,
                                unserializable)
        else:
            typeseq_info = get_type_sequence_info(
                (typ for (_, typ) in inp), create_new=True)
            after_info = get_after_delay(typeseq_info)
            args_init = AfterIntoSendNowArgsInit(inargs,
                                                 methodref.hookref_expr)

    return [mkAfter(site, clock, api_unit, delay, domains, after_info, indices,
                    args_init)]


class MsgCompParamRestrictedSymbol(NonValue):
    @auto_init
    def __init__(self, site, name): pass
    def __str__(self):
        return self.name
    def exc(self):
        return EAFTERMSGCOMPPARAM(self.site, self.name)

class MsgCompParam(Expression):
    @auto_init
    def __init__(self, site, name, type): pass
    def __str__(self):
        return self.name
    def read(self):
        raise ICE(self.site,
                  (f".read() of message component parameter '{self.name}' "
                   + "called"))

@statement_dispatcher
def stmt_afteronhook(stmt, location, scope):
    [hookref, msg_comp_param_asts, callexpr] = stmt.args
    site = stmt.site

    if callexpr[0] != 'apply':
        raise ESYNTAX(site, None,
                      'callback expression to after statement must be a '
                      + 'function application')

    method = callexpr[2]
    inarg_asts = callexpr[3]

    hookref_expr = codegen_expression(hookref, location, scope)
    hooktype = hookref_expr.ctype()
    real_hooktype = safe_realtype_shallow(hooktype)
    if not isinstance(real_hooktype, THook):
        raise EBTYPE(hookref_expr.site, hooktype, 'hook')

    real_hooktype.validate(hooktype.declaration_site or hookref_expr.site)

    # TODO after statement should be extended to allow the user to explicitly
    # give the domains
    if location.method():
        domains = [ObjIdentity(site, location.node.parent, location.indices)]
    else:
        domains = [TraitObjIdentity(site, lookup_var(site, scope, "this"))]

    methodref = codegen_expression_maybe_nonvalue(method, location, scope)

    if isinstance(methodref, NodeRef) and methodref.node.objtype == 'method':
        method, indices = methodref.get_ref()

        if len(method.outp) > 0:
            raise EAFTER(site, None, method, None)

        require_fully_typed(site, method)
        func = method_instance(method)
        inp = func.inp
        kind = 'method'
    elif isinstance(methodref, HookSendNowRef):
        indices = ()
        send_now_hookref = methodref.hookref_expr
        msg_types = safe_realtype_shallow(send_now_hookref.ctype()).msg_types
        inp = [(f'comp{i}', typ) for (i, typ) in enumerate(msg_types)]
        kind = 'send_now'
    else:
        raise ENMETH(site, methodref)

    if len(msg_comp_param_asts) != len(real_hooktype.msg_types):
        raise EAFTERHOOK(
            site, hookref_expr, len(real_hooktype.msg_types),
            len(msg_comp_param_asts))

    msg_comp_params = {}
    for (idx, (mcp_site, mcp_name)) in enumerate(msg_comp_param_asts):
        if mcp_name in msg_comp_params:
            raise EDVAR(mcp_site, msg_comp_params[mcp_name][1],
                        mcp_name)
        else:
            msg_comp_params[mcp_name] = (idx, mcp_site)

    arg_index_to_msg_comp_param = {}
    for (idx, inarg) in enumerate(inarg_asts):
        if (inarg.kind == 'initializer_scalar'
            and inarg.args[0].kind == 'variable'
            and inarg.args[0].args[0] in msg_comp_params):
            arg_index_to_msg_comp_param[idx] = (
                inarg.args[0].args[0],
                *msg_comp_params[inarg.args[0].args[0]])

    inarg_inits = []
    for (i, inarg_ast) in enumerate(inarg_asts):
        if i in arg_index_to_msg_comp_param:
            (name, param_idx, site) = arg_index_to_msg_comp_param[i]
            typ = real_hooktype.msg_types[param_idx]
            inarg_inits.append(
                ExpressionInitializer(MsgCompParam(site, name, typ)))
        else:
            inarg_inits.append(inarg_ast)

    args_scope = MethodParamScope(scope)
    for (name, (_, msg_param_site)) in msg_comp_params.items():
        args_scope.add(ExpressionSymbol(
            name, MsgCompParamRestrictedSymbol(msg_param_site, name),
            msg_param_site))

    inargs = typecheck_inarg_inits(site, inarg_inits, inp, location,
                                   args_scope, kind)
    filtered_inargs = [inarg for (i, inarg) in enumerate(inargs)
                       if i not in arg_index_to_msg_comp_param]

    unserializable = []
    for (idx, (pname, ptype)) in enumerate(inp):
        if idx not in arg_index_to_msg_comp_param:
            try:
                serialize.mark_for_serialization(site, ptype)
            except ESERIALIZE:
                unserializable.append((pname, ptype))

    if kind == 'method':
        if len(unserializable) > 0:
            raise EAFTER(site, hookref_expr, method, unserializable)
        else:
            mark_method_referenced(func)
            aoh_key = method
            args_init = AfterIntoMethodArgsInit(filtered_inargs)
    else:
        assert kind == 'send_now'
        if len(unserializable) > 0:
            raise EAFTERSENDNOW(site, hookref_expr, methodref.hookref_expr,
                                unserializable)
        else:
            aoh_key = get_type_sequence_info(
                (typ for (_, typ) in inp), create_new=True)
            args_init = AfterIntoSendNowArgsInit(filtered_inargs,
                                                 methodref.hookref_expr)

    param_idx_to_msg_comp_idx = { i: arg_index_to_msg_comp_param[i][1]
                                  for i in arg_index_to_msg_comp_param }

    typeseq_info = get_type_sequence_info(real_hooktype.msg_types,
                                          create_new=True)

    aoh_info = typeseq_info.get_after_on_hook(
        aoh_key, param_idx_to_msg_comp_idx, len(inp), create_new=True)

    return [mkAfterOnHook(site, domains, hookref_expr, aoh_info, indices,
                          args_init)]

@statement_dispatcher
def stmt_immediateafter(stmt, location, scope):
    [callexpr] = stmt.args
    site = stmt.site

    if callexpr[0] != 'apply':
        raise ESYNTAX(site, None,
                      'callback expression to after statement must be a '
                      + 'function application')

    method = callexpr[2]
    inarg_asts = callexpr[3]

    # TODO after statement should be extended to allow the user to explicitly
    # give the domains
    if location.method():
        domains = [ObjIdentity(site, location.node.parent, location.indices)]
    else:
        domains = [TraitObjIdentity(site, lookup_var(site, scope, "this"))]

    methodref = codegen_expression_maybe_nonvalue(method, location, scope)

    if isinstance(methodref, NodeRef) and methodref.node.objtype == 'method':
        method, indices = methodref.get_ref()

        if len(method.outp) > 0:
            raise EAFTER(site, None, method, None)

        require_fully_typed(site, method)
        func = method_instance(method)
        inp = func.inp
        kind = 'method'
    elif isinstance(methodref, HookSendNowRef):
        indices = ()
        send_now_hookref = methodref.hookref_expr
        msg_types = safe_realtype_shallow(send_now_hookref.ctype()).msg_types
        inp = [(f'comp{i}', typ) for (i, typ) in enumerate(msg_types)]
        kind = 'send_now'
    else:
        raise ENMETH(site, methodref)

    inargs = typecheck_inarg_inits(
        site, inarg_asts, inp, location, scope, kind,
        on_ptr_to_stack=(lambda x: report(WIMMAFTER(x.site, x))))


    if kind == 'method':
        mark_method_referenced(func)
        after_info = get_immediate_after(method)
        args_init = AfterIntoMethodArgsInit(inargs)
    else:
        assert kind == 'send_now'
        typeseq_info = get_type_sequence_info((typ for (_, typ) in inp),
                                              create_new=True)
        after_info = get_immediate_after(typeseq_info)
        args_init = AfterIntoSendNowArgsInit(inargs,
                                             methodref.hookref_expr)

    return [mkImmediateAfter(site, domains, after_info, indices, args_init)]

@statement_dispatcher
def stmt_select(stmt, location, scope):
    [itername, lst, cond_ast, stmt_ast, else_ast] = stmt.args
    # dbg('SELNODE %r, %r, %r' % (location.node, location.indices, lst))
    lst = codegen_expression_maybe_nonvalue(lst, location, scope)
    # dbg('SELECT %s in %r' % (itername, lst))
    if isinstance(lst, NonValue):
        if isinstance(lst, AbstractList):
            l = lst.iter_flat()
            scope = Symtab(scope)
            else_dead = False
            # list of (cond, body)
            clauses = []
            for it in l:
                condscope = Symtab(scope)
                condscope.add(ExpressionSymbol(itername, it, stmt.site))
                cond = as_bool(codegen_expression(
                    cond_ast, location, condscope))
                if cond.constant and not cond.value:
                    continue
                clauses.append((
                    cond, codegen_statement(stmt_ast, location, condscope)))
                if cond.constant and cond.value:
                    else_dead = True
                    break

            if else_dead:
                (last_cond, last_stmt) = clauses.pop(-1)
                assert last_cond.constant and last_cond.value
                if_chain = last_stmt
            else:
                if_chain = codegen_statement(else_ast, location, scope)
            for (cond, stmt) in reversed(clauses):
                if_chain = mkIf(cond.site, cond, stmt, if_chain)
            return [if_chain]
        raise lst.exc()
    elif (compat.dml12_misc in dml.globals.enabled_compat
          and isinstance(lst.ctype(), TVector)):
        itervar = lookup_var(stmt.site, scope, itername)
        if not itervar:
            raise EIDENT(stmt.site, itername)
        return [mkVectorForeach(stmt.site,
                                lst, itervar,
                                codegen_statement(stmt_ast, location, scope))]
    else:
        raise ENLST(stmt.site, lst)

def foreach_each_in(site, itername, trait, each_in,
                    body_ast, location, scope):
    inner_scope = Symtab(scope)
    trait_type = TTrait(trait)
    inner_scope.add_variable(
        itername, type=trait_type, site=site,
        init=ForeachSequence.itervar_initializer(site, trait))
    context = ForeachSequenceLoopContext()
    with context:
        inner_body = mkCompound(site, declarations(inner_scope)
            + codegen_statements([body_ast], location, inner_scope))

    break_label = context.label if context.used else None
    return [mkForeachSequence(site, trait, each_in, inner_body, break_label)]

@expression_dispatcher
def expr_each_in(ast, location, scope):
    (traitname, node_ast) = ast.args
    node_expr = codegen_expression_maybe_nonvalue(node_ast, location, scope)
    if not isinstance(node_expr, NodeRef):
        raise ENOBJ(node_expr.site, node_expr)
    (node, indices) = node_expr.get_ref()
    trait = dml.globals.traits.get(traitname)
    if trait is None:
        raise ENTMPL(ast.site, traitname)
    return mkEachIn(ast.site, trait, node, indices)

@statement_dispatcher
def stmt_foreach_dml12(stmt, location, scope):
    [itername, lst, statement] = stmt.args
    lst = codegen_expression_maybe_nonvalue(lst, location, scope)
    if isinstance(lst, NonValue):
        if not isinstance(lst, AbstractList):
            raise lst.exc()
        return foreach_constant_list(stmt.site, itername, lst,
                                     statement, location, scope)

    list_type = safe_realtype(lst.ctype())
    if isinstance(list_type, TVector):
        itervar = lookup_var(stmt.site, scope, itername)
        if not itervar:
            raise EIDENT(lst, itername)
        with CLoopContext():
            res = mkVectorForeach(stmt.site, lst, itervar,
                                  codegen_statement(statement, location, scope))
        return [res]
    else:
        raise ENLST(stmt.site, lst)

@statement_dispatcher
def stmt_foreach(stmt, location, scope):
    [itername, lst, statement] = stmt.args
    lst = codegen_expression(lst, location, scope)
    list_type = safe_realtype(lst.ctype())
    if isinstance(list_type, TTraitList):
        return foreach_each_in(
            stmt.site, itername,
            # .traitname was validated by safe_realtype()
            dml.globals.traits[list_type.traitname],
            lst, statement, location, scope)
    else:
        raise ENLST(stmt.site, lst)

@statement_dispatcher
def stmt_hashforeach(stmt, location, scope):
    [itername, lst, statement] = stmt.args
    lst = codegen_expression_maybe_nonvalue(lst, location, scope)
    if isinstance(lst, NonValue):
        if not isinstance(lst, AbstractList):
            raise lst.exc()
        return foreach_constant_list(stmt.site, itername, lst,
                                     statement, location, scope)
    elif not lst.constant:
        raise ENCONST(stmt.site, lst)
    else:
        raise ENLST(stmt.site, lst)

def foreach_constant_list(site, itername, lst, statement, location, scope):
    assert isinstance(lst, AbstractList)
    spec = []
    context = GotoLoopContext()
    with context:
        for items in lst.iter():
            loopvars = tuple(mkLit(site, '_ai%d_%d' % (context.id, dim),
                                   TInt(32, True))
                             for dim in range(len(items.dimsizes)))
            loopscope = Symtab(scope)
            loopscope.add(ExpressionSymbol(
                itername, items.expr(loopvars), site))
            stmt = codegen_statement(statement, location, loopscope)

            if stmt.is_empty:
                continue

            decls = []
            for dim in reversed(range(len(items.dimsizes))):
                decls.append(mkDeclaration(site, loopvars[dim].str,
                                           TInt(32, True)))
                stmt = mkFor(
                    site,
                    [mkAssignOp(site,
                                loopvars[dim],
                                mkIntegerLiteral(site, 0))],
                    mkLessThan(site, loopvars[dim],
                               mkIntegerLiteral(site,
                                                items.dimsizes[dim])),
                    [mkInline(site, '++%s;' % (loopvars[dim].str,))],
                    stmt)
            spec.append(mkCompound(site, decls + [stmt]))

        return [mkUnrolledLoop(site, spec,
                               context.label if context.used else None)]

@statement_dispatcher
def stmt_while(stmt, location, scope):
    [cond, statement] = stmt.args
    cond = as_bool(codegen_expression(cond, location, scope))
    if stmt.site.dml_version() == (1, 2) and cond.constant and not cond.value:
        return [mkNull(stmt.site)]
    else:
        with CLoopContext():
            res = mkWhile(stmt.site, cond,
                          codegen_statement(statement, location, scope))
        return [res]

@statement_dispatcher
def stmt_for(stmt, location, scope):
    [pres, cond, posts, statement] = stmt.args
    pres = [codegen_expression(pre, location, scope)
            for pre in pres]
    if cond is None:
        cond = mkBoolConstant(stmt.site, 1)
    else:
        cond = as_bool(codegen_expression(cond, location, scope))
    posts = codegen_statements(posts, location, scope)
    with CLoopContext():
        res = mkFor(stmt.site, pres, cond, posts,
                    codegen_statement(statement, location, scope))
    return [res]

@statement_dispatcher
def stmt_dowhile(stmt, location, scope):
    [cond, statement] = stmt.args
    cond = as_bool(codegen_expression(cond, location, scope))
    with CLoopContext():
        res = mkDoWhile(stmt.site, cond,
                        codegen_statement(statement, location, scope))
    return [res]

@statement_dispatcher
def stmt_switch(stmt, location, scope):
    [expr, body_ast] = stmt.args
    expr = codegen_expression(expr, location, scope)
    with CLoopContext():
        if stmt.site.dml_version() != (1, 2):
            assert body_ast.kind == 'compound'
            [stmt_asts, rbrace_site] = body_ast.args
            stmts = codegen_statements(stmt_asts, location, scope)
            if (not stmts
                or not isinstance(stmts[0], (ctree.Case, ctree.Default))):
                raise ESWITCH(
                    body_ast.site, "statement before first case label")
            defaults = [i for (i, sub) in enumerate(stmts)
                        if isinstance(sub, ctree.Default)]
            if len(defaults) > 1:
                raise ESWITCH(stmts[defaults[1]].site,
                              "duplicate default label")
            if defaults:
                for sub in stmts[defaults[0]:]:
                    if isinstance(sub, ctree.Case):
                        raise ESWITCH(sub.site,
                                      "case label after default label")
            body_stmts = []
            default_found = False
            subsequent_cases = []
            for body_stmt in stmts:
                if isinstance(body_stmt, (ctree.Case, ctree.Default)):
                    default_found = (default_found
                                     or isinstance(body_stmt, ctree.Default))

                    subsequent_cases.append(body_stmt)
                else:
                    if subsequent_cases:
                        body_stmts.append(mkSubsequentCases(
                            subsequent_cases[0].site, subsequent_cases,
                            default_found))
                        subsequent_cases = []
                        default_found = False
                    body_stmts.append(body_stmt)

            if subsequent_cases:
                body_stmts.append(mkSubsequentCases(
                    subsequent_cases[0].site, subsequent_cases, default_found))

            body = ctree.Compound(body_ast.site, body_stmts, rbrace_site)
        else:
            body = codegen_statement(body_ast, location, scope)

        res = mkSwitch(stmt.site, expr, body)
    return [res]

@statement_dispatcher
def stmt_continue(stmt, location, scope):
    if LoopContext.current is None:
        raise ECONT(stmt.site)
    else:
        return LoopContext.current.continue_(stmt.site)

@statement_dispatcher
def stmt_break(stmt, location, scope):
    if LoopContext.current is None:
        raise EBREAK(stmt.site)
    else:
        return LoopContext.current.break_(stmt.site)

def eval_call_stmt(method_ast, location, scope):
    '''Given a call (or inline) AST node, deconstruct it and eval the
    method reference and all parameters.'''
    expr = codegen_expression_maybe_nonvalue(method_ast, location, scope)
    if isinstance(expr, NonValue) and not isinstance(
            expr, (TraitMethodRef, NodeRef)):
        raise expr.exc()
    return expr

def verify_args(site, inp, outp, inargs, outargs):
    '''Verify that the given arguments can be used when calling or
    inlining method'''
    if len(inargs) != len(inp):
        raise EARG(site, 'input')
    if len(outargs) != len(outp):
        if dml.globals.dml_version == (1, 2):
            raise EARG(site, 'output')
        else:
            raise ERETLVALS(site, len(outp), len(outargs))
    for arg in outargs:
        if not arg.writable:
            report(EASSIGN(site, arg))
            return False
    return True

def mkcall_method(site, func, indices):
    for i in indices:
        if isinstance(i, NonValue):
            raise i.exc()
    from .crep import dev
    if crep.TypedParamContext.active and func.independent:
        raise ETYPEDPARAMVIOL(site)

    devarg = ([] if func.independent
              else [mkLit(site, dev(site),
                          TDevice(crep.structtype(dml.globals.device)))])
    return lambda args: mkApply(
        site, func.cfunc_expr(site), devarg + list(indices) + args)

def common_inline(site, method, indices, inargs, outargs):
    if not verify_args(site, method.inp, method.outp, inargs, outargs):
        return mkNull(site)

    if dml.globals.debuggable:
        if method.fully_typed and (
                compat.dml12_inline not in dml.globals.enabled_compat
                or all(not arg.constant for arg in inargs)):
            # call method instead of inlining it
            func = method_instance(method)
        else:
            # create a specialized method instance based on parameter
            # types, and call that
            intypes = tuple(
                arg if ((ptype is None
                         or compat.dml12_inline in dml.globals.enabled_compat)
                        and (arg.constant or undefined(arg)))
                else methfunc_param(ptype, arg)
                for ((pname, ptype), arg) in zip(method.inp, inargs))
            outtypes = tuple(methfunc_param(ptype, arg)
                             for ((pname, ptype), arg)
                             in zip(method.outp, outargs))
            func = untyped_method_instance(method, (intypes, outtypes))
        mark_method_referenced(func)

        # Filter out inlined arguments
        used_args = [i for (i, (n, t)) in enumerate(func.inp)
                     if isinstance(t, DMLType)]
        inargs = [inargs[i] for i in used_args]
        inp = [func.inp[i] for i in used_args]

        return codegen_call_stmt(site, func.method.name,
                                 mkcall_method(site, func, indices),
                                 inp, func.outp, func.throws, inargs, outargs)

    if not method.independent:
        crep.require_dev(site)

    with NoLoopContext():
        res = codegen_inline(site, method, indices, inargs, outargs)
    return res

def in_try_block(location):
    '''Return whether we are currently protected by a try block
    within the currently dispatched method.'''

    handler = Failure.fail_stack[-1]
    return (isinstance(handler, CatchFailure)
            # Note when inlining a method, the fail handler can be a
            # CatchFailure from the calling method; we detect this by
            # comparing method nodes.  There are theoretical cases
            # where this comparison is insufficient; a method can
            # inline itself within a try block. But that never happens
            # in practice, and I don't even know if it can cause any
            # problems in theory.
            and handler.method == location.method())

def dml12_method_throws_in_dml14(meth_node):
    return (meth_node.parent.objtype, meth_node.name) in {
        ('attribute', 'set'),
        ('bank', 'read_access'),
        ('bank', 'write_access')}

def mark_method_invocation(call_site, method, location):
    '''Mark that a given method is called from a certain location.
    This is used in calls between 1.4->1.2: DML 1.4 requires in general that
    methods that don't throw are marked as nothrow, but most 1.2
    methods are not supposed to throw an exception, even if they have nothrow '''
    if method.site.dml_version() == (1, 2):
        if dml12_method_throws_in_dml14(method):
            # some methods will be converted to 'throws' when moving
            # to 1.4; these will eventually need encapsulation in try
            # blocks, so we count them as throwing.
            EBADFAIL_dml12.throwing_methods[location.method()] = call_site
        else:
            # ordinary 1.2 method: will count as throwing if it
            # actually throws, or (recursively) if it calls a method
            # that does. This analysis is done by EBADFAIL_dml12.all_errors().
            EBADFAIL_dml12.uncaught_method_calls.setdefault(
                method, []).append((call_site, location.method()))
    else:
        # 1.4 methods marked as 'throws' count as throwing even if they don't,
        # because they will need a try block
        EBADFAIL_dml12.throwing_methods[location.method()] = call_site

@statement_dispatcher
def stmt_inline(stmt, location, scope):
    (method_ast, inarg_asts, outarg_asts) = stmt.args
    assert dml.globals.dml_version == (1, 2)
    inargs = [codegen_expression_maybe_nonvalue(arg, location, scope)
              for arg in inarg_asts]
    for e in inargs:
        if isinstance(e, NonValue) and not undefined(e):
            raise e.exc()
    outargs = [codegen_expression(arg, location, scope)
               for arg in outarg_asts]
    expr = eval_call_stmt(method_ast, location, scope)
    if isinstance(expr, NodeRef):
        (method, indices) = expr.get_ref()
        if method.objtype != 'method':
            raise ENMETH(stmt.site, expr)
        if not in_try_block(location) and method.throws:
            mark_method_invocation(expr.site, method, location)
        return [common_inline(stmt.site, method, indices, inargs, outargs)]
    else:
        raise ENMETH(stmt.site, expr)

@statement_dispatcher
def stmt_call(stmt, location, scope):
    (method_ast, inarg_asts, outarg_asts) = stmt.args
    assert dml.globals.dml_version == (1, 2)
    inargs = [codegen_expression(arg, location, scope)
              for arg in inarg_asts]
    outargs = [codegen_expression(arg, location, scope)
               for arg in outarg_asts]
    expr = eval_call_stmt(method_ast, location, scope)
    if isinstance(expr, NodeRef):
        (method, indices) = expr.get_ref()
        if method.objtype != 'method':
            raise ENMETH(stmt.site, expr)
        if not in_try_block(location) and method.throws:
            mark_method_invocation(expr.site, method, location)
        return [codegen_call(stmt.site, method, indices, inargs, outargs)]
    elif isinstance(expr, TraitMethodRef):
        if not in_try_block(location) and expr.throws:
            # Shared methods marked as 'throws' count as
            # unconditionally throwing
            EBADFAIL_dml12.throwing_methods[location.method()] = expr.site
        return [codegen_call_traitmethod(stmt.site, expr, inargs, outargs)]
    else:
        raise ENMETH(stmt.site, expr)

# Context manager that protects from recursive inlining
class RecursiveInlineGuard(object):
    # method nodes for the currently applied methods, possibly nested
    stack = set()

    def __init__(self, site, meth_node):
        self.site = site
        self.meth_node = meth_node
    def __enter__(self):
        if self.meth_node in self.stack:
            raise ERECUR(self.site, self.meth_node)
        self.stack.add(self.meth_node)
    def __exit__(self, exc_type, exc_val, exc_tb):
        self.stack.remove(self.meth_node)

def codegen_inline_byname(node, indices, meth_name, inargs, outargs,
                          site,
                          inhibit_copyin = False):
    assert isinstance(node, objects.DMLObject)
    assert isinstance(indices, tuple)
    assert isinstance(meth_name, str)

    if meth_name:
        meth_node = node.get_component(meth_name, 'method')
        if not meth_node:
            raise UnknownMethod(site, meth_name)
    else:
        meth_node = node

    return codegen_inline(site, meth_node, indices, inargs, outargs,
                          inhibit_copyin)

intercepted_in_int_reg = {
    'get_name': int_register.codegen_get_name,
    'get_number': int_register.codegen_get_number,
    'read': int_register.codegen_read,
    'write': int_register.codegen_write
}
intercepted_in_bank = {
    '_read_one_reg': 'codegen_read_access',
    '_write_one_reg': 'codegen_write_access'
}

def intercepted_method(meth_node):
    if (meth_node.name in intercepted_in_int_reg
        and meth_node.parent.objtype == 'implement'
        and meth_node.parent.name == 'int_register'
        and meth_node.parent.parent.objtype == 'bank'):
        return intercepted_in_int_reg[meth_node.name]
    if (meth_node.name in intercepted_in_bank
        and meth_node.parent.objtype == 'bank'):
        from . import io_memory
        return getattr(io_memory, intercepted_in_bank[meth_node.name])
    return False

def report_pevent_data_arg(meth_node, site, inargs):
    if (meth_node.parent.objtype == 'event'
        and meth_node.site.dml_version() == (1, 2)
        and meth_node.name in {'post', 'posted', 'next', 'remove'}):
        from . import structure
        if structure.method_is_std(meth_node.parent, 'get_event_info'):
            report(PEVENT_NO_ARG(dmlparse.start_site(inargs[-1].site),
                                   dmlparse.end_site(site)))
        elif (isinstance(inargs[-1], ctree.Cast)
              and safe_realtype(inargs[-1].expr.ctype()).is_int):
            report(PEVENT_UINT64_ARG(
                inargs[-1].site, dmlparse.end_site(inargs[-1].site),
                meth_node.parent.site))
            event_meth_node = meth_node.parent.get_component('event')
            (argname, _) = event_meth_node.inp[0]
            report(PCHANGE_INARGS(event_meth_node.site,
                                  f'method event(uint64 {argname})'))
            for methname in ['get_event_info', 'set_event_info']:
                meth = meth_node.parent.get_component(methname)
                report(PEVENT_REMOVE_INFO(meth.site, dmlparse.end_site(meth.site)))

def codegen_inline(site, meth_node, indices, inargs, outargs,
                   inhibit_copyin = False):
    assert isinstance(meth_node, objects.DMLObject)
    PWUNUSED.inlined_methods.add(meth_node.site)

    if len(inargs) != len(meth_node.inp):
        raise ICE(meth_node, "wrong number of inargs")
    if len(outargs) != len(meth_node.outp):
        raise ICE(meth_node, "wrong number of outargs")

    if meth_node.throws and not Failure.fail_stack[-1].allowed:
        raise EBADFAIL(site)

    if (site.dml_version() == (1, 2) and logging.show_porting):
        report_pevent_data_arg(meth_node, site, inargs)

    meth_node.refcount += 1        # regard method as used

    # Open the scope
    # TODO: in python 3.10 we can use parentheses instead of \
    with RecursiveInlineGuard(site, meth_node),  \
         ErrorContext(meth_node, site),          \
         contextlib.nullcontext() if meth_node.throws else NoFailure(site):
        param_scope = MethodParamScope(global_scope)
        param_scope.add(meth_node.default_method.default_sym(indices))

        if intercepted_method(meth_node):
            # Inlining an intercepted method would yield an
            # error. These methods are fully typed, so converting to a
            # call is safe.
            return codegen_call(site, meth_node, indices,
                                inargs, outargs)
        for (arg, (parmname, parmtype), argno) in zip(inargs, meth_node.inp,
                                                      list(range(len(inargs)))):
            # Create an alias
            if parmtype:
                if undefined(arg):
                    raise arg.exc()
                argtype  = arg.ctype()
                if not argtype:
                    raise ICE(arg.site, "unknown expression type")
                parmt = safe_realtype(parmtype)
                argt = safe_realtype(argtype)
                (ok, trunc, constviol) = parmt.canstore(argt)
                if not ok:
                    raise EARGT(site, 'inline', meth_node.name,
                                arg.ctype(), parmname, parmtype, 'input')

                if constviol:
                    raise ECONSTP(site, parmname, "method call")
                arg = coerce_if_eint(arg)

            if inhibit_copyin or undefined(arg):
                param_scope.add(ExpressionSymbol(parmname, arg, arg.site))
            elif arg.constant and (
                    parmtype is None
                    or compat.dml12_inline in dml.globals.enabled_compat):
                # Constants must be passed directly to
                # provide constant folding.  Other values are stored in a
                # local variable to improve type checking and variable
                # scoping.
                inlined_arg = mkInlinedParam(site, arg, parmname,
                                             parmtype or arg.ctype())
                param_scope.add(ExpressionSymbol(
                    parmname, inlined_arg, site))
            else:
                param_scope.add_variable(parmname,
                                         type = parmtype or arg.ctype(),
                                         site = arg.site,
                                         init = ExpressionInitializer(arg))
                arg.decref()

        if meth_node.astcode.site.dml_version() == (1, 2):
            if inhibit_copyin:
                # inhibit_copyin also inhibits copyout
                for (arg, (parmname, parmtype)) in zip(outargs, meth_node.outp):
                    param_scope.add(ExpressionSymbol(parmname, arg, site))
                copyout = []
            else:
                outvars = [
                    add_proxy_outvar(
                        meth_node.site, parmname,
                        parmtype if parmtype else arg.ctype(),
                        param_scope)
                    for (arg, (parmname, parmtype))
                    in zip(outargs, meth_node.outp)]
                copyout = [
                    copy_outarg(arg, var, parmname,
                                parmtype if parmtype else arg.ctype(),
                                meth_node.name)
                    for (arg, var, (parmname, parmtype)) in zip(
                            outargs, outvars, meth_node.outp)]
            exit_handler = GotoExit_dml12()
            with exit_handler:
                code = [codegen_statement(meth_node.astcode,
                                          Location(meth_node, indices),
                                          param_scope)]
            post = ([mkLabel(site, exit_handler.label)]
                    if exit_handler.used else [])
            post.extend(copyout)
            return mkInlinedMethod(site, meth_node, declarations(param_scope),
                                   code, post)
        else:
            assert meth_node.astcode.kind == 'compound'
            [subs, rbrace_site] = meth_node.astcode.args
            location = Location(meth_node, indices)
            exit_handler = GotoExit_dml14(outargs)
            with exit_handler:
                code = codegen_statements(subs, location, param_scope)
            decls = declarations(param_scope)
            post = ([mkLabel(site, exit_handler.label)]
                    if exit_handler.used else [])
            body = mkCompound(site, decls + code, rbrace_site)
            if meth_node.outp and body.control_flow().fallthrough:
                report(ENORET(meth_node.astcode.site))
            return mkInlinedMethod(site, meth_node, decls, code, post)

def c_rettype(outp, throws):
    if throws:
        return TBool()
    elif outp:
        (_, rettype) = outp[0]
        return rettype
    else:
        return TVoid()

def c_inargs(inp, outp, throws):
    '''Return the signature of the C function representing a DML method,
    on the form (outtype, [arg1, ...]), where each arg is a pair
    (name, type). inp includes any implicit arguments
    (device struct pointer, indices, etc)'''
    if throws:
        return inp + [(n, TPtr(t)) for (n, t) in outp]
    elif outp:
        return inp + [(n, TPtr(t)) for (n, t) in outp[1:]]
    else:
        return list(inp)

# TODO is startup a necessary member?
class MethodFunc(object):
    '''A concrete method instance, where all parameters are fully
    typed. One MethodFunc corresponds to one generated C function. A
    fully typed method will always yield a single MethodFunc. An
    incompletely typed method may generate multiple MethodFunc
    instances, one for each set of parameter types it is inlined
    with. When a method is inlined with a constant parameter, this
    will result in a separate MethodFunc instance with the constant
    parameter removed from the signature, and the constant propagated
    into the method body.'''
    __slots__ = ('method', 'inp', 'outp', 'throws', 'independent', 'startup',
                 'memoized', 'cparams', 'rettype', 'suffix')

    def __init__(self, method, inp, outp, throws, independent, startup,
                 memoized, cparams, suffix):
        '''(inp, outp, throws) describe the method's signature; cparams
        describe the generated C function parameters corresponding to
        inp. If some method parameters are constant propagated, then
        the corresponding method parameter is on the form (name,
        value), instead of (name, type), and the corresponding C
        function parameter is omitted.'''

        self.method = method

        self.inp = tuple(inp)
        self.outp = tuple(outp)
        self.throws = throws
        self.independent = independent
        self.startup = startup
        self.memoized = memoized
        self.suffix = suffix

        # rettype is the return type of the C function
        self.rettype = c_rettype(outp, throws)
        self.cparams = c_inargs(
            implicit_params(method) + list(cparams), outp, throws)

    @property
    def prototype(self):
        return self.rettype.declaration(
            "%s(%s)" % (self.get_cname(),
                        ", ".join([t.declaration(n)
                                   for (n, t) in self.cparams])))

    def cfunc_expr(self, site):
        return mkLit(site, self.get_cname(), self.cfunc_type)

    @property
    def cfunc_type(self):
        return TFunction([t for (_, t) in self.cparams], self.rettype)

    def get_name(self):
        '''textual description of method, used in comment'''
        name = self.method.logname()
        if self.suffix:
            name += " (specialized)"
        return name

    def get_cname(self):
        base = crep.cref_method(self.method)
        return '_DML_M_' + base + self.suffix

def canonicalize_signature(signature):
    "Make a signature hashable"
    # The problem is that the same type can be represented by
    # different DMLType objects. Use an ugly trick and canonicalize to
    # the string representation.
    (intypes, outtypes) = signature
    return (tuple(t.value if isinstance(t, Expression) and t.constant
                  else str(t) for t in intypes),
            tuple(str(t) for t in outtypes))

def implicit_params(method):
    return (crep.maybe_dev_arg(method.independent)
            + [('_idx%d' % i, TInt(32, False))
               for i in range(method.dimensions)])

def untyped_method_instance(method, signature):
    """Return the MethodFunc instance for a given signature"""
    canon_signature = canonicalize_signature(signature)
    # Idempotency; this can be called repeatedly for the same method.
    if canon_signature in method.funcs:
        return method.funcs[canon_signature]

    (intypes, outtypes) = signature
    inp = [(arg, stype)
           for stype, (arg, etype) in zip(intypes, method.inp)]
    assert all(isinstance(t, DMLType) for t in outtypes)
    outp = [(arg, stype)
            for stype, (arg, etype) in zip(outtypes, method.outp)]

    cparams = [(n, t) for (n, t) in inp if isinstance(t, DMLType)]

    func = MethodFunc(method, inp, outp, method.throws, method.independent,
                      method.startup, method.memoized, cparams,
                      "__"+str(len(method.funcs)))

    method.funcs[canon_signature] = func
    return func

def method_instance(method):
    """Return the MethodFunc instance for a typed method"""
    assert method.fully_typed
    if None in method.funcs:
        return method.funcs[None]

    func = MethodFunc(method, method.inp, method.outp, method.throws,
                      method.independent, method.startup, method.memoized,
                      method.inp, "")

    method.funcs[None] = func
    return func

def codegen_method_func(func):
    """Return the function body of the C function corresponding to a
    specific instance of a method defined directly in the device tree"""
    method = func.method

    indices = tuple(mkLit(method.site, '_idx%d' % i, TInt(32, False),
                          str=dollar(method.site) + "%s" % (idxvar,))
                    for (i, idxvar) in enumerate(method.parent.idxvars()))
    intercepted = intercepted_method(method)
    if intercepted:
        assert method.throws
        with crep.DeviceInstanceContext():
            return intercepted(
                method.parent, indices,
                [mkLit(method.site, n, t) for (n, t) in func.inp],
                [mkLit(method.site, "*%s" % n, t) for (n, t) in func.outp],
                SimpleSite(method.site.loc()))
    inline_scope = MethodParamScope(global_scope)
    for (name, e) in func.inp:
        if dml.globals.dml_version == (1, 2) and (
                compat.dml12_misc not in dml.globals.enabled_compat):
            check_varname(method.site, name)
        if isinstance(e, Expression):
            inlined_arg = (
                mkInlinedParam(method.site, e, name, e.ctype())
                if defined(e) else e)
            inline_scope.add(ExpressionSymbol(name, inlined_arg, method.site))
    inp = [(n, t) for (n, t) in func.inp if isinstance(t, DMLType)]

    with ErrorContext(method):
        location = Location(method, indices)
        if func.memoized:
            assert func.independent and func.startup
            memoization = IndependentMemoized(func)
        else:
            memoization = None
        code = codegen_method(
            method.site, inp, func.outp, func.throws, func.independent,
            memoization, method.astcode,
            method.default_method.default_sym(indices),
            location, inline_scope, method.rbrace_site)
    return code

def codegen_return(site, outp, throws, retvals):
    '''Generate code for returning from a method with a given list of
    return values'''
    if len(outp) != len(retvals):
        report(ERETARGS(site, len(outp), len(retvals)))
        # avoid control flow errors by falling back to statement with
        # no fall-through
        return mkAssert(site, mkBoolConstant(site, False))
    if throws:
        ret = mkReturn(site, mkBoolConstant(site, False))
    elif outp:
        (_, t) = outp[0]
        ret = mkReturn(site, retvals[0], t)
    else:
        ret = mkReturn(site, None)
    stmts = []
    return_first_outarg = bool(not throws and outp)
    for ((name, typ), val) in itertools.islice(
            zip(outp, retvals), return_first_outarg, None):
        if (return_first_outarg and site.dml_version() == (1, 2)):
            # Avoid outputting "*x = *x" for nothrow methods in 1.2
            assert isinstance(val, ctree.Dereference)
            assert isinstance(val.rh, ctree.Lit)
            assert val.rh.str == name
            continue
        stmts.append(mkCopyData(site, val, mkLit(site, "*%s" % (name,), typ)))
    stmts.append(ret)
    return mkCompound(site, stmts)

def codegen_method(site, inp, outp, throws, independent, memoization, ast,
                   default, location, fnscope, rbrace_site):
    with (crep.DeviceInstanceContext() if not independent
          else contextlib.nullcontext()):
        for (arg, etype) in inp:
            fnscope.add_variable(arg, type=etype, site=site, make_unique=False)
        initializers = [get_initializer(site, parmtype, None, None, None)
                        for (_, parmtype) in outp]

        fnscope.add(default)

        if memoization:
            prelude = memoization.prelude
            fail_handler = memoization.fail_handler()
            exit_handler = memoization.exit_handler()
        else:
            def prelude():
                return []
            fail_handler = (ReturnFailure(rbrace_site) if throws
                            else NoFailure(site))
            exit_handler = (GotoExit_dml12()
                            if ast.site.dml_version() == (1, 2)
                            else ReturnExit(outp, throws))

        if ast.site.dml_version() == (1, 2):
            if throws:
                # Declare and initialize one variable for each output
                # parameter.  We cannot write to the output parameters
                # directly, because they should be left untouched if an
                # exception is thrown.
                code = []
                for ((varname, parmtype), init) in zip(outp, initializers):
                    sym = fnscope.add_variable(
                        varname, type=parmtype, init=init, make_unique=True,
                        site=ast.site)
                    sym.incref()
                    code.append(sym_declaration(sym))
            else:
                if outp:
                    # pass first out argument as return value
                    (name, typ) = outp[0]
                    sym = fnscope.add_variable(name, typ, site=site,
                                               init=initializers[0],
                                               make_unique=False)
                    sym.incref()
                    code = [sym_declaration(sym)]
                    for ((name, typ), init) in zip(outp[1:], initializers[1:]):
                        # remaining output arguments pass-by-pointer
                        param = mkDereference(site,
                                              mkLit(site, name, TPtr(typ)))
                        fnscope.add(ExpressionSymbol(name, param, site))
                        code.append(mkAssignStatement(site, param, init))
                else:
                    code = []

            with fail_handler, exit_handler:
                code.append(codegen_statement(ast, location, fnscope))
            if exit_handler.used:
                code.append(mkLabel(rbrace_site, exit_handler.label))
            code.append(codegen_return(rbrace_site, outp, throws, [
                lookup_var(site, fnscope, varname) for (varname, _) in outp]))
            to_return = mkCompound(site, code)
        else:
            # manually deconstruct compound AST node, to make sure
            # top-level locals share scope with parameters
            assert ast.kind == 'compound'
            [subs, _] = ast.args
            with fail_handler, exit_handler:
                body = ([mkIndicesAssert(site, location.node,
                                         location.indices)]
                        if ((compat.no_method_index_asserts
                             not in dml.globals.enabled_compat)
                            and location.method() and location.node.dimsizes)
                        else [])
                body.extend(prelude())
                body.extend(codegen_statements(subs, location, fnscope))
                code = mkCompound(site, body)
                if code.control_flow().fallthrough:
                    if outp:
                        report(ENORET(site))
                    else:
                        code = mkCompound(site,
                                          body + [codegen_exit(rbrace_site,
                                                               [])],
                                          rbrace_site)
            to_return = code
        return to_return

# Keep track of methods that we need to generate code for
def mark_method_referenced(func):
    cnt = referenced_methods.get(func, 0)
    cnt += 1
    referenced_methods[func] = cnt
    func.method.refcount += 1
    if cnt == 1:
        method_queue.append(func)

def mark_method_exported(func, name, export_site):
    # name -> func instances -> export statement sites
    if name in exported_methods:
        (otherfunc, othersite) = exported_methods[name]
        report(ENAMECOLL(export_site, othersite, name))
    else:
        if (export_site.dml_version() != (1, 2)
            and not func.independent):
            # extern trampolines for 1.4 methods rely on static trampolines
            # for non-independent methods. Thus they must also be statically
            # exported.
            mark_method_statically_exported(func)
        exported_methods[name] = (func, export_site)

def mark_method_statically_exported(func):
    statically_exported_methods.add(func)

def methfunc_param(ptype, arg):
    if ptype:
        return ptype
    # Special case, normally endian integer inargs or outargs are not allowed,
    # so we pretend its a regular integer here and count on coercion
    # to handle the translation
    realargtype = realtype(arg.ctype())
    if realargtype.is_int and realargtype.is_endian:
        return TInt(realargtype.bits, realargtype.signed,
                    realargtype.members, realargtype.const)
    return arg.ctype()

def require_fully_typed(site, meth_node):
    if not meth_node.fully_typed:
        for (parmname, parmtype) in meth_node.inp:
            if not parmtype:
                raise ENARGT(meth_node.site, parmname, 'input', site)
        for (parmname, parmtype) in meth_node.outp:
            if not parmtype:
                raise ENARGT(meth_node.site, parmname, 'output', site)
        raise ICE(site, "no missing parameter type")

def codegen_call_expr(site, meth_node, indices, inits, location, scope):
    require_fully_typed(site, meth_node)
    func = method_instance(meth_node)
    mark_method_referenced(func)
    args = typecheck_inarg_inits(site, inits, func.inp, location, scope,
                                 'method')
    return mkcall_method(site, func, indices)(args)

def codegen_call_traitmethod(site, expr, inargs, outargs):
    if not isinstance(expr, TraitMethodRef):
        raise ICE(site, "cannot call %r: not a trait method" % (expr,))
    if not verify_args(site, expr.inp, expr.outp, inargs, outargs):
        return mkNull(site)
    def mkcall(args):
        rettype = c_rettype(expr.outp, expr.throws)
        # implicitly convert endian int arguments to integers
        args = [coerce_if_eint(arg) for arg in args]
        return expr.call_expr(args, rettype)
    return codegen_call_stmt(site, str(expr), mkcall, expr.inp, expr.outp,
                             expr.throws, inargs, outargs)

def codegen_call(site, meth_node, indices, inargs, outargs):
    '''Generate a call using a direct reference to the method node'''
    if not verify_args(site, meth_node.inp, meth_node.outp, inargs, outargs):
        return mkNull(site)
    require_fully_typed(site, meth_node)
    func = method_instance(meth_node)

    if (site.dml_version() == (1, 2) and logging.show_porting):
        report_pevent_data_arg(meth_node, site, inargs)

    if compat.dml12_misc in dml.globals.enabled_compat:
        # For backward compatibility. See bug 21367.
        inargs = [mkCast(site, arg, TPtr(TNamed('char')))
                  if isinstance(arg, StringConstant) else arg
                  for arg in inargs]

    mark_method_referenced(func)
    return codegen_call_stmt(site, func.method.name,
                             mkcall_method(site, func, indices),
                             func.inp, func.outp, func.throws, inargs, outargs)

def codegen_call_byname(site, node, indices, meth_name, inargs, outargs):
    '''Generate a call using the parent node and indices, plus the method
    name.  For convenience.'''
    assert isinstance(node, objects.DMLObject)
    assert isinstance(meth_name, str)

    meth_node = node.get_component(meth_name, 'method')
    if not meth_node:
        raise UnknownMethod(node, meth_name)
    return codegen_call(site, meth_node, indices, inargs, outargs)

def copy_outarg(arg, var, parmname, parmtype, method_name):
    '''Type-check the output argument 'arg', and create a local
    variable with that type in scope 'callscope'. The address of this
    variable will be passed as an output argument to the C function
    generated by the method.

    This is needed to protect the output parameter from being
    modified, in case a method clobbers the parameter and then throws
    an exception. We would be able to skip the proxy variable for
    calls to non-throwing methods when arg.ctype() and parmtype are
    equivalent types, but we don't do this today.'''
    argtype = arg.ctype()

    if not argtype:
        raise ICE(arg.site, "unknown expression type")
    else:
        ok, trunc, constviol = realtype(parmtype).canstore(realtype(argtype))
        if not ok:
            raise EARGT(arg.site, 'call', method_name,
                         arg.ctype(), parmname, parmtype, 'output')

    return mkCopyData(var.site, var, arg)

def add_proxy_outvar(site, parmname, parmtype, callscope):
    varname = parmname
    varinit = get_initializer(site, parmtype, None, None, None)
    sym = callscope.add_variable(varname, type=parmtype, init=varinit, site=site)
    return mkLocalVariable(site, sym)

def codegen_call_stmt(site, name, mkcall, inp, outp, throws, inargs, outargs):
    '''Generate a statement for calling a method'''
    if len(outargs) != len(outp):
        raise ICE(site, "wrong number of outargs")

    return_first_outarg = bool(not throws and outp)

    callscope = Symtab(global_scope)

    # Add proxy output variables if needed. This is needed e.g. if
    # an uint8 variable is passed in an uint32 output parameter.
    postcode = []
    outargs_conv = []
    for (arg, (parmname, parmtype)) in zip(
            outargs[return_first_outarg:], outp[return_first_outarg:]):
        # It would make sense to pass output arguments directly, but
        # the mechanisms to detect whether this is safe are
        # broken. See SIMICS-9504.
        # if (isinstance(arg, (
        #         Variable, ctree.Dereference, ctree.ArrayRef, ctree.SubRef))
        #     and TPtr(parmtype).canstore(TPtr(arg.ctype()))):
        #     outargs_conv.append(mkAddressOf(arg.site, arg))
        # else:
        var = add_proxy_outvar(site, '_ret_' + parmname, parmtype,
                               callscope)
        outargs_conv.append(mkAddressOf(var.site, var))
        postcode.append(copy_outarg(arg, var, parmname, parmtype, name))

    typecheck_inargs(site, inargs, inp, 'method')
    call_expr = mkcall(list(inargs) + outargs_conv)

    if throws:
        if not Failure.fail_stack[-1].allowed:
            raise EBADFAIL(site)
        call_stmt = mkIf(site, call_expr, Failure.fail_stack[-1].fail(site))
    else:
        if outargs:
            call_stmt = mkCopyData(site, call_expr, outargs[0])
        else:
            call_stmt = mkExpressionStatement(site, call_expr)

    return mkCompound(site, declarations(callscope) + [call_stmt] + postcode)

ctree.codegen_call_expr = codegen_call_expr
