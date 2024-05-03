# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import builtins
from .logging import dbg

class AST(object):
    __slots__ = ('__site', '__args', 'kind')
    site = property(lambda self: self.__site)
    args = property(lambda self: self.__args)
    def __init__(self, kind, site, *args):
        self.kind = kind
        self.__site = site
        self.__args = args
    def __getitem__(self, idx):
        if isinstance(idx, builtins.slice):
            l = [self.kind, self.site] + builtins.list(self.__args)
            return l[idx]
        if idx == 0:
            return self.kind
        if idx == 1:
            return self.site
        return self.__args[idx - 2]
    def __repr__(self):
        return 'ast.%s(%s)' % (self.kind,
                               ",".join(repr(y) for y in self.__args))
    def __getstate__(self):
        return (self.kind, self.__site, self.__args)
    def __setstate__(self, data):
        (self.kind, self.__site, self.__args) = data

astkinds = {
    'after',
    'afteronhook',
    'immediateafter',
    'apply',
    'assert',
    'assign',
    'assign_target_chain',
    'assign_target_tuple',
    'assignop',
    'auto',
    'binop',
    'bitorder',
    'break',
    'call',
    'case',
    'case_dml12',
    'cast',
    'cdecl',
    'compound',
    'conditional',
    'constant',
    'continue',
    'default',
    'default_dml12',
    'delete',
    'dml',
    'dml_typedef',
    'dowhile',
    'each_in',
    'error',
    'expression',
    'extern',
    'extern_typedef',
    'float',
    'footer',
    'for',
    'foreach',
    'foreach_dml12',
    'goto',
    'hashcond',
    'hashforeach',
    'hashif',
    'hashselect',
    'header',
    'hook',
    'if',
    'import',
    'initializer_compound',
    'initializer_designated_struct',
    'initializer_scalar',
    'index',
    'inline',
    'int',
    'in_each',
    'is',
    'label',
    'list',
    'local',
    'log',
    'loggroup',
    'member',
    'method',
    'new',
    'null',
    'object',
    'objectref',
    'param',
    'paramtype',
    'return',
    'return_dml12',
    'select',
    'set',
    'slice',
    'session',
    'saved',
    'string',
    'struct',
    'switch',
    'template',
    'template_dml12',
    'throw',
    'toplevel_if',
    'sharedhook',
    'sharedmethod',
    'try',
    'typeop',
    'undefined',
    'unop',
    'variable',
    'variable_dml12',
    'walrus',
    'warning',
    'while',
    'export',
    }

python_keywords = {'import', 'if', 'while', 'for', 'try', 'assert', 'return',
                   'break', 'continue', 'is', 'object'}

def astclass(name):
    kind = n.rstrip('_')
    def create(site, *args):
        return AST(kind, site, *args)
    create.__name__ = name
    return create

for n in astkinds:
    if n in python_keywords:
        n += '_'
    globals()[n] = astclass(n)

def get(n):
    if n in astkinds:
        if n in python_keywords:
            n += '_'
        return globals()[n]
    else:
        raise KeyError(n)


class ASTDispatchFnError(Exception):
    pass

class astdispatcher(object):
    __slots__ = ('table', 'prefix')
    def __init__(self, prefix):
        self.table = {}
        self.prefix = prefix
    def __call__(self, fn):
        name = fn.__name__
        if not name.startswith(self.prefix):
            raise ASTDispatchFnError(name)
        name = name[len(self.prefix):]
        if not name in astkinds:
            raise ASTDispatchFnError(name)
        self.table[name] = fn
        return fn
    def dispatch(self, tree, *args):
        return self.table[tree.kind](tree, *args)
