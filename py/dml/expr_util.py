# © 2021-2022 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

# Various convenience functions for common operations on expressions
import dml.globals
from .logging import report, ICE
from .messages import *
from .types import *
from .expr import *

__all__ = (
    'defined', 'undefined',
    'static_indices',
    'expr_intval',
    'expr_strval',
    'param_expr',
    'param_expr_site',
    'param_defined',
    'param_str',
    'param_str_or_null',
    'param_int',
    'param_bool',
    'coerce_if_eint',
)

def apply(f):
    return f()

def static_indices(node, site=None):
    if site is None:
        site = node.site
    return tuple(StaticIndex(site, idx) for idx in node.idxvars())

def undefined(expr):
    "Check if an expression is Undefined()"
    return expr.undefined

def defined(expr):
    "The same as 'not undefined(expr)'"
    return not undefined(expr)

def param_expr_site(node, name):
    '''Return the site of a parameter's expression'''
    pnode = node.get_component(name)
    assert pnode.objtype == 'parameter'
    return pnode.expr_site()

def param_expr(node, name, indices=None):
    "Return the expression for a parameter."
    pnode = node.get_component(name)
    assert pnode.objtype == 'parameter'
    if indices is None:
        indices = static_indices(pnode.parent)
    return pnode.get_expr(indices)

def param_defined(node, name):
    "Return true if the parameter value is not undefined."
    return defined(param_expr(node, name))

@apply
class _RAISE(object): pass

def expr_constvalue(expr, pytype, typestr, fallback):
    try:
        if isinstance(expr, NonValue):
            raise expr.exc()
        if not expr.constant:
            raise ENCONST(expr.site, expr)
        if not isinstance(expr.value, pytype):
            raise EBTYPE(expr.site, expr.ctype(), typestr)
    except DMLError as e:
        if fallback is _RAISE:
            raise
        else:
            report(e)
            return fallback
    return expr.value

def param_const(node, name, pytype, typestr, fallback, indices=None):
    try:
        expr = param_expr(node, name, indices)
    except EIDXVAR as e:
        if fallback is _RAISE:
            raise
        else:
            report(e)
            return fallback
    return expr_constvalue(expr, pytype, typestr, fallback)

def expr_strval(expr, fallback=_RAISE):
    try:
        value = expr_constvalue(expr, bytes, "string", _RAISE)
        try:
            return value.decode('utf-8')
        except UnicodeDecodeError:
            raise EBTYPE(expr.site, expr.ctype(), 'utf-8 encoded string')
    except DMLError as e:
        if fallback is _RAISE:
            raise
        else:
            report(e)
            return fallback

def expr_intval(expr, fallback=_RAISE):
    return expr_constvalue(expr, int, "integer", fallback)

def param_str(node, name, fallback=_RAISE):
    "Return the parameter value as a python string."
    try:
        expr = param_expr(node, name)
    except EIDXVAR as e:
        if fallback is _RAISE:
            raise
        else:
            report(e)
            return fallback
    return expr_strval(expr, fallback)

def param_str_or_null(node, name):
    expr = param_expr(node, name)
    if dml.globals.dml_version == (1, 2):
        is_null = isinstance(expr, Lit) and expr.cexpr == 'NULL'
    else:
        is_null = isinstance(expr, NullConstant)
    return None if is_null else expr_strval(expr, fallback=None)

def param_int(node, name, fallback=_RAISE, indices=None):
    "Return the parameter value as a python integer."
    return param_const(node, name, int, "integer", fallback, indices)

def param_bool(node, name, fallback=_RAISE):
    "Return the parameter value as a boolean."
    return param_const(node, name, bool, "bool", fallback)

def coerce_if_eint(expr):
    from .ctree import as_int
    e_type = realtype(expr.ctype())
    if e_type.is_int and e_type.is_endian:
        return as_int(expr)
    return expr
