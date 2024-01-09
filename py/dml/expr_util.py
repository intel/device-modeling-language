# © 2021 Intel Corporation
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


def expr_constvalue(expr, pytype, typestr):
    if isinstance(expr, NonValue):
        raise expr.exc()
    if not expr.constant:
        raise ENCONST(expr.site, expr)
    if not isinstance(expr.value, pytype):
        raise EBTYPE(expr.site, expr.ctype(), typestr)
    return expr.value


def expr_strval(expr):
    value = expr_constvalue(expr, bytes, "string")
    try:
        return value.decode('utf-8')
    except UnicodeDecodeError:
        raise EBTYPE(expr.site, expr.ctype(), 'utf-8 encoded string')


def expr_intval(expr):
    return expr_constvalue(expr, int, "integer")


def param_str(node, name):
    "Return the parameter value as a python string."
    return expr_strval(param_expr(node, name))


def param_str_or_null(node, name):
    expr = param_expr(node, name)
    if dml.globals.dml_version == (1, 2):
        if isinstance(expr, Lit) and expr.cexpr == 'NULL':
            return None
    elif isinstance(expr, NullConstant):
        return None
    try:
        return expr_strval(expr)
    except DMLError as e:
        report(e)
        return None


def param_int(node, name, indices=None):
    "Return the parameter value as a python integer."
    return expr_constvalue(param_expr(node, name, indices), int, "integer")


def param_bool(node, name):
    "Return the parameter value as a boolean."
    return expr_constvalue(param_expr(node, name), bool, "bool")


def coerce_if_eint(expr):
    from .ctree import as_int
    e_type = realtype(expr.ctype())
    if e_type.is_int and e_type.is_endian:
        return as_int(expr)
    return expr
