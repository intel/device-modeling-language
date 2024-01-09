# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

__all__ = ['generate']

import codecs
import dml.globals
from itertools import product
from collections import OrderedDict
from .ctree import (StringConstant, IntegerConstant, mkIntegerLiteral,
                    all_index_exprs, param_str_fixup)
from .expr_util import (
    defined, undefined, param_int, param_defined,
    static_indices)
from .messages import *
from .logging import *

class XMLWriter(object):
    def __init__(self, filename):
        self._f = codecs.open(filename, "w", "utf-8")
        self.indent = 0

    def __enter__(self):
        self.write('<?xml version="1.0" encoding="UTF-8"?>\n')
        return self

    def __exit__(self, typ, val, tb):
        self._f.close()

    def write(self, s):
        self._f.write("  " * self.indent + s)

    def open_element(self, tag, attrs):
        self.write('<%s %s>\n' % (tag, attr2str(attrs)))
        self.indent += 1

    def end_element(self, tag):
        self.indent -= 1
        self.write('</%s>\n' % tag)

    def one_element(self, tag, attrs):
        self.write('<%s %s />\n' % (tag, attr2str(attrs)))

def attr2str(attrs):
    return ' '.join('%s="%s"' % (k, v) for k, v in list(attrs.items()))

def string_param(node, pname, dimsizes):
    pnode = node.get_component(pname)
    assert pnode.objtype == 'parameter'

    # Allowing index variables to appear in the expression,
    # but requires them all evaluated to the same value.
    try:
        val = pnode.get_expr(static_indices(node))
    except DMLError as e:
        report(e)
        return None

    if isinstance(val, StringConstant):
        return val.unicode_value()
    else:
        return None

_attr_escape_tbl = {
    ord('<'): '&lt;',
    ord('>'): '&gt;',
    ord('"'): '&quot;',
    ord("'"): '&apos;',
    ord('&'): '&amp;',
}

def common_attrs(node, name, dimsizes, suppress_vsize=False):
    attrs = OrderedDict()
    if name:
        attrs['name'] = name
    if node.isindexed() and not suppress_vsize:
        attrs['vsize'] = " ".join([str(arrlen)
                                   for arrlen in node.arraylens()])
    if dml.globals.dml_version == (1, 2):
        for i in ('desc', 'documentation', 'limitations'):
            val = string_param(node, i, dimsizes)
            if val is not None:
                attrs[i] = val.translate(_attr_escape_tbl)
    return attrs

def field_info(fmt, node, name, dimsizes, eval_indices):
    dimsizes += node.arraylens()
    attrs = common_attrs(node, name, dimsizes, suppress_vsize=True)
    index_exprs = tuple(mkIntegerLiteral(None, i) for i in eval_indices)
    for param in ['msb', 'lsb']:
        pnode = node.get_component(param)
        assert pnode.objtype == 'parameter'
        expr = pnode.get_expr(index_exprs)
        # Guaranteed earlier on, by structure.register_fields()
        assert isinstance(expr, IntegerConstant)
        attrs[param] = expr.value
    fmt.one_element('field', attrs)

def offset_defined(offs):
    if dml.globals.dml_version == (1, 2):
        return defined(offs)
    else:
        assert isinstance(offs, IntegerConstant)
        return offs.value != 0xffffffffffffffff

def reg_info(fmt, node, name, dimsizes):
    if node.is_confidential():
        return

    dimsizes += node.arraylens()
    pnode = node.get_component('offset')

    all_offs = [o for o in (pnode.get_expr(indices)
                           for indices in all_index_exprs(node))]
    # only process registers with at least one memory mapped offset
    if any(map(offset_defined, all_offs)):
        attrs = common_attrs(node, name, dimsizes)
        xml_offs = ' '.join([str(o) if offset_defined(o)
                             else '-1' for o in all_offs])
        attrs.update(offset=xml_offs, size=param_int(node, 'size'))
        fmt.open_element('register', attrs)
        for n in node.get_components('field'):
            if not n.ident:
                continue
            if n.arraylens():
                for findex in product(*(range(i) for i in n.arraylens())):
                    field_info(fmt, n,
                               n.name + "".join("[%d]" % i for i in findex),
                               dimsizes, (0,) * len(dimsizes) + findex)
            else:
                field_info(fmt, n, n.name, dimsizes, (0,) * len(dimsizes))
        fmt.end_element('register')

def group_info(fmt, node, name, dimsizes):
    fmt.open_element('group', common_attrs(node, name, dimsizes))
    dimsizes += node.arraylens()
    for n in node.get_components('register', 'group'):
        if n.objtype == 'group':
            group_info(fmt, n, n.name, dimsizes)
        else:
            reg_info(fmt, n, n.name, dimsizes)
    fmt.end_element('group')

def bank_info(fmt, node, name):
    if node.is_confidential():
        return
    attrs = common_attrs(node, name, ())
    if dml.globals.dml_version == (1, 2):
        val = string_param(node, 'byte_order', ())
        if isinstance(val, str):
            attrs['byte_order'] = val

    fparam = node.get_component('function')
    if fparam is not None:
        fun_exprs = [fparam.get_expr(indices)
                     for indices in all_index_exprs(node)]
        if all(map(defined, fun_exprs)):
            attrs['function'] = ' '.join(str(e) for e in fun_exprs)

    fmt.open_element('bank', attrs)
    if dml.globals.dml_version == (1, 2):
        indices = node.arraylens()
        for n in node.get_components('register', 'group'):
            if n.objtype == 'group':
                group_info(fmt, n, n.name, indices)
            else:
                reg_info(fmt, n, n.name, indices)
    fmt.end_element('bank')

def dev_info(fmt, node, classname):
    if node.is_confidential():
        fmt.open_element('device', {'name': classname})
    else:
        attrs = common_attrs(node, classname, ())
        attrs.update(bitorder = node.site.bitorder())
        fmt.open_element('device', attrs)
        for n in node.get_components('bank'):
            bank_info(fmt, n, n.name)
    fmt.end_element('device')

def generate(device, filename):
    classname = param_str_fixup(device, 'classname', '-')
    with XMLWriter(filename) as outfile:
        dev_info(outfile, device, classname)
