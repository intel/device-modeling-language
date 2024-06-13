# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

# Generates DML debugging info

__all__ = ('generate',)

import pickle as pickle
from . import ctree, crep, expr_util, types
from . import logging
from .expr import mkLit
from .logging import dollar
import dml.globals

VERSION = (0, 1)

ID_PARAMETER = 0
ID_METHOD    = 1
ID_DATA      = 2
ID_REGISTER  = 3
ID_FIELD     = 4
ID_BANK      = 5
ID_CONNECT   = 6
ID_PORT      = 7
ID_ATTRIBUTE = 8
ID_EVENT     = 9
ID_IMPLEMENT = 10
ID_INTERFACE = 11
ID_GROUP     = 12
ID_DEVICE    = 13
ID_SUBDEVICE = 14
ID_HOOK      = 15

def array_info(obj):
    return list(zip(obj._arraylens, obj._idxvars))

def en_parameter(node):
    def enc(expr):
        if expr.constant:
            val = expr.value
            if isinstance(val, list):
                return [enc(e) for e in val]
            elif isinstance(val, (int, str, bytes, float, bool)):
                # Not every constant expression can be encoded through .value
                # ObjTraitRef is an example of such an expression class
                return val

        return (str(expr),)

    try:
        with crep.DeviceInstanceContext():
            expr = node.get_expr(tuple(
                mkLit(node.site, dollar(node.site) + idxvar, types.TInt(32, False))
                for idxvar in node.parent.idxvars()))
    except logging.DMLError:
        import os, sys, traceback
        if os.getenv('DMLC_DEBUG'):
            sys.stderr.write("error encoding parameter: %s" % node)
            traceback.print_exc()
        return None
    else:
        return (ID_PARAMETER, node.name, enc(expr))

def en_method(node):
    if not node.funcs:
        return None
    fs = []
    for f in list(node.funcs.values()):
        fs.append((f.get_cname(),
                   f.independent,
                   tuple((n, str(t)) for n, t in f.inp),
                   tuple((n, str(t)) for n, t in f.outp)))
    return (ID_METHOD, node.name, tuple(fs))

def en_data(node):
    return (ID_DATA, node.name, str(node._type))

def en_register(node):
    return (ID_REGISTER, node.name, array_info(node), en_subobjs(node))

def en_field(node):
    return (ID_FIELD, node.name, array_info(node), en_subobjs(node))

def en_bank(node):
    return (ID_BANK, node.name, array_info(node), en_subobjs(node))

def en_connect(node):
    return (ID_CONNECT, node.name, array_info(node), en_subobjs(node))

def en_port(node):
    return (ID_PORT, node.name, array_info(node), en_subobjs(node))

def en_attribute(node):
    return (ID_ATTRIBUTE, node.name, array_info(node), en_subobjs(node))

def en_event(node):
    return (ID_EVENT, node.name, array_info(node), en_subobjs(node))

def en_implement(node):
    return (ID_IMPLEMENT, node.name, en_subobjs(node))

def en_interface(node):
    return (ID_INTERFACE, node.name, en_subobjs(node))

def en_group(node):
    return (ID_GROUP, node.name, array_info(node), en_subobjs(node))

def en_device(node):
    return (ID_DEVICE, node.name, crep.structtype(node), en_subobjs(node))

def en_subdevice(node):
    return (ID_SUBDEVICE, node.name, array_info(node), en_subobjs(node))

def en_hook(node):
    return (ID_HOOK, node.name, node._arraylens,
            [str(typ) for typ in node.msg_types])

obj_encoder_map = {
    'device'    : en_device,
    'bank'      : en_bank,
    'subdevice' : en_subdevice,
    'connect'   : en_connect,
    'port'      : en_port,
    'attribute' : en_attribute,
    'parameter' : en_parameter,
    'method'    : en_method,
    'register'  : en_register,
    'field'     : en_field,
    'data'      : en_data,
    'session'   : en_data,
    'saved'     : en_data,
    'implement' : en_implement,
    'event'     : en_event,
    'interface' : en_interface,
    'group'     : en_group,
    'hook'      : en_hook}

def en_obj(node):
    return obj_encoder_map[node.objtype](node)

def subobjs(node):
    for s in node.get_components():
        # skip auto and non-interested parameters
        if s.objtype == 'parameter':
            if s.name in {'this', 'name', 'qname', 'parent', 'index',
                          'indexvar', '_confidentiality', 'desc', 'shown_desc',
                          'dev', 'bank', 'documentation', 'limitations',
                          'subdevice'}:
                continue
            if (node.objtype == 'device' and
                s.name in {'obj', 'logobj', 'simics_api_version',
                           'banks', 'simics_bool_is_int'}):
                continue
            if (node.objtype == 'bank' and
                s.name in {'mapped_registers', 'unmapped_registers',
                           'numbered_registers'}):
                continue
            if (node.objtype == 'register' and
                s.name in {'notinregister', 'fields', '_regname'}):
                continue
            if (node.objtype == 'field' and
                s.name in {'notinfield', 'reg',}):
                continue
            if (node.objtype == 'event' and
                s.name in {'evclass',}):
                continue
        # skip implicit field
        if s.objtype == 'field' and not s.name:
            continue
        yield s
        if s.objtype == 'method' and s.default_method.node:
            yield s.default_method.node

def en_subobjs(node):
    ret = []
    for s in subobjs(node):
        v = en_obj(s)
        if v is not None:
            ret.append(v)
    return tuple(ret)

def generate(classname, device, dml_version, out_file):
    with open(out_file, "wb") as outfile:
        pickle.dump('DML', outfile, protocol=2)
        pickle.dump(dml_version, outfile, protocol=2)
        pickle.dump(classname, outfile, protocol=2)
        pickle.dump(en_obj(device), outfile, protocol=2)
