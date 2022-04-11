# © 2021-2022 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

# Generates Unrolled Device Info

__all__ = ('generate',)

import itertools
import json
from . import ctree, crep, expr_util, types
from . import logging
from .expr import mkLit
from .logging import dollar
from .codegen import eval_initializer
from .symtab import global_scope
import dml.globals

def array_info(obj):
    return list(zip(obj._arraylens, obj._idxvars))

def enc(expr):
    if expr.constant:
        val = expr.value
        if isinstance(val, list):
            return [enc(e) for e in val]
        elif isinstance(val, bytes):
            return val.decode("utf-8")
        else:
            return val
    else:
        return str(expr)

def en_parameter(node, indices = []):
    try:
        expr = node.get_expr(tuple(indices))
    except logging.DMLError:
        import os, sys, traceback
        if os.getenv('DMLC_DEBUG'):
            sys.stderr.write("error encoding parameter: %s" % node)
            traceback.print_exc()
        return None
    else:
        return {node.name : enc(expr)}

def evaluate_init_value(node, indices):
    if node.astinit:
        return enc(eval_initializer(
            node.site, node._type, node.astinit,
            ctree.Location(node.parent, tuple(indices)),
            global_scope, True).as_expr(node._type))
    else:
        return None

def en_data(node, indices = []):
    return {node.name : { "init_value" : evaluate_init_value(node, indices) }}

def en_var(node, indices = []):
    return {node.name : { "type" : str(node._type),
                          "init_value" : evaluate_init_value(node, indices) }}

def en_compobj(node, indices = []):
    def do_enc(node, proper_indices):
        content = {}
        for trait in node.traits.ancestors:
            content.setdefault("templates", []).append(trait.name)
        en_subobjs(content, node, proper_indices)
        return content

    new_indices = [[ctree.mkIntegerConstant(node.site, i, False)
                    for i in range(arrsize)] for arrsize in node._arraylens]
    if new_indices:
        new_indices = itertools.product(*new_indices)
        expanded = {}
        for extra_indices in new_indices:
            full_name = "{}{}".format(node.name,
                                      "".join("[{}]".format(enc(ind)) for
                                              ind in extra_indices))
            expanded[full_name] = do_enc(node, indices + list(extra_indices))
        return expanded
    else:
        content = {}
        en_subobjs(content, node, indices)
        return {node.name : do_enc(node, indices)}

obj_encoder_map = {
    'attribute' : ("attributes", en_compobj),
    'bank'      : ("banks", en_compobj),
    'connect'   : ("connects", en_compobj),
    'data'      : ("data", en_data),
    'field'     : ("fields", en_compobj),
    'group'     : ("groups", en_compobj),
    'implement' : ("implements", en_compobj),
    'parameter' : ("parameters", en_parameter),
    'register'  : ("registers", en_compobj),
    'session'   : ("data", en_var),
    'saved'     : ("data", en_var)
}

def en_obj(output, obj, indices = []):
    if obj.objtype in obj_encoder_map:
        (collection, encoder) = obj_encoder_map[obj.objtype]
        subobjdict = encoder(obj, indices)
        output.setdefault(collection, {}).update(subobjdict)

def subobjs(node):
    for s in node.get_components():
        # skip implicit field
        if s.objtype == 'field' and not s.name:
            continue
        # skip "internal" objects
        if s.name.startswith("_"):
            continue

        # skip auto and non-interested parameters
        if s.objtype == 'parameter':
            if s.name in ('this', 'name', 'qname', 'parent', 'index',
                          'indexvar', 'shown_desc', 'objtype',
                          'dev', 'bank',
                          'documentation', 'shown_documentation',
                          'limitations', 'shown_limitations',
                          'dml_1_4', "dml_1_2"):
                continue
            if (node.objtype == 'device' and
                s.name in ('obj', 'logobj', 'simics_api_version',
                           'banks', 'simics_bool_is_int')):
                continue
            if (node.objtype == 'bank' and
                s.name in ('mapped_registers', 'unmapped_registers',
                           'numbered_registers')):
                continue
            if (node.objtype == 'register' and
                s.name in ('notinregister', 'fields', '_regname')):
                continue
            if (node.objtype == 'field' and
                s.name in ('notinfield', 'reg',)):
                continue
        yield s

def en_subobjs(output, obj, indices = []):
    for s in subobjs(obj):
        en_obj(output, s, indices)

# layout:
# {classname:
#     DML: <version>,
#     banks: {
#         <bank>: {
#             stuff,
#             registers: {
#                 <register>: {
#                     stuff,
#                     fields: {
#                         <field> : {stuff}
#                     },
#                 },
#             },
#         },
#     }
# }
def generate(classname, device, dml_version, out_file):
    # Setting things up like this, allows easy combination and sorting of
    # devices in later tools
    complete_json = {
        classname : {
            "DML" : dml_version,
        }
    }
    complete_json[classname].update(en_compobj(device)[device.name])
    with open(out_file, "w") as outfile:
        json.dump(complete_json, outfile)
