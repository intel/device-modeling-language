# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import sys, os
import itertools
import operator
import re
from contextlib import nullcontext
from functools import reduce
from abc import ABC, abstractmethod
import dataclasses
import json
from pathlib import Path

from . import objects, logging, crep, output, ctree, serialize, structure
from . import traits, compat
import dml.globals
from .structure import get_attr_name, port_class_ident, need_port_proxy_attrs
from .logging import *
from .messages import *
from .output import *
from .ctree import *
from .expr import *
from .expr_util import *
from .symtab import *
from .codegen import *
from .types import *
from .set import Set

prototypes = []
c_split_threshold = None

log_object_t = TNamed("log_object_t")
conf_object_t = TNamed("conf_object_t")
attr_value_t = TNamed('attr_value_t')

structfilename = None

def get_attr_flags(obj):
    conf = param_str(obj, 'configuration')
    persist = param_bool(obj, 'persistent')
    internal = (param_bool_fixup(obj, 'internal', True)
                or obj.is_confidential())

    if conf == 'required':
        flags = 'Sim_Attr_Required'
    elif conf == 'optional':
        flags = 'Sim_Attr_Optional'
    elif conf == 'pseudo':
        flags = 'Sim_Attr_Pseudo'
    else:
        raise EPARAM(param_expr_site(obj, "configuration"), "configuration")

    if persist:
        flags += "|Sim_Attr_Persistent"
    if internal:
        flags += "|Sim_Attr_Internal"

    return flags

def get_short_doc(node):
    if dml.globals.dml_version == (1, 2):
        if node.is_confidential():
            return "confidential"
        elif param_defined(node, 'desc'):
            return param_str(node, 'desc')
        else:
            return None
    else:
        return param_str_or_null(node, 'shown_desc')

def get_long_doc(node):
    doc = None
    if param_defined(node, 'documentation'):
        doc = param_str_fixup(node, 'documentation', "")
    # always check desc to catch EIDXVAR even if it isn't used
    desc = get_short_doc(node)
    if doc != None:
        return "confidential" if node.is_confidential() else doc
    else:
        return desc

# This should be output at every point where we are leaving
# a code path controlled by the DML model, and the DML state might have
# changed since last we notified
def output_dml_state_change(device_ref):
    # This is only for 1.4 devices, or 1.2 devices using a specific flag
    if (dml.globals.dml_version == (1, 2) and
        not dml.globals.state_change_dml12):
        return
    out(f"if (unlikely({device_ref}->_has_state_callbacks)) " + "{\n",
        postindent = 1)
    out(f"{crep.cname(dml.globals.device)}_notify_state_change("
        f"{device_ref});\n")
    out("}\n", preindent = -1)

registered_attribute_names = {}
def register_attribute(site, port, name):
    '''remember that attribute 'name' is registered on port 'port',
    and report error if this name was not free.
    'port' is a Bank or Port, or None for device.'''
    global registered_attribute_names

    if name in dml.globals.illegal_attributes:
        report(EANAME(site, name))
    key = (port, name)
    if key in registered_attribute_names:
        report(EATTRCOLL(site, registered_attribute_names[key]))
    registered_attribute_names[key] = site

# Creating the C struct definition is done in two steps. First, the
# structure is built up using the DMLType type, with a TStruct as the
# base.  Then this is printed.

def print_device_substruct(node):
    '''Return the type of the generated C type used to store a node's
    local state, i.e., data objects plus implicit storage. Return None
    if the node needs no storage.'''

    def arraywrap(node, typ):
        for arraylen in reversed(node.dimsizes):
            typ = TArray(typ, mkIntegerLiteral(node.site, arraylen))
        return typ

    def composite_ctype(node, unfiltered_members, label=None):
        '''Helper. Unfiltered_members is a list of (name, type) pairs, where
        type=None entries are ignored.'''
        members = {name: typ for (name, typ) in unfiltered_members if typ}
        if not members:
            return None
        if not label:
            # mangle names so x_y.z and x.y_z give different idents
            label = '__devstruct_' + '_'.join("%d%s" % (s.count('_'), s)
                                    for s in crep.ancestor_cnames(node))
        structtype = TStruct(members, label)
        structtype.print_struct_definition()
        return structtype

    if node.objtype == 'device':
        members = [("obj", conf_object_t)]
        for (v, _) in dml.globals.static_vars:
            members.append((v.value, v.type))
        members.append(('_immediate_after_state',
                        TPtr(TNamed('_dml_immediate_after_state_t'))))
        return composite_ctype(node,
                               members + [(crep.cname(sub), print_device_substruct(sub))
                                          for sub in node.get_components()],
                               label=crep.cname(node))

    elif ((node.objtype == 'session' or node.objtype == 'saved')
          or (dml.globals.dml_version == (1, 2)
              and node.objtype == 'interface')):
        return arraywrap(node, crep.node_storage_type(node, node.site))
    elif node.objtype == 'hook':
        return arraywrap(node, TNamed('_dml_hook_t'))
    elif (node.objtype in {'register', 'field'}
          and dml.globals.dml_version == (1, 2)):
        allocate = param_bool_fixup(node, 'allocate', True)
        if node.simple_storage:
            return (arraywrap(node, crep.node_storage_type(node))
                    if allocate else None)
        else:
            @list
            @apply
            def members():
                if allocate:
                    yield ('__DMLfield',
                           arraywrap(node, crep.node_storage_type(node)))
                for sub in node.get_components():
                    if not sub.ident:
                        # implicit field
                        assert sub.objtype == 'field'

                        if not sub.simple_storage:
                            raise ICE(sub.site,
                                      'implicit fields may not contain '
                                      + 'data/saved variables or hooks')

                        # storage is inherited from the parent register
                        continue

                    yield (crep.cname(sub), print_device_substruct(sub))
            return composite_ctype(node, members)

    elif node.objtype in {'bank', 'port', 'subdevice'}:
        if node.name is None:
            assert dml.globals.dml_version == (1, 2)
            obj = []
        else:
            # really a _port_object_t* rather than conf_object_t*, but
            # there is no DML type for the former
            obj = [("_obj", arraywrap(node, TPtr(conf_object_t)))]
        return composite_ctype(node,
                               obj + [(crep.cname(sub),
                                       print_device_substruct(sub))
                                      for sub in node.get_components()])

    elif dml.globals.dml_version == (1, 2) and node.objtype == 'attribute':
        composite_type = composite_ctype(node,
                                         [(crep.cname(sub),
                                           print_device_substruct(sub))
                                          for sub in node.get_components()])
        allocate_type = crep.node_storage_type(node)
        if allocate_type:
            if composite_type:
                report(EATTRDATA(node,
                                 param_expr_site(node, 'allocate_type'),
                                 [session.site for session in
                                  node.get_recursive_components('session')]))
            return arraywrap(node, allocate_type)
        else:
            return composite_type
    elif node.objtype in {'interface', 'group', 'event', 'connect',
                          'register', 'field', 'implement', 'attribute'}:
        return composite_ctype(node,
                               [(crep.cname(sub), print_device_substruct(sub))
                                for sub in node.get_components()])
    elif node.objtype in {'parameter', 'method'}:
        return None

    else:
        raise Exception("Unknown group node " + repr(node))

def make_guard_sym(filename):
    guard = re.compile(r'[^_A-Z0-9]').sub('_', filename.upper())
    if not guard[0].isalpha() and guard[0] != '_':
        return '_' + guard
    return guard

def emit_guard_start(filename):
    guard = make_guard_sym(filename)
    if not re.match(r'[_A-Z][_A-Z0-9]*$', guard):
        raise ICE(None, 'bad guard symbol %s' % guard)
    out('#ifndef %s\n#define %s\n\n' % (guard, guard))

def emit_guard_end(filename):
    out('\n#endif /* not %s */\n' % make_guard_sym(filename))

def generate_structfile(device, filename, outprefix):
    out("/* Generated by dmlc, do not edit! */\n\n")
    emit_guard_start(filename)
    out('typedef struct %s %s_t;\n\n' % (crep.cname(device),
                                         crep.cname(device)))
    out('conf_class_t *%s(void);\n\n' % (init_function_name(device, outprefix)))
    for (name, (func, export_site)) in list(exported_methods.items()):
        if export_site.dml_version() != (1, 2):
            out("extern %s;\n"
                % func.rettype.declaration(
                    "%s(%s)" % (name,
                                ", ".join((["conf_object_t *_obj"]
                                           * (not func.independent))
                                          + [t.declaration(n)
                                             for (n, t) in (
                                                func.cparams
                                                if func.independent
                                                else func.cparams[1:])]))))
    emit_guard_end(filename)

def generate_hfile(device, headers, filename):
    out("/* Generated by dmlc, do not edit! */\n\n")
    emit_guard_start(filename)
    out('#define DML_PREFIX(x) '+crep.cname(device)+'_##x\n\n')
    legacy_attrs = int(compat.legacy_attributes in dml.globals.enabled_compat)
    out(f'#define DML_LEGACY_ATTRS {legacy_attrs}\n')

    with allow_linemarks():
        for c in headers:
            c.toc()
            out('\n')

    out('\n')

    out('#include <simics/util/help-macros.h>\n')
    out('#include <stdint.h>\n')
    out('#include "'+os.path.basename(structfilename)+'"\n\n')

    for name in dml.globals.traits:
        out(f'typedef _traitref_t {cident(name)};\n')

    # Constraints from C:
    # - Types must be defined before they are referred to
    # - Structs can be referred to by struct label before they are defined,
    #   but the struct's definition (member list) must appear before
    #   it can be used as a direct member of another struct. An indirect member
    #   (struct x *member) may however appear before the struct's definition.
    # - An 'extern typedef struct', cannot be referred to by struct label,
    #   only by typename. But we don't need to worry about declaration order,
    #   an extern typedef cannot depend on a non-extern typedef.

    # We do the following to solve this:
    # - start by creating a typedef for each named struct type.
    #   This way, we can use 'X' and 'struct X' interchangeably.
    # - Named structs are consistently referenced by their typedef name
    # - Each anonymous struct is assigned a unique struct label, and the
    #   type is always referenced as 'struct LABEL'. The first time
    #   an anonymous struct is referenced, the reference is preceded by
    #   a struct definition, defining the struct's fields.
    # - topologically sort types based on C declaration dependencies.
    #   In this ordering, a struct definition appears after all types
    #   it depends on, except that it may appear before structs it
    #   refers to *indirectly*, i.e. via a pointer.

    # typedefs for named structs come first, because in:
    #   typedef struct { int i; } X;
    #   typedef struct { X *x; } Y;
    # the C declaration of Y may appear before that of X, and the
    # C code for Y currently names the member type 'X' rather than 'struct X'.
    for tn in global_type_declaration_order:
        if tn not in typedefs:
            continue
        t = typedefs[tn]
        if isinstance(t, TStruct):
            out('typedef %sstruct %s %s;\n' % (
                'const ' if t.const else '', cident(tn), cident(tn)))

    for tn in global_type_declaration_order:
        if tn in global_anonymous_structs:
            global_anonymous_structs[tn].print_struct_definition()
        else:
            t = typedefs[tn]
            if isinstance(t, TStruct):
                t.print_struct_definition()
            else:
                out('typedef ')
                t.print_declaration(cident(tn))
        out('\n')
    out('\n')

    for (_, t) in TStruct.late_global_struct_defs:
        t.print_struct_definition()
    out('\n')

    for t in dml.globals.traits.values():
        for memo_outs_struct in t.vtable_memoized_outs.values():
            memo_outs_struct.print_struct_definition()
        print_vtable_struct_declaration(t)
    out('\n')

    for info in dml.globals.type_sequence_infos.values():
        if info.struct:
            info.struct.print_struct_definition()
            out(f'typedef struct {info.struct.label} {info.struct.label}_t;\n')
    out('\n')

    for typ in (typ
                for info in itertools.chain(
                        dml.globals.after_delay_infos.values(),
                        dml.globals.after_on_hook_infos,
                        dml.globals.immediate_after_infos.values())
                for typ in info.types_to_declare):
        typ.print_struct_definition()
    out('\n')

    print_device_substruct(device)

    out('// allows generated code to store device struct offsets in uint32,\n')
    out('// which saves space\n')
    out(f'STATIC_ASSERT(sizeof(struct {crep.cname(device)}) <= UINT32_MAX);\n')

    if dml.globals.log_groups:
        i = 1
        for g in dml.globals.log_groups:
            out('static const uint64 %s UNUSED = %dULL;\n'
                % (crep.cloggroup(g), i))
            i <<= 1
        out('\n')

    out('void hard_reset_'+crep.cname(device)
        + '('+crep.structtype(device)+' *_obj);\n')
    out('void soft_reset_'+crep.cname(device)
        + '('+crep.structtype(device)+' *_obj);\n')
    emit_guard_end(filename)

def generate_protofile(device):
    linkage = 'extern' if c_split_threshold else 'static'
    out('\n/* generated function prototypes */\n')
    for proto in prototypes:
        out("%s %s UNUSED;\n" % (linkage, proto))

def get_attr_fname(node, port, group_prefix):
    port_prefix = port.attrname() + '_' if port else ''
    if node.objtype == 'register' and node.is_confidential():
        return port_prefix + get_anonymized_name(node)
    else:
        return port_prefix + group_prefix + crep.cname(node)

def generate_attr_setter(fname, node, port, dimsizes, cprefix, loopvars,
                         allow_cutoff=False):
    device = dml.globals.device
    proto = ('set_error_t ' + fname +
             '(conf_object_t *_obj, attr_value_t *_val, lang_void *_aux)')
    start_function_definition(proto)
    out('{\n', postindent = 1)
    if port:
        out(' _port_object_t *_portobj = (_port_object_t *)_obj;\n')
        out(crep.structtype(device)+' *_dev UNUSED = ('
            + crep.structtype(device)+'*)_portobj->dev;\n')
        index_array = mkLit(port.site, '_portobj->indices',
                           TPtr(TInt(32, False, const=True)))
        port_indices = tuple(mkIndex(port.site, index_array,
                                     mkIntegerLiteral(port.site, i))
                             for i in range(port.dimensions))
    else:
        out(crep.structtype(device)+' *_dev UNUSED = ('
            + crep.structtype(device)+'*)_obj;\n')
        port_indices = ()
    out('set_error_t _status = Sim_Set_Illegal_Value;\n')
    if loopvars:
        out('uint32 ' + ','.join(v.str for v in loopvars) + ';\n')
    fscope = Symtab(global_scope)

    valuevar = '*_val'
    for (dim, (dimsize, loopvar)) in enumerate(zip(dimsizes, loopvars)):
        out('for (%s = 0; %s < %d; %s++) {\n'
            % (loopvar.read(), loopvar.read(), dimsize, loopvar.read()),
            postindent = 1)
        list_item = 'SIM_attr_list_item(%s, %s)' % (valuevar, loopvar.read())
        if allow_cutoff:
            in_list = ('%s < SIM_attr_list_size(%s)'
                       % (loopvar.read(), valuevar))
            if dim:
                in_list = 'SIM_attr_is_list(%s) && %s' % (valuevar, in_list)
            list_item = '%s ? %s : SIM_make_attr_nil()' % (in_list, list_item)
        out('attr_value_t attr%d = %s;\n' % (dim, list_item))
        valuevar = 'attr%d' % (dim,)

    with NoFailure(node.site), crep.DeviceInstanceContext():
        setcode = [
            codegen_inline_byname(
                node, port_indices + loopvars,
                '_set_attribute' if dml.globals.dml_version == (1, 2)
                else 'set_attribute',
                [mkLit(node.site, valuevar, TNamed('attr_value_t'))],
                [mkLit(node.site, '_status', TNamed('set_error_t'))],
                node.site,
                inhibit_copyin = not loopvars)]

    code = mkCompound(None, declarations(fscope) + setcode)
    code.toc_inline()
    if dimsizes:
        # abort on first bad value
        out('if (_status != Sim_Set_Ok) goto exit;\n')
        for _ in dimsizes:
            out('}\n', preindent = -1)
        out('exit:\n')
    output_dml_state_change('_dev')
    out('return _status;\n')
    out('}\n\n', preindent = -1)
    splitting_point()

def generate_attr_getter(fname, node, port, dimsizes, cprefix, loopvars):
    device = dml.globals.device
    proto = ('attr_value_t ' + fname+'(conf_object_t *_obj, lang_void *_aux)')
    start_function_definition(proto)
    out('{\n', postindent = 1)
    if port:
        out(' _port_object_t *_portobj = (_port_object_t *)_obj;\n')
        out(crep.structtype(device)+' *_dev UNUSED = ('
            + crep.structtype(device)+'*)_portobj->dev;\n')
        index_array = mkLit(port.site, '_portobj->indices',
                           TPtr(TInt(32, False, const=True)))
        port_indices = tuple(mkIndex(port.site, index_array,
                                     mkIntegerLiteral(port.site, i))
                             for i in range(port.dimensions))
    else:
        out(crep.structtype(device)+' *_dev UNUSED = ('
            + crep.structtype(device)+'*)_obj;\n')
        port_indices = ()
    out('attr_value_t _val0;\n')

    if loopvars:
        out('uint32 ' + ', '.join(v.str for v in loopvars) + ';\n')
    fscope = Symtab(global_scope)

    valuevar = mkLit(node.site, '_val0', attr_value_t)
    assert len(dimsizes) == len(loopvars)
    for (dim, loopvar, depth) in zip(dimsizes, loopvars,
                                     list(range(len(dimsizes)))):
        next_valuevar = mkLit(node.site, '_val%s' % (depth + 1,), attr_value_t)
        out('%s = SIM_alloc_attr_list(%d);\n' % (valuevar.read(), dim))
        out('for (%s = 0; %s < %d; %s++) {\n'
            % (loopvar.read(), loopvar.read(), dim, loopvar.read()),
            postindent = 1)
        out('attr_value_t %s;\n' % (next_valuevar.read()))
        valuevar = next_valuevar

    with NoFailure(node.site), crep.DeviceInstanceContext():
        getcode = codegen_inline_byname(
            node, port_indices + loopvars,
            '_get_attribute' if dml.globals.dml_version == (1, 2)
            else 'get_attribute',
            [], [valuevar], node.site)
        code = mkCompound(node.site, declarations(fscope) + [getcode])
        code.toc_inline()

    for depth, loopvar in reversed(list(enumerate(loopvars))):
        out('SIM_attr_list_set_item(&_val%d, %s, _val%d);\n'
            % (depth, loopvar.read(), depth + 1))
        out('}\n', preindent = -1)

    output_dml_state_change('_dev')
    out('return _val0;\n')
    out('}\n\n', preindent = -1)
    splitting_point()

# dimsizes, loopvars, prefix are relative to port.
def check_attribute(node, port, prefix):
    config_param = param_str_fixup(node, 'configuration', 'none')
    if config_param == 'none':
        return
    if not get_long_doc(node):
        if (node.objtype in {'attribute', 'connect'}
            and config_param == 'required'):
            report(WNDOCRA(node, node.logname()))
        elif node.objtype != 'register':
            report(WNDOC(node, node.logname()))
    attrname = get_attr_name(prefix, node)
    register_attribute(node.site, port, attrname)
    if port and need_port_proxy_attrs(port):
        register_attribute(node.site, None, "%s_%s" % (port.name, attrname))

# dimsizes, loopvars, prefix are relative to port.
def generate_attribute_common(initcode, node, port, dimsizes, prefix,
                              loopvars):
    assert dml.globals.dml_version == (1, 2)
    attrname = get_attr_name(prefix, node)

    config_param = param_str_fixup(node, 'configuration', 'none')
    if config_param == 'none':
        return

    for _ in node.arraylens():
        loopvars += (mkLit(None, '_i'+str(len(loopvars)+1), TInt(32, False)),)
    dimsizes += node.arraylens()

    doc = get_long_doc(node)
    if doc:
        pass
    elif node.objtype == 'register':
        doc = 'register ' + node.logname_anonymized()
    elif (node.objtype in {'attribute', 'connect'}
          and config_param == 'required'):
        report(WNDOCRA(node, node.logname()))
        doc = "Undocumented"
    else:
        report(WNDOC(node, node.logname()))
        doc = "Undocumented"

    # append the required interfaces to the docstring
    if node.objtype == 'connect':
        ifaces = [i for i in node.get_components('interface')
                  if param_bool(i, 'required')]
        if ifaces:
            doc += (
                '\n\nRequired interfaces: '
                + ', '.join('<iface>' + i.name + '</iface>' for i in ifaces)
                + '.')
    doc = mkStringConstant(node.site, doc)

    fname = get_attr_fname(node, port, prefix)

    allow_cutoff = (node.objtype == 'connect'
                    and param_str(node, 'configuration') == 'optional')

    if node.writable:
        setter = 'set_'+fname
        generate_attr_setter(setter, node, port, dimsizes, prefix, loopvars,
                             allow_cutoff)
    else:
        setter = '0'

    if node.readable:
        getter = 'get_'+fname
        generate_attr_getter(getter, node, port, dimsizes, prefix, loopvars)
    else:
        getter = '0'

    attr_flag = get_attr_flags(node)
    attr_type = param_str(node, 'attr_type')

    for dim in reversed(dimsizes):
        if allow_cutoff:
            attr_type = "[%s{0:%d}]" % (attr_type, dim)
        else:
            attr_type = "[%s{%d}]" % (attr_type, dim)

    register_attribute(node.site, port, attrname)
    if port:
        if need_port_proxy_attrs(port):
            if port.dimensions == 0:
                register_attribute(
                    node.site, None, "%s_%s" % (port.name, attrname))
                initcode.out(
                    '_register_port_attr(class, %s, offsetof(%s, %s), %s,'
                    % (port_class_ident(port),
                       crep.structtype(dml.globals.device),
                       crep.cref_portobj(port, ()),
                       'true' if port.objtype == "bank" else 'false')
                    + ' "%s", "%s", %s, %s, %s, "%s", %s);\n'
                    % (port.name, attrname, getter, setter,
                       attr_flag, attr_type, doc.read()))
            else:
                assert port.dimensions == 1
                # Generate an accessor attribute for legacy reasons
                register_attribute(
                    node.site, None, "%s_%s" % (port.name, attrname))
                member = crep.cref_portobj(
                    port, (mkLit(port.site, '0', TInt(32, False)),))
                (dimsize,) = port.dimsizes
                initcode.out(
                    '_register_port_array_attr(class, %s, offsetof(%s, %s),'
                    % (port_class_ident(port),
                       crep.structtype(dml.globals.device),
                       member)
                    + ' %d, %s, "%s", "%s", %s, %s, %s, "%s",'
                    % (dimsize, 'true' if port.objtype == "bank" else 'false',
                       port.name, attrname, getter, setter, attr_flag,
                       attr_type)
                    + ' %s);\n' % (doc.read(),))
        else:
            initcode.out(
                '_register_port_attr_no_aux(%s,'
                ' "%s", %s, %s, %s, "%s", %s);\n'
                % (port_class_ident(port),
                   attrname, getter, setter,
                   attr_flag, attr_type, doc.read()))
    else:
        initcode.out(
            f'_DML_register_attribute(class, "{attrname}", '
            + f'{getter}, NULL, {setter}, NULL, {attr_flag}, "{attr_type}", '
            + f'{doc.read()});\n')

# Output register attribute functions and return a string with
# initialization code
def generate_attributes(initcode, node, port=None,
                        dimsizes=(), prefix='', loopvars=()):
    if node.objtype in {'connect', 'attribute', 'register'}:
        try:
            if dml.globals.dml_version == (1, 2):
                generate_attribute_common(
                    initcode, node, port, dimsizes, prefix, loopvars)
            else:
                check_attribute(node, port, prefix)
        except DMLError as e:
            report(e)
        return

    if node.objtype in {'parameter', 'method', 'session', 'saved', 'hook'}:
        return

    # Registration order is undefined but has significance, so
    # register attributes of subobjects in an order that is deterministic,
    # platform-independent, and unaffected by adding or removing
    # objects.
    children = sorted(node.get_components(),
                      key=lambda o: o.name or '')
    if node.objtype in {'group', 'implement', 'event'}:
        prefix += crep.cname(node) + '_'
        for _ in node.arraylens():
            loopvars += (mkLit(None, '_i' + str(len(loopvars) + 1),
                               TInt(32, False)),)
        dimsizes += node.arraylens()

        for child in children:
            generate_attributes(initcode, child, port, dimsizes, prefix,
                                loopvars)
    elif node.objtype in {'device', 'bank', 'port', 'subdevice'}:
        if node.objtype in {'bank', 'port', 'subdevice'} and (
                # anonymous bank
                dml.globals.dml_version != (1, 2) or node.name != None):
            port = node
        for child in children:
            generate_attributes(initcode, child, port)
    else:
        raise ICE(node, f"unknown object type {node.objtype}")

def find_connects(node, subobj_parent):
    if node.objtype == 'connect':
        yield (subobj_parent, node)
    for sub in node.get_components():
        if sub.objtype in {'bank', 'port', 'subdevice'}:
            yield from find_connects(sub, sub)
        else:
            yield from find_connects(sub, subobj_parent)

def generate_subobj_connects(init_code, device, prefixes=("",)):
    if dml.globals.dml_version != (1, 2):
        t = dml.globals.traits['init_as_subobj']
        for (parent, node) in find_connects(device, device):
            if t in node.traits.ancestors:
                classname = mkStringConstant(
                    None, param_str(node, 'classname')).quoted
                desc = (mkStringConstant(None, param_str(node, "desc")).quoted
                        if param_defined(node, 'desc') else 'NULL')
                cls = port_class_ident(parent)
                for indices in itertools.product(
                        *(list(range(i)) for i in node.dimsizes[
                            parent.dimensions:])):
                    name = node.logname_anonymized(
                        indices, relative=parent.objtype)
                    init_code.out(
                        f'_DML_register_subobj_connect({cls}, '
                        + f'{classname}, "{name}", {desc});\n')

def apply(f):
    return f()
@apply
class PORTOBJ(object):
    """Marker that indicates that a method is wrapped for a port object,
    and indices are taken from that object"""

def wrap_method(meth, wrapper_name, indices=()):
    """Wrap a method in a C function taking a conf_object_t pointer as first
    argument, and returning the optional output argument. Since this is a
    common DML surface area, we inject DML context checks into it.

    indices is either a tuple of integers, or PORTOBJ if the first arg
    is a port object
    """

    inparams = [t.declaration(p) for p, t in meth.inp]
    if not meth.outp:
        retvar = None
        rettype = TVoid()
    elif len(meth.outp) == 1:
        retvar, rettype = meth.outp[0]
    else:
        raise ICE(meth, "many return types")
    start_function_definition(rettype.declaration('%s(%s)' % (
        wrapper_name, ", ".join(["conf_object_t *_obj"] + inparams))))
    out('{\n', postindent = 1)
    devstruct = crep.structtype(dml.globals.device)
    if indices is PORTOBJ:
        out('_port_object_t *_portobj = (_port_object_t *)_obj;\n')
        out(devstruct+' *_dev UNUSED = (' + devstruct + ' *)_portobj->dev;\n')
        index_array = mkLit(meth.site, '_portobj->indices',
                           TPtr(TInt(32, False, const=True)))
        indices = tuple(mkIndex(meth.site, index_array,
                                mkIntegerLiteral(meth.site, i))
                        for i in range(meth.dimensions))
    else:
        assert meth.dimensions == len(indices)
        out(devstruct+' *_dev UNUSED = ('+devstruct+'*)_obj;\n')
        indices = tuple(mkIntegerLiteral(meth.site, i) for i in indices)
    with crep.DeviceInstanceContext():
        if retvar:
            mkDeclaration(meth.site, retvar, rettype,
                          init = get_initializer(meth.site, rettype,
                                                 None, None, None)).toc()

        with LogFailure(meth.site, meth, indices):
            inargs = [mkLit(meth.site, v, t) for v, t in meth.inp]
            outargs = [mkLit(meth.site, v, t) for v, t in meth.outp]
            codegen_call(meth.site, meth,
                         indices,
                         inargs, outargs).toc()
    output_dml_state_change('_dev')
    if retvar:
        out('return '+retvar+';\n')
    out('}\n', preindent = -1)
    out('\n')

    splitting_point()

def generate_implement_method(device, ifacestruct, meth, indices):
    # FIXME: this is how it should be done, but we have to fix
    # codegen_method so it generates a function that returns
    # the value if there is a single output parameter.
    #
    # meth.func.fail = IgnoreFailure()
    # meth.func.confobj = 1
    # codegen_method(meth)
    # out(meth.get_c())

    try:
        require_fully_typed(None, meth)

        # Calculate the expected method signature
        member_type = ifacestruct.get_member_qualified(meth.name)
        if not member_type:
            raise EMEMBER(meth.site, meth.parent.name, meth.name)
        member_type = safe_realtype(member_type)
        if not isinstance(member_type, TPtr):
            raise EIMPLMEMBER(
                meth.site,
                f'{meth.parent.name}_interface_t.{meth.name}',
                ifacestruct.declaration_site)
        func_type = member_type.base
        if not isinstance(func_type, TFunction):
            raise EIMPLMEMBER(meth.site,
                              f'{meth.parent.name}_interface_t.{meth.name}',
                              ifacestruct.declaration_site)
        iface_input_types = func_type.input_types[1:]
        iface_num_outputs = 0 if func_type.output_type.void else 1

        # Check the signature
        if len(meth.inp) != len(iface_input_types):
            raise EMETH(meth.site, None,
                        'different number of input parameters')
        if len(meth.outp) != iface_num_outputs:
            raise EMETH(meth.site, None,
                        'different number of output parameters')
        if func_type.varargs:
            # currently impossible to implement a varargs interface
            # method in DML
            raise EMETH(meth.site, None, 'interface method is variadic')
        for ((mp, mt), it) in zip(meth.inp, iface_input_types):
            if not safe_realtype_unconst(mt).eq(safe_realtype_unconst(it)):
                raise EARGT(meth.site, 'implement', meth.name,
                            mt, mp, it, 'method')
        if iface_num_outputs and dml.globals.dml_version != (1, 2):
            [(_, mt)] = meth.outp
            if not safe_realtype_unconst(mt).eq(
                    safe_realtype_unconst(func_type.output_type)):
                raise EARGT(meth.site, 'implement', meth.name,
                            mt, '<return value>', func_type.output_type,
                            'method')
        if indices is PORTOBJ:
            name = '_DML_PIFACE_' + crep.cref_method(meth)
        else:
            name = '_DML_IFACE_' + crep.cref_method(meth) + "".join([
                "__%d" % idx for idx in indices])

        wrap_method(meth, name, indices)
    except DMLError as e:
        report(e)
        return f".{meth.name} = NULL,\n"
    return ".%s = &%s,\n" % (meth.name, name)

def interface_block(device, ifacestruct, methods, indices = ()):
    if not methods:
        # Empty interface, but we need at least one initialiser (some
        # compilers complain if we declare an uninitialised const
        # variable). There is at least one member, probably a
        # placeholder, so try placating it with a zero.
        return "{ 0 }"

    indent = ' ' * indent_level
    indent2 = indent * 2
    return "{\n%s%s%s}" % (
        indent2,
        indent2.join(generate_implement_method(device, ifacestruct, meth,
                                               indices)
                     for meth in methods),
        indent)

def string_literal(pystr):
    if pystr is None:
        return 'NULL'
    else:
        return '"%s"' % (ctree.string_escape(pystr.encode('utf-8')),)

def generate_implement(code, device, impl):
    varname = crep.cname(impl) + "_interface"
    methods = impl.get_components('method')

    typename = param_str(impl,
                         'c_type' if dml.globals.dml_version == (1, 2)
                         else '_c_type')
    ifacetype = TNamed(typename)
    ifacetype.declaration_site = impl.site
    ifacestruct = safe_realtype(ifacetype)

    if not isinstance(ifacestruct, (TStruct, TExternStruct)):
        raise EIFTYPE(impl.site, ifacetype)

    port = impl.parent
    assert port.objtype in  {'port', 'bank', 'device', 'subdevice'}
    assert not impl.local_dimensions()
    if not port.name:
        # anonymous bank
        assert dml.globals.dml_version == (1, 2)
        raise EANONPORT(impl.site, port)
    code.out("{\n", postindent = 1)
    if not port.parent:
        # device
        code.out("static const %s = %s;\n" %
                 (ifacetype.declaration(varname),
                  interface_block(device, ifacestruct, methods, ())))
        code.out('SIM_register_interface(class, "%s", &%s);\n' %
                 (impl.name, varname))
    else:
        desc = string_literal(get_short_doc(port))

        code.out("static const %s = %s;\n" % (
            ifacetype.declaration(varname),
            interface_block(device, ifacestruct, methods, PORTOBJ)))
        code.out('SIM_register_interface(%s, "%s", &%s);\n'
                 % (port_class_ident(port), impl.name, varname))
        # Legacy interface ports are only added for ports and banks that were
        # available in Simics 5, i.e. zero or one dimensional direct
        # descendants of the device object
        if (port.parent is dml.globals.device
            and port.objtype in {'port', 'bank'}
            and compat.port_proxy_ifaces in dml.globals.enabled_compat):
            if port.local_dimensions() == 0:
                code.out("static const %s = %s;\n" % (
                    ifacetype.declaration('port_iface'),
                    interface_block(device, ifacestruct, methods, ())))
                code.out('SIM_register_port_interface'
                         '(class, "%s", &port_iface, "%s", %s);\n'
                         % (impl.name, crep.cname(port), desc))
            elif port.local_dimensions() == 1:
                [arrlen] = port.arraylens()
                code.out("static const %s%s = %s;\n" %
                         (ifacetype.declaration("ifaces"),
                          "[%s]" % (arrlen,),
                          "{%s\n}" % ",\n".join(
                              "%s" % interface_block(device, ifacestruct,
                                                     methods, (i, ))
                              for i in range(arrlen))))
                code.out("interface_array_t iface_vect = VNULL;\n")
                idxvar = "i0"
                code.out("for (int %s = 0; %s < %d; %s++)\n" %
                         (idxvar, idxvar, arrlen, idxvar),
                         postindent = 1)
                access = "[%s]" % idxvar
                code.out("VADD(iface_vect, &ifaces%s);\n" % access,
                         postindent = -1)
                code.out('VT_register_port_array_interface'
                         '(class, "%s", &iface_vect, "%s", %s);\n'
                         % (impl.name, crep.cname(port), desc))
                code.out('VFREE(iface_vect);\n')
    code.out("}\n", preindent = -1)

def port_prefix(port):
    return {'bank': 'bank.', 'port': 'port.', 'subdevice': ''}[port.objtype]

def find_port_parent(port, indices):
    '''Given a port or bank node, find the port node that correspond to
    port object's port parent, and the name of the port object
    relative to port parent and the device. The return value is a
    triple (node, prefix, suffix), where node is the port parent,
    suffix is the conf-object name as descendant of the port parent,
    and (prefix+suffix) is the conf-object name as descendant of the
    device. E.g., for a node subdev.p, returns (subdev.obj, "subdev",
    "port.p").'''
    ancestor = port.parent
    suffix_parts = [port_prefix(port) + port.name_anonymized
                    + ''.join(f'[{i}]' for i in indices[ancestor.dimensions:])]
    indices = indices[:ancestor.dimensions]
    while ancestor.objtype not in {'device', 'subdevice'}:
        assert ancestor.objtype == 'group'
        name = ancestor.name_anonymized
        idx = ''.join(f'[{i}]' for i in indices[ancestor.parent.dimensions:])
        suffix_parts.append(f'{name}{idx}.')
        ancestor = ancestor.parent
        indices = indices[:ancestor.dimensions]
    if ancestor.objtype == 'device':
        prefix = ''
    else:
        (_, prefix_prefix, prefix_suffix) = find_port_parent(ancestor, indices)
        prefix = f'{prefix_prefix}{prefix_suffix}.'
    return (ancestor, prefix, ''.join(reversed(suffix_parts)))

def node_ancestors(node, since):
    while True:
        if node is since:
            return
        yield node
        node = node.parent

def generate_port_class(code, device, port):
    (port_parent, prefix, suffix) = find_port_parent(
        port, ('%d',) * port.dimensions)
    portclass_name_comps = [o.name_anonymized
                            for o in node_ancestors(port, device)]
    portclass_name_comps.append(param_str_fixup(device, 'classname', ''))
    portclass_name = '.'.join(reversed(portclass_name_comps))
    desc = string_literal(get_short_doc(port))
    doc = string_literal(get_long_doc(port))
    code.out(f'{port_class_ident(port)} = _register_port_class('
             f'"{portclass_name}", {desc}, {doc});\n')
    port_parent_class = ('class' if port_parent.objtype == 'device'
                         else port_class_ident(port_parent))
    port_dims = port.dimensions - port_parent.dimensions
    if port_dims:
        for (i, sz) in enumerate(port.dimsizes[port_parent.dimensions:]):
            code.out(f'for (int _i{i} = 0; _i{i} < {sz}; ++_i{i}) {{\n',
                     postindent=1)
        fmtargs = ''.join(f', _i{i}' for i in range(port_dims))
        code.out(f'strbuf_t portname = sb_newf("{suffix}"{fmtargs});\n')
        code.out(f'SIM_register_port({port_parent_class}, sb_str(&portname),'
                 f' {port_class_ident(port)}, {desc});\n')
        code.out('sb_free(&portname);\n')
        for _ in range(port_dims):
            code.out('}\n', preindent=-1)
    else:
        code.out(f'SIM_register_port({port_parent_class},'
                 f' "{suffix}", {port_class_ident(port)}, {desc});\n')

def generate_port_classes(code, device):
    for port in device.get_recursive_components('port', 'bank', 'subdevice'):
        if port.name is None:
            assert dml.globals.dml_version == (1, 2)
            continue
        generate_port_class(code, device, port)
        add_variable_declaration(f'conf_class_t *{port_class_ident(port)}')


def generate_implements(code, device):
    for impl in device.get_recursive_components('implement'):
        try:
            generate_implement(code, device, impl)
        except DMLError as e:
            report(e)

def generate_simple_event_only_domains_funs():
    start_function_definition(
        'attr_value_t _simple_event_only_domains_get_value('
        + 'conf_object_t *_obj, lang_void *data)')
    out('{\n', postindent=1)
    out('return _serialize_simple_event_data(0, NULL, _id_infos, '
        + '(_simple_event_data_t *)data);\n')
    out('}\n\n', preindent=-1)

    start_function_definition(
        'lang_void *_simple_event_only_domains_set_value('
        + 'conf_object_t *_obj, attr_value_t val)')
    out('{\n', postindent=1)
    out('_simple_event_data_t *out;\n')
    out('set_error_t error = _deserialize_simple_event_data(NULL, 0, 0, NULL, '
        + '&_id_info_ht, val, &out);\n')
    out('if (error != Sim_Set_Ok) {\n', postindent=1)
    out('out = NULL;\n')
    out('}\n', preindent=-1)
    out('return out;\n')
    out('}\n\n', preindent=-1)

def generate_simple_events(device):
    for info in dml.globals.after_delay_infos.values():
        start_function_definition(
            f'void {info.cident_callback}(conf_object_t *_obj, '
            + 'lang_void *_data)')
        out('{\n', postindent = 1)
        out(crep.structtype(device) + ' *_dev UNUSED = ('
            + crep.structtype(device) + '*)_obj;\n')

        out('_simple_event_data_t *data = '
            + '(_simple_event_data_t *)_data;\n')

        # If data is NULL, report it and use emergency indices/args
        if info.dimensions or info.args_type:
            out('if (!data) {', postindent=1)
            out('const char *msg = "Failed deserialization of after event '
                + 'data. Using emergency indices/arguments.";\n')
            out('VT_critical_error(msg, msg);\n')
            out('}', preindent=-1)

        if info.dimensions > 0:
            out('const uint32 *_indices = data ? data->indices '
                + (': (const uint32 [%s]) { 0 };\n'
                   % (info.dimensions,)))

        if info.args_type:
            args_decl = TPtr(
                conv_const(True, info.args_type)).declaration('_args')
            emergency_args_c_type = TArray(
                info.args_type,
                mkIntegerConstant(None, 1, False)).declaration('')
            out('%s = data ? data->args : (%s) { 0 };\n'
                % (args_decl, emergency_args_c_type))

        indices_lit = '_indices' if info.dimensions else None
        args_lit = '_args' if info.args_type else None
        info.generate_callback_call(indices_lit, args_lit)
        out('if (data) {\n', postindent=1)
        out('_free_simple_event_data(*data);\n')
        out('MM_FREE(data);\n')
        out('}\n', preindent=-1)
        output_dml_state_change('_dev')
        out('}\n\n', preindent = -1)
        splitting_point()

        if info.dimensions or info.args_type:
            start_function_definition(
                f'attr_value_t {info.cident_get_value}'
                + '(conf_object_t *_obj, lang_void *data)')
            out('{\n', postindent = 1)
            out(('return _serialize_simple_event_data(%d, %s, _id_infos, '
                 + '(_simple_event_data_t *)data);\n')
                % (info.dimensions,
                   (serialize.lookup_serialize(info.args_type)
                    if info.args_type else 'NULL')))
            out('}\n\n', preindent = -1)
            splitting_point()

            start_function_definition(
                f'lang_void *{info.cident_set_value}'
                + '(conf_object_t *_obj, attr_value_t val)')
            out('{\n', postindent = 1)
            out('_simple_event_data_t *out;\n')
            out(('set_error_t error = _deserialize_simple_event_data(%s, '
                 + '%d, %s, %s, &_id_info_ht, val, &out);\n')
                % ((('(const uint32 []) {%s}'
                     % (', '.join(map(str, info.dimsizes)),))
                    if info.dimensions > 0 else 'NULL'),
                   info.dimensions,
                   (f'sizeof({info.args_type.declaration("")})'
                    if info.args_type else 0),
                   (serialize.lookup_deserialize(info.args_type)
                    if info.args_type else 'NULL'),
                   ))
            out('if (error != Sim_Set_Ok) {\n', postindent=1)
            out('out = NULL;\n')
            out('}\n', preindent=-1)
            out('return (lang_void *) out;\n')
            out('}\n\n', preindent = -1)
            splitting_point()

def generate_after_on_hooks_artifacts(device):
    for info in dml.globals.after_on_hook_infos:
        site = SimpleSite(f'after on hook {string_literal(info.string_key)}')

        start_function_definition(
            f'void {info.cident_callback}(conf_object_t *_obj, '
            + 'const uint32 *indices, const void *_args, const void *_msg)')
        out('{\n', postindent = 1)
        out(crep.structtype(device) + ' *_dev UNUSED = ('
            + crep.structtype(device) + '*)_obj;\n')
        if info.param_to_msg_comp:
            out(f'const {info.typeseq_info.struct.label}_t *msg UNUSED '
                + '= _msg;\n')

        if info.args_type:
            args_decl = TPtr(
                conv_const(True, info.args_type)).declaration('args')
            out('%s = _args;\n' % (args_decl,))
        indices_lit = 'indices' if info.dimensions else None
        args_lit = 'args' if info.args_type else None
        msg_lit = 'msg' if info.param_to_msg_comp else None
        info.generate_callback_call(indices_lit, args_lit, msg_lit)
        out('}\n\n', preindent = -1)
        splitting_point()

        if info.has_serialized_args:
            start_function_definition(
                f'attr_value_t {info.cident_args_serializer}('
                + 'const void *_args)')
            out('{\n', postindent = 1)
            if info.args_type:
                args_type_ptr = TPtr(conv_const(True, info.args_type))
                out(f'{args_type_ptr.declaration("args")} = _args;\n')
                args_expr = mkDereference(site, mkLit(site, 'args', args_type_ptr))
            else:
                args_expr = None
            out('attr_value_t out;\n')
            out_expr = mkLocalVariable(
                site, LocalSymbol('out', 'out', attr_value_t, site=site))
            info.generate_args_serializer(site, args_expr, out_expr)
            out('return out;\n')
            out('}\n\n', preindent = -1)
            splitting_point()

            start_function_definition(
                f'set_error_t {info.cident_args_deserializer}('
                + 'attr_value_t val, void *_out)')
            out('{\n', postindent = 1)
            out('set_error_t _success UNUSED = Sim_Set_Ok;\n')

            if info.args_type:
                out(f'{TPtr(info.args_type).declaration("out")} = _out;\n')
                out_expr = mkDereference(site, mkLit(site, 'out',
                                                     TPtr(info.args_type)))
            else:
                out_expr = None
            def error_out(exc, msg):
                stmts = []
                stmts.append(mkInline(site, f'_success = {exc};'))
                if msg is not None:
                    stmts.append(
                        mkInline(site, f'SIM_attribute_error("{msg}");'))
                stmts.append(mkInline(site, 'goto _exit;'))
                return stmts
            val_expr = mkLit(site, 'val', attr_value_t)

            info.generate_args_deserializer(site, val_expr, out_expr,
                                            error_out)

            out('_exit:\n')
            out('return _success;\n')
            out('}\n\n', preindent = -1)
            splitting_point()

    if dml.globals.after_on_hook_infos:
        init = '{\n%s\n}' % (',\n'.join(
            '    {%s, %s, %s, %s, %s, %d}'
            % ((string_literal(info.string_key), info.cident_callback)
               + ((info.cident_args_serializer, info.cident_args_deserializer)
                  if info.has_serialized_args else ('NULL', 'NULL'))
               + (f'sizeof({info.args_type.declaration("")})'
                  if info.args_type else '0', info.parent.uniq))
            for info in dml.globals.after_on_hook_infos),)
        add_variable_declaration(
            'const _dml_after_on_hook_info_t _after_on_hook_infos[]', init)

    add_variable_declaration('ht_str_table_t _typeseq_after_on_hook_hts[%d]'
                             % (len(dml.globals.type_sequence_infos),))

    start_function_definition(
        'void _initialize_typeseq_after_on_hook_hts(void)')
    out('{\n', postindent = 1)
    for typeseq_info in dml.globals.type_sequence_infos.values():
        ht_ident = f'_typeseq_after_on_hook_hts[{typeseq_info.uniq}]'
        for info in typeseq_info.after_on_hooks.values():
            out(f'ht_insert_str(&{ht_ident}, '
                + f'_after_on_hook_infos[{info.uniq}].callback_key, '
                + f'&_after_on_hook_infos[{info.uniq}]);\n')
    out('}\n', preindent = -1)
    splitting_point()

def generate_immediate_after_callbacks(device):
    for info in dml.globals.immediate_after_infos.values():
        start_function_definition(
            f'void {info.cident_callback}(conf_object_t *_obj, '
            + 'const uint32 *indices, const void *_args)')
        out('{\n', postindent = 1)
        out(crep.structtype(device) + ' *_dev UNUSED = ('
            + crep.structtype(device) + '*)_obj;\n')

        if info.args_type:
            args_decl = TPtr(
                conv_const(True, info.args_type)).declaration('args')
            out('%s = _args;\n' % (args_decl,))
        indices_lit = 'indices' if info.dimensions else None
        args_lit = 'args' if info.args_type else None
        info.generate_callback_call(indices_lit, args_lit)
        output_dml_state_change('_dev')
        out('}\n\n', preindent = -1)
        splitting_point()

def generate_simple_events_control_methods(device):
    start_function_definition(
        'void _cancel_simple_delay_events(conf_object_t *_obj, '
        'conf_object_t *_clock, const _identity_t *_domain)')
    out('{\n', postindent=1)
    out('int (*_pred)(lang_void *, lang_void *) UNUSED = '
        + '_domain ? _simple_event_predicate : NULL;\n')
    for key in dml.globals.after_delay_infos:
        out('SIM_event_cancel_time('
            + f'_clock, {crep.get_evclass(key)}, _obj, _pred, '
            + '(lang_void *)_domain);\n')
    out('}\n', preindent=-1)

    start_function_definition(
        'void _cancel_simple_events(conf_object_t *_obj, _identity_t _domain)')
    out('{\n', postindent = 1)
    out(crep.structtype(device) + ' *_dev UNUSED = ('
        + crep.structtype(device) + '*)_obj;\n')
    # !SIM_marked_for_deletion(_obj) is in case .cancel_after() is called
    # during deletion
    out('if (!SIM_marked_for_deletion(_obj) && SIM_object_clock(_obj)) {\n',
        postindent=1)
    out('_cancel_simple_delay_events(_obj, SIM_object_clock(_obj), '
        + '&_domain);\n')
    out('_cancel_simple_delay_events(_obj, SIM_picosecond_clock(_obj), '
        + '&_domain);\n')
    out('}\n', preindent=-1)

    site = logging.SimpleSite('<_cancel_simple_events>')
    by_dims = {}
    for hook in objects.Device.hooks:
        by_dims.setdefault(hook.dimsizes, []).append(hook)

    for (dims, hooks) in by_dims.items():
        for i in range(len(dims)):
            out(f'for (uint32 _i{i} = 0; _i{i} < {dims[i]}; _i{i}++) {{\n',
                postindent=1)

        indices = tuple(mkLit(site, f'_i{i}', TInt(32, False))
                        for i in range(len(dims)))
        for hook in hooks:
            out('_DML_cancel_afters_in_hook_queue('
                + f'&_dev->{crep.cref_hook(hook, indices)}.queue, _domain, '
                + '0);\n')
        for i in range(len(dims)):
            out('}\n', preindent=-1)

    out('_DML_cancel_afters_in_detached_hook_queues('
        + '_dev->_detached_hook_queue_stack, _domain);\n')
    if dml.globals.immediate_after_infos:
        out('_DML_cancel_immediate_afters(_dev->_immediate_after_state, '
            + '_domain);\n')
    out('}\n\n', preindent = -1)
    splitting_point()

def event_callbacks(event, indices):
    method_names = (
        ['callback',
         'destroy',
         'get_event_info',
         'set_event_info',
         'describe_event'] if dml.globals.dml_version == (1, 2) else
        ['__callback',
         '__destroy',
         '__get_event_info',
         '__set_event_info',
         '__describe_event'])
    result = []
    for method_name in method_names:
        method = event.get_component(method_name)
        if not method:
            raise ICE(event.site, 'cannot find method %s' % (method_name))
        result.append((method, '_DML_EV_%s%s' % (
            crep.cref_method(method),
            '_'.join(str(i) for i in indices))))
    return result

def generate_register_events(device):
    events = device.get_recursive_components('event')
    for key in dml.globals.after_delay_infos:
        add_variable_declaration(
            'event_class_t *%s' % (crep.get_evclass(key),))
    start_function_definition('void _register_events(conf_class_t *class)')
    out('{\n', postindent = 1)
    if not events and not dml.globals.after_delay_infos:
        out('return;\n')
    else:
        for event in events:
            if (dml.globals.dml_version == (1, 2)
                and param_str(event, 'timebase') == 'stacked'
                and event.dimensions > 0):
                raise ICE(event, "stacked event array not supported")
            for indices in event.all_indices():
                out('%s%s = SIM_register_event("%s", class, 0, %s);\n'
                    % (crep.get_evclass(event),
                    ''.join('[' + str(i) + ']' for i in indices),
                    event.logname_anonymized(indices),
                    ', '.join(
                        cname
                        for (_, cname) in event_callbacks(event,
                                                          indices))))
        for (key, info) in dml.globals.after_delay_infos.items():
            out(('%s = SIM_register_event(%s, class, 0, %s, %s, %s, %s, '
                + 'NULL);\n')
                % (crep.get_evclass(key), string_literal(info.string_key),
                   info.cident_callback, '_destroy_simple_event_data',
                   info.cident_get_value, info.cident_set_value))
    out('}\n\n', preindent = -1)
    splitting_point()

def generate_events(device):
    events = device.get_recursive_components('event')

    for event in events:
        add_variable_declaration('event_class_t *%s%s' %
                                 (crep.get_evclass(event),
                                  ''.join('[' + str(s) + ']'
                                          for s in event.dimsizes)))

        for indices in event.all_indices():
            for (method, cname) in event_callbacks(event, indices):
                wrap_method(method, cname, indices)

def generate_reg_callback(meth, name):
    dev_t = crep.structtype(dml.globals.device)
    out('static bool\n')
    params = [t.declaration(p) for p, t in meth.inp] + [
        TPtr(t).declaration(p) for p, t in meth.outp]
    out('%s(void *_obj, const uint16 *indices, ' % (name,)
        + ', '.join(params) + ')\n')
    out('{\n', postindent = 1)
    out('%s *_dev = _obj;\n' % dev_t)
    fail = ReturnFailure(meth.site)
    with fail, crep.DeviceInstanceContext():
        inargs = [mkLit(meth.site, n, t) for n, t in meth.inp]
        outargs = [mkLit(meth.site, "*" + n, t) for n, t in meth.outp]
        code = [codegen_call(
                meth.site, meth,
                tuple(mkLit(meth.site, 'indices[%d]' % i, TInt(32, False))
                      for i in range(meth.dimensions)),
                inargs, outargs)]

    mkCompound(meth.site, code + [fail.nofail()]).toc_inline()
    out('}\n', preindent = -1)
    out('\n')

def generate_register_tables(device):
    for bank in device.get_components('bank'):
        if not bank.numbered_registers:
            continue
        i = 0
        regidxs = {}
        regs = []
        for r in bank.numbered_registers:
            dims = r.dimsizes or []
            getter = r.node.get_component('_get64')
            setter = r.node.get_component('_set64')
            generate_reg_callback(getter, '_DML_MI_%s' % crep.cref_method(getter),
                                  )
            generate_reg_callback(setter, '_DML_MI_%s' % crep.cref_method(setter))
            name = r.node.logname_anonymized(tuple("" for _ in dims),
                                             relative='bank')
            regs.append((name,
                         len(dims),
                         '_DML_MI_%s' % crep.cref_method(getter),
                         '_DML_MI_%s' % crep.cref_method(setter)))
            mark_method_referenced(method_instance(getter))
            mark_method_referenced(method_instance(setter))
            regidxs[r] = i
            i += 1
        out('// Register tables for %s\n' % bank.name)
        add_variable_declaration(
            'const _dml_reg_t _DML_R_%s[%d]' % (bank.name, len(regs)),
            '{\n%s}' % (
                ''.join('    { "%s", %d, %s, %s },\n' % (name, dim, read, write)
                        for (name, dim, read, write) in regs)))

        regmap = []
        # maps dimension length to map from coordinates to index
        coord_tables = {}

        for r in bank.numbered_registers:
            dim = len(r.dimsizes or [])
            for l in r.layout:
                if dim:
                    coords = coord_tables.setdefault(dim, {})
                    idx = coords.setdefault(l.coord, len(coords))
                    ref = '_DML_RI_%s_%u[%u]' % (bank.name, dim, idx)
                else:
                    ref = 'NULL'
                regmap.append((l.offset, regidxs[r], ref))
        for size, coords in sorted(coord_tables.items()):
            out('static const uint16 (_DML_RI_%s_%u[])[%u] = {\n'
                % (bank.name, size, size), postindent = 1)
            for _, coord in sorted((idx, coord)
                                   for (coord, idx) in list(coords.items())):
                out('{ ' + ', '.join("%d" % (i,) for i in coord) + ' },\n')
            out('};\n', preindent = -1)
        add_variable_declaration(
            'const _dml_reg_number_t _DML_RN_%s[%d]' % (bank.name, len(regmap)),
            '{\n%s}' % (
                ''.join('    { %u, %u, %s },\n' % (offset, regidx, coordref)
                        for (offset, regidx, coordref) in regmap)))
    out('\n')

def generate_serialize(device):
    out('// DML serialization functions\n')
    for func_code in serialize.serialize_function_code:
        out(func_code)
        splitting_point()
    prototypes.extend(serialize.serialize_prototypes)

def generate_state_notify(device):
    start_function_definition(
        f'void\n {crep.cname(device)}_notify_state_change('
        f'{crep.structtype(device)} *dev)')
    out('{\n', postindent = 1)
    out("if (!dev->_issuing_state_callbacks) {\n",
        postindent = 1)
    out("dev->_issuing_state_callbacks = true;\n")
    out("SIM_notify(&dev->obj, Sim_Notify_State_Change)"
        ";\n")
    out("dev->_issuing_state_callbacks = false;\n")
    out("}\n", preindent = -1)
    out("}\n", preindent = -1)

def generate_state_existence_callback(device):
    out('// DML notification registration callback\n')
    start_function_definition(f'void _{crep.cname(device)}'
                              + '_update_has_state_notifier('
                              + 'conf_object_t *_obj, '
                              + 'notifier_type_t type, '
                              + 'bool has_subscribers)')
    out('{\n', postindent = 1)
    devstruct = crep.structtype(device)
    out('if (type == Sim_Notify_State_Change) {\n', postindent = 1)
    out(f'{devstruct} *_dev = ({devstruct}*)_obj;\n')
    out("_dev->_has_state_callbacks = has_subscribers;\n")
    out("}\n", preindent = -1)
    out('}\n\n', preindent = -1)

def generate_marker(device):
    mangled_classname = ''.join(
        (ch if ch in ident_chars else '_')
        for ch in param_str(device, "classname"))
    out(f'const int _dml_gdb_marker_{mangled_classname} UNUSED = 0;\n')

def generate_identity_data_decls():
    def declare_id_infos(nodes, prefix):
        items = ('{"%s", %s, %d, %d}' %
                 ('dev' if node is dml.globals.device
                  else node.logname_anonymized(("%u",) * node.dimensions),
                  ('(const uint32 []) {%s}' % (', '.join(map(str, node.dimsizes)),)
                   if node.dimensions else 'NULL'),
                  node.dimensions,
                  node.uniq)
                 for node in nodes)
        init = '{\n%s\n}' % (',\n'.join('    %s' % (item,) for item in items),)
        add_variable_declaration(
            f'const _id_info_t {prefix}_id_infos[{len(nodes)}]',
            init)

        add_variable_declaration(f'ht_str_table_t {prefix}_id_info_ht',
                                 'HT_STR_NULL(false)')

    declare_id_infos(objects.Device.objects, '')
    if objects.Device.hooks:
        declare_id_infos(objects.Device.hooks, '_hook')

def generate_class_var_decl():
    add_variable_declaration('conf_class_t *_dev_class')

def generate_init_identity_hashtable():
    start_function_definition('void _initialize_identity_ht(void)')
    out('{\n', postindent=1)
    # We add an entry to alias the device name to the device object
    # for backwards checkpoint compatibility (SIMICS-19681).
    # This gets overridden if there's a top-level object that shares its name
    # with the device.
    out(f'ht_insert_str(&_id_info_ht, "{dml.globals.device.name}", '
        + f'&_id_infos[{dml.globals.device.uniq - 1}]);\n')
    out('for (uint64 i = 0; i < %d; ++i) {\n' % (len(objects.Device.objects),),
        postindent=1)
    out('ht_insert_str(&_id_info_ht, _id_infos[i].logname, &_id_infos[i]);\n')
    out('}\n', preindent=-1)
    if objects.Device.hooks:
        out('for (uint64 i = 0; i < %d; ++i) {\n' % (len(objects.Device.hooks),),
            postindent=1)
        out('ht_insert_str(&_hook_id_info_ht, _hook_id_infos[i].logname, '
            + '&_hook_id_infos[i]);\n')
        out('}\n', preindent=-1)
    out('}\n', preindent=-1)
    splitting_point()

def generate_hook_auxiliary_info_array():
    items = []
    for hook in objects.Device.hooks:
        offset = ('offsetof(%s, %s)'
                  % (crep.structtype(dml.globals.device),
                     crep.cref_hook(
                         hook, (mkIntegerConstant(hook.site, 0, False),)
                         * hook.dimensions)))

        try:
            typeseq_uniq = get_type_sequence_info(
                hook.msg_types, create_new=True).uniq
            items.append('{%s, %d, %d}' % (offset, typeseq_uniq, hook.uniq))
        except DMLUnkeyableType:
            pass # already reported
    if objects.Device.hooks:
        init = '{\n%s\n}' % (',\n'.join(f'    {item}' for item in items),)
        add_variable_declaration(
            'const _dml_hook_aux_info_t '
            + f'_hook_aux_infos[{len(objects.Device.hooks)}]',
            init)

def generate_alloc(device):
    start_function_definition(
        'conf_object_t *\n' + crep.cname(device)+'_alloc(conf_class_t *cls)')
    out('{\n', postindent = 1)
    out(crep.structtype(device) + ' *_dev = MM_ZALLOC(1, '
        + crep.structtype(device) + ');\n')
    objarg = '&_dev->obj'
    out('_dev->_immediate_after_state = MM_ZALLOC(1, '
        + '_dml_immediate_after_state_t);\n')
    out('return %s;\n' % objarg)
    out('}\n\n', preindent = -1)

def generate_initialize(device):
    start_function_definition(
        'lang_void *\n' +
        crep.cname(device)+'_init(conf_object_t *_obj)')
    out('{\n', postindent = 1)
    devstruct = crep.structtype(device)
    out(devstruct+' *_dev = ('+devstruct+'*)_obj;\n')

    out('_init_port_objs(_dev);\n')
    out('_init_static_vars(_dev);\n')
    out('_init_data_objs(_dev);\n')

    # Initialize table for tracking log-once feature
    out('ht_init_int_table(&(_dev->_subsequent_log_ht));\n')

    # All QINIT does is zero-initialization, which is already covered --
    # this is a Just In Case if the representation of queues were to be
    # changed such that zero-initialization would not be valid.
    out('QINIT(_dev->_immediate_after_state->queue);\n')

    with crep.DeviceInstanceContext():
        if dml.globals.dml_version == (1, 2):
            # Functions called from init_object shouldn't throw any
            # exceptions. But we don't want to force them to insert try-catch
            # in the init method.
            stmts = []
            for method_name in ['init', 'hard_reset']:
                method = device.get_component(method_name, 'method')
                with InitFailure(method.site):
                    stmts.append(codegen_call_byname(
                        method.site, device, (), method_name, [], []))

            mkCompound(device.site, stmts).toc()
        else:
            codegen_inline_byname(device, (), '_init', [], [],
                                  device.site).toc()

    out('SIM_add_notifier(_obj, Sim_Notify_Object_Delete, _obj, '
        + crep.cname(device) + '_pre_delete, NULL);\n')

    out('return _obj;\n')
    out('}\n\n', preindent = -1)

def generate_dealloc(device):
    start_function_definition(
        f'void {crep.cname(device)}_dealloc(conf_object_t *dev)')
    out('{\n', postindent = 1)
    out('MM_FREE(dev);\n')
    out('}\n\n', preindent = -1)

def generate_finalize(device):
    start_function_definition(
        f'void {crep.cname(device)}_finalize(conf_object_t *_obj)')
    out('{\n', postindent = 1)
    out(crep.structtype(device) + ' *_dev UNUSED = ('
        + crep.structtype(device) + ' *)_obj;\n')

    with crep.DeviceInstanceContext():
        if dml.globals.dml_version == (1, 2):
            # Functions called from new_instance shouldn't throw any
            # exceptions.  But we don't want to force them to insert try-catch
            # in the init method.
            with LogFailure(
                    device.get_component('post_init', 'method').site,
                    device, ()):
                code = codegen_inline_byname(device, (), 'post_init', [], [],
                                             device.site)
        else:
            code = codegen_inline_byname(device, (), '_post_init', [], [],
                                     device.site)
    code.toc_inline()
    out('}\n\n', preindent = -1)

def generate_objects_finalized(device):
    start_function_definition(
        f'void {crep.cname(device)}_objects_finalized(conf_object_t *_obj)')
    out('{\n', postindent = 1)
    out(crep.structtype(device) + ' *_dev UNUSED = ('
        + crep.structtype(device) + ' *)_obj;\n')
    out('_dev->_immediate_after_state->warn_upon_deletion = true;\n')
    out('_DML_execute_immediate_afters_now(_obj, '
        + '_dev->_immediate_after_state);\n')
    out('}\n\n', preindent = -1)

def generate_pre_delete(device):
    start_function_definition(
        ('void %s_pre_delete(conf_object_t *_subscriber,'
         ' conf_object_t *_obj, lang_void *_data)') % crep.cname(device))
    out('{\n', postindent = 1)
    out(crep.structtype(device) + ' *_dev = ('
        + crep.structtype(device) + ' *)_obj;\n')
    out('_DML_pre_delete_cancel_immediate_afters(_obj, '
        + '_dev->_immediate_after_state);\n')

    # Cancel all events. This is safety measure: we wouldn't need to do this
    # if all event queues used by clocks were those of 'cycle-counter's and
    # 'ps-counter's, or every clock otherwise took care to cancel events of
    # objects that get marked for deletion, but neither is explicitly enforced
    # by the Simics API.
    with crep.DeviceInstanceContext():
        events = device.get_recursive_components('event')

        by_dims = {}
        for event in events:
            by_dims.setdefault(event.dimsizes, []).append(event)

        for (dims, events) in by_dims.items():
            for i in range(len(dims)):
                out(f'for (uint32 _i{i} = 0; _i{i} < {dims[i]}; _i{i}++) {{\n',
                    postindent=1)

            indices = tuple(mkLit(device.site, f'_i{i}', TInt(32, False))
                            for i in range(len(dims)))
            for event in events:
                method = event.get_component('_cancel_all', 'method')
                codegen_inline(method.site, method, indices, [], []).toc()
            for i in range(len(dims)):
                out('}\n', preindent=-1)

        out('if (SIM_object_clock(_obj)) {\n', postindent=1)
        out('_cancel_simple_delay_events(_obj, SIM_object_clock(_obj), '
            + 'NULL);\n')
        out('_cancel_simple_delay_events(_obj, SIM_picosecond_clock(_obj), '
            + 'NULL);\n')
        out('}\n', preindent=-1)

    if dml.globals.api_version < compat.api_7:
        out(f'{crep.cname(device)}_deinit(_obj);\n')
    out('}\n\n', preindent = -1)

def generate_deinit(device):
    start_function_definition(
        'void %s_deinit(conf_object_t *_obj)' % (
            crep.cname(device),))
    out('{\n', postindent = 1)
    out(crep.structtype(device) + ' *_dev UNUSED = ('
        + crep.structtype(device) + ' *)_obj;\n')
    with crep.DeviceInstanceContext():
        codegen_inline_byname(device, (), '_destroy', [], [],
                              device.site).toc()

        # Execute all immediate afters posted by destruction code
        # Unlike immediate afters pending from before destruction, these
        # callbacks are executed and not warned about, as they can be assumed
        # to be aware that they are called in a deletion context.
        out('_DML_execute_immediate_afters_now(_obj, '
            + '_dev->_immediate_after_state);\n')

        # Cancel all pending afters on hooks
        by_dims = {}
        for hook in objects.Device.hooks:
            by_dims.setdefault(hook.dimsizes, []).append(hook)

        for (dims, hooks) in by_dims.items():
            for i in range(len(dims)):
                out(f'for (uint32 _i{i} = 0; _i{i} < {dims[i]}; _i{i}++) {{\n',
                    postindent=1)

            indices = tuple(mkLit(device.site, f'_i{i}', TInt(32, False))
                            for i in range(len(dims)))
            for hook in hooks:
                out('_DML_free_hook_queue('
                    + f'&_dev->{crep.cref_hook(hook, indices)}.queue);\n')
            for i in range(len(dims)):
                out('}\n', preindent=-1)

        # Free the tables used for log_once after all calls into device code
        # are done
        out('_free_table(&_dev->_subsequent_log_ht);\n')

    out('QFREE(_dev->_immediate_after_state->queue);\n')
    out('if (likely(!_dev->_immediate_after_state->posted)) {\n', postindent=1)
    out('MM_FREE(_dev->_immediate_after_state);\n')
    out('} else {\n', preindent=-1, postindent=1)
    # Let the posted event deallocate the immediate after state
    out('_dev->_immediate_after_state->deleted = true;\n')
    out('}\n', preindent = -1)

    out('}\n\n', preindent = -1)
    splitting_point()

def generate_reset(device, hardness):
    out('void\n')
    out(hardness + '_reset_' + crep.cname(device) + '('
        + crep.structtype(device) + ' *_obj)\n')
    out('{\n', postindent = 1)
    out(crep.structtype(device) + ' *_dev UNUSED = ('
        + crep.structtype(device) + ' *)_obj;\n\n')
    method_name = hardness + '_reset'
    method = device.get_component(method_name, 'method')
    with LogFailure(method.site, device, ()), crep.DeviceInstanceContext():
        code = codegen_call_byname(method.site, device, (),
                                   method_name, [], [])
    code.toc_inline()
    out('}\n\n', preindent = -1)

# {(B, C): A} means that a static array 'const uint32 _indices_B_C[A][B][C][3]'
# will be generated, where element [x][y][z] is {x,y,z}
_index_enumeration_pool = {}
def get_index_enumeration(site, dimsizes: tuple):
    '''return an expression with a nested array of index enumerations for
    the given dimsizes, e.g. for dimsizes=(2,3), return an array containing
    {{{0, 0}, {0, 1}, {0, 2}}, {{1, 0}, {1, 1}, {1, 2}}}'''
    assert dimsizes
    lower = dimsizes[1:]
    _index_enumeration_pool[lower] = max(dimsizes[0],
                                      _index_enumeration_pool.get(lower, 0))
    t = TArray(TInt(32, False, const=True),
               mkIntegerLiteral(site, len(dimsizes)))
    for dim in lower[::-1]:
        t = TArray(t, mkIntegerLiteral(site, dim))
    return mkLit(site, '_indices_%s' % ('_'.join(map(str, lower))), TPtr(t))

def generate_index_enumerations():
    def index_enumeration(dimsizes):
        assert dimsizes
        lines = [('{', '%d}' % (i,)) for i in range(dimsizes[-1])]
        for dim in dimsizes[-2::-1]:
            (a, b) = lines[0]
            lines[0] = ('{' + a, b)
            (a, b) = lines[-1]
            lines[-1] = (a, b + '}')
            lines = [(a, '%d, %s' % (i, b))
                     for i in range(dim)
                     for (a, b) in lines]
        return ',\n    '.join('%s%s' % (a, b) for (a, b) in lines)
    assert index_enumeration((2, 3)) == '''{{0, 0},
    {0, 1},
    {0, 2}},
    {{1, 0},
    {1, 1},
    {1, 2}}'''

    global _index_enumeration_pool
    for lower in sorted(_index_enumeration_pool):
        dimsizes = (_index_enumeration_pool[lower],) + lower
        add_variable_declaration('const uint32 _indices_%s%s' % (
            '_'.join(map(str, lower)),
            ''.join('[%d]' % (dim,) for dim in dimsizes + (len(dimsizes),))),
            init='{%s}' % (index_enumeration(dimsizes),))
    out('\n')
    # capture late additions with a crash
    _index_enumeration_pool = None

_int_tuple_pool = {}
def tuple_as_uint32_array(site, numbers, hint):
    '''Given a tuple of integer constants, return a reference to an array
    containing these constants. '''

    assert isinstance(numbers, tuple)
    if not numbers:
        return mkLit(site, 'NULL', TPtr(TVoid()))
    if numbers not in _int_tuple_pool:
        suffix = '%d_%s' % (len(_int_tuple_pool), hint)
        _int_tuple_pool[numbers] = suffix
    return mkLit(site, '_tuple%s' % (_int_tuple_pool[numbers],),
                 TArray(TInt(32, False, const=True),
                        mkIntegerLiteral(site, len(numbers))))

def generate_tuple_table():
    global _int_tuple_pool
    for (numbers, suffix) in _int_tuple_pool.items():
        add_variable_declaration(
            'const uint32 _tuple%s[%d]' % (suffix, len(numbers)),
            init='{%s}' % (',\n  '.join(map(str, numbers)),))
    # capture late additions with a crash
    _int_tuple_pool=None

def generate_init_port_objs(device):
    start_function_definition(
        'void _init_port_objs(%s *_dev)' % (crep.structtype(device),))
    out('{\n', postindent = 1)
    for port in device.get_recursive_components('port', 'bank', 'subdevice'):
        if port.name is None:
            # anonymous bank
            continue
        (_, prefix, suffix) = find_port_parent(
            port, ('%d',) * port.dimensions)
        portname = prefix + suffix
        if port.dimensions:
            index_enumeration = get_index_enumeration(port.site, port.dimsizes)
            for (i, sz) in enumerate(port.dimsizes):
                out(f'for (int _i{i} = 0; _i{i} < {sz}; ++_i{i}) {{\n',
                    postindent=1)
            fmtargs = ''.join(f', _i{i}' for i in range(port.dimensions))
            out(f'strbuf_t portname = sb_newf("{portname}"{fmtargs});\n')
            loop_indices = tuple(
                mkLit(port.site, '_i%d' % i, TInt(32, False))
                for i in range(port.dimensions))
            index_array = "%s%s" % (
                index_enumeration.read(),
                ''.join('[_i%d]' % (i,) for i in range(port.dimensions)))
            out('_dev->%s = _init_port_object(&_dev->obj'
                % (crep.cref_portobj(port, loop_indices),)
                + ', sb_str(&portname), %d, %s);\n'
                % (port.dimensions, index_array))
            out('sb_free(&portname);\n')
            for _ in range(port.dimensions):
                out('}\n', preindent=-1)
        else:
            out(f'_dev->{crep.cref_portobj(port, ())} = _init_port_object('
                f'&_dev->obj, "{portname}", 0, NULL);\n')
    out('}\n', preindent=-1)

def generate_init_static_vars(device):
    start_function_definition(
        'void _init_static_vars(%s *_dev)' % (crep.structtype(device),))
    out('{\n', postindent = 1)
    for (_, init) in dml.globals.static_vars:
        if init:
            for line in init.splitlines(keepends=True):
                out(line)
    out('}\n\n', preindent = -1)

def generate_init_data_objs(device):
    start_function_definition(
        'void _init_data_objs(%s *_dev)' % (crep.structtype(device),))
    out('{\n', postindent = 1)
    with crep.DeviceInstanceContext():
        for node in device.initdata:
            # Usually, the initializer is constant, but we permit that it
            # depends on index. When the initializer is constant, we use a loop
            # to initialize all instances of the variable across arrays; if the
            # initializer depends on an index variable, then we unfold the loop
            # to allow the initializer to be evaluated as a constant expression
            # once for every index.
            # TODO: we should change initializer to permit certain non-constant
            # expressions, as long as they don't access the device instance.
            # That way, it would always work to generate a loop.

            try:
                # only data/method obj
                assert not node.isindexed()
                init = eval_initializer(
                    node.site, node._type, node.astinit,
                    Location(node.parent, static_indices(node)),
                    global_scope, True)
            # mainly meant to capture EIDXVAR; for other errors, the error will
            # normally re-appear when evaluating per instance
            except DMLError:
                with allow_linemarks():
                    for indices in node.all_indices():
                        index_exprs = tuple(mkIntegerLiteral(node.site, i)
                                            for i in indices)
                        nref = mkNodeRef(node.site, node, index_exprs)
                        try:
                            init = eval_initializer(
                                node.site, node._type, node.astinit,
                                Location(node.parent, index_exprs),
                                global_scope, True)
                        except DMLError as e:
                            report(e)
                        else:
                            markers = ([('store_writes_const_field', 'FALSE')]
                                       if deep_const(node._type) else [])
                            coverity_markers(markers, init.site)
                            init.assign_to(nref, node._type)
            else:
                index_exprs = ()
                for (i, sz) in enumerate(node.dimsizes):
                    var = 'i%d' % (i,)
                    out(('for (int %s = 0; %s < %s; ++%s) {\n'
                         % (var, var, sz, var)),
                        postindent=1)
                    index_exprs += (mkLit(node.site, var, TInt(64, True)),)
                nref = mkNodeRef(node.site, node, index_exprs)
                with allow_linemarks():
                    markers = ([('store_writes_const_field', 'FALSE')]
                               if deep_const(node._type) else [])
                    coverity_markers(markers, init.site)
                    init.assign_to(nref, node._type)
                for _ in range(node.dimensions):
                    out('}\n', postindent=-1)
    out('}\n\n', preindent = -1)
    splitting_point()

ident_chars = set(chr(i)
    for (start, end) in [('A', 'Z'), ('a', 'z'), ('0', '9'), ('_', '_')]
    for i in range(ord(start), ord(end) + 1))
def init_function_name(device, outprefix):
    if dml.globals.api_version <= compat.api_4_8:
        return 'initialize_' + crep.cname(device)
    return '_initialize_' + ''.join(
        (ch if ch in ident_chars else '_') for ch in outprefix)

def generate_init(device, initcode, outprefix):
    doc = get_long_doc(device)
    if doc == None:
        doc = device.name + ' device'
    doc = mkStringConstant(device.site, doc)

    sdoc = get_short_doc(device)
    if sdoc:
        sdoc = mkStringConstant(device.site, sdoc)

    #banks = device.get_components('bank')

    out('conf_class_t *\n')
    out('%s(void)\n' % (init_function_name(device, outprefix)))
    out('{\n', postindent = 1)
    #if banks:
    #    out('io_memory_interface_t *io_interface;')
    out('\n')
    out('const class_info_t funcs = {\n', postindent = 1)
    out('.alloc = '+crep.cname(device)+'_alloc,\n')
    out('.init = '+crep.cname(device)+'_init,\n')
    out('.finalize = '+crep.cname(device)+'_finalize,\n')
    out('.objects_finalized = '+crep.cname(device)+'_objects_finalized,\n')
    if dml.globals.api_version >= compat.api_7:
        out('.deinit = '+crep.cname(device)+'_deinit,\n')
    out('.dealloc = '+crep.cname(device)+'_dealloc,\n')
    out('.description = '+doc.read()+',\n')
    if sdoc:
        out('.short_desc = '+sdoc.read()+',\n')
    else:
        report(WNSHORTDESC(device.site))
    out('};\n', preindent = -1)
    out('\n')
    out('conf_class_t *class = SIM_create_class("'
        + param_str_fixup(device, 'classname', '') + '", &funcs);\n')
    out('_dev_class = class;\n')
    out('if (SIM_clear_exception() != SimExc_No_Exception) {\n', postindent = 1)
    out('fprintf(stderr, "Failed to register class '
        + param_str_fixup(device, 'classname', '')
        + ': %s\\n", SIM_last_error());\n')
    out('return NULL;\n')
    out('}\n', preindent = -1)
    out('\n')

    if dml.globals.dml_version != (1, 2) or dml.globals.state_change_dml12:
        out('GET_API_FUNCTION(_SIM_rtn, SIM_register_tracked_notifier);\n')
        out('if (_SIM_rtn != NULL) {\n', postindent = 1)
        out('_SIM_rtn(class, '
            'Sim_Notify_State_Change, '
            '"Notifier on potential DML state change" , ' +
            f'_{crep.cname(device)}_update_has_state_notifier'
            ');\n')
        out('}\n', preindent = -1)

    out('_initialize_traits();\n')
    out('_initialize_identity_ht();\n')
    out('_initialize_vtable_hts();\n')
    out('_initialize_typeseq_after_on_hook_hts();\n')
    out('_register_events(class);\n')
    # initcode is independently indented
    out(initcode.buf, preindent = -1, postindent = 1)
    out('\n')
    if dml.globals.log_groups:
        out('const char *const log_groups[] = {\n', postindent = 1)
        for g in dml.globals.log_groups:
            out('"%s",\n' % (g,))
        out('NULL\n')
        out('};\n', preindent = -1)
        out('SIM_log_register_groups(class, log_groups);\n')
        for port in device.get_recursive_components('port', 'bank',
                                                    'subdevice'):
            if port.name is None:
                assert dml.globals.dml_version == (1, 2)
            else:
                out('SIM_log_register_groups(%s, log_groups);\n'
                    % (port_class_ident(port),))
    out('\n')
    out('_startup_calls();\n')
    out('return class;\n')
    out('}\n\n', preindent = -1)
    splitting_point()

def generate_static_trampoline(func):
    # static trampolines never need to be generated for independent methods
    assert not func.independent
    params = [("_obj", TPtr(TNamed("conf_object_t")))] + func.cparams[1:]
    params_string = ('void' if not params
                     else ", ".join(t.declaration(n) for (n, t) in params))
    start_function_definition(func.rettype.declaration(
        "%s(%s)" % ("_trampoline" + func.get_cname(), params_string)))
    out("{\n", postindent=1)
    out('ASSERT(_obj);\n')
    out('ASSERT(SIM_object_class(_obj) == _dev_class);\n')
    (name, typ) = func.cparams[0]
    out("%s = (%s)_obj;\n" % (typ.declaration(name), typ.declaration("")))
    out("%s%s(%s);\n" % ("" if func.rettype.void
                         else func.rettype.declaration("result") + " = ",
                         func.get_cname(),
                         ", ".join(n for (n, t) in func.cparams)))
    output_dml_state_change(name)
    if not func.rettype.void:
        out("return result;\n")
    out("}\n", preindent=-1)

def generate_extern_trampoline(exported_name, func):
    params = (func.cparams if func.independent else
              [("_obj", TPtr(TNamed("conf_object_t")))] + func.cparams[1:])
    params_string = ('void' if not params
                     else ", ".join(t.declaration(n) for (n, t) in params))
    out("extern %s\n" % (func.rettype.declaration(
                         "%s(%s)" % (exported_name, params_string))))
    out("{\n", postindent=1)
    out("%s%s(%s);\n" % ("return " * (not func.rettype.void),
                         "_trampoline" * (not func.independent)
                         + func.get_cname(),
                         ", ".join(n for (n, t) in params)))
    out("}\n", preindent=-1)

def generate_extern_trampoline_dml12(exported_name, func):
    out("static UNUSED %s\n" % (func.rettype.declaration(
        "%s(%s)" % (exported_name,
                    ", ".join(t.declaration(n)
                              for (n, t) in func.cparams)))))
    out("{\n", postindent=1)
    out("%s%s(%s);\n" % ("" if func.rettype.void else "return ",
                         func.get_cname(),
                         ", ".join(n for (n, t) in func.cparams)))
    out("}\n", preindent=-1)

def generate_each_in_table(trait, instances):
    items = []
    for (node, subnodes) in instances:
        ident = EachIn.index_ident(node, trait)
        if subnodes:
            add_variable_declaration(f'const uint32 {ident}', str(len(items)))

        for sub in subnodes:
            # vtables exist, because trait instances are marked referenced
            # when an 'each in' instance is referenced
            ancestry_path = sub.traits.ancestry_paths[trait][0]
            base = '&%s%s' % (
                sub.traits.vtable_cname(ancestry_path[0]),
                ''.join('.' + cident(t.name)
                        for t in ancestry_path[1:]))
            num = reduce(operator.mul, sub.dimsizes, 1)
            uniq = sub.uniq
            items.append("{%s, %d, %d}" % (base, num, uniq))
    arrayname = EachIn.array_ident(trait)
    if items:
        init = '{\n%s\n}' % (',\n'.join(f'    {item}' for item in items),)
        add_variable_declaration(
            f'const _vtable_list_t {arrayname}[{len(items)}]', init)
    else:
        # avoid array of size 0, e.g. when foreach:ing over `each X in
        # Y` where template X is never instantiated. GCC 11 complains
        # about indexing an empty array in the loop body, but accepts
        # dereferencing NULL.
        add_variable_declaration(
            f'const _vtable_list_t *const {arrayname}', 'NULL')

def generate_each_in_tables():
    by_trait = {}
    for ((node, trait), subobjs) in list(EachIn.instances.items()):
        by_trait.setdefault(trait, []).append((node, subobjs))
    for t in by_trait:
        generate_each_in_table(t, by_trait[t])
    for t in Set(dml.globals.traits.values()).difference(by_trait):
        # Need by shared methods that belong to unused templates;
        # when dereferencing sequence params, these methods reference
        # the base array.
        add_variable_declaration(
            f'const _vtable_list_t *const {EachIn.array_ident(t)}', 'NULL')

def generate_trait_deserialization_hashtables(device):
    inserts = []
    assert dml.globals.object_trait
    for trait in dml.globals.serialized_traits.traits:
        if trait.empty() or trait is dml.globals.object_trait:
            continue
        add_variable_declaration('ht_int_table_t '
                                 + f'_{cident(trait.name)}_vtable_ht',
                                 'HT_INT_NULL()')
        for node in dml.globals.trait_instances.get(trait, ()):
            node.traits.mark_referenced(trait)
            ancestry_path = node.traits.ancestry_paths[trait][0]
            structref = node.traits.vtable_cname(ancestry_path[0])
            pointer = '(&%s)' % ('.'.join([structref] + [
                cident(t.name) for t in ancestry_path[1:]]))
            inserts.append(f'ht_insert_int(&_{cident(trait.name)}_vtable_ht, '
                           + f'{node.uniq}, {pointer});\n')

    start_function_definition('void _initialize_vtable_hts(void)')
    out('{\n', postindent=1)
    for line in inserts:
        out(line)
    out('}\n', preindent=-1)

def generate_object_vtables_array():
    items = []
    for node in objects.Device.objects:
        if node.traits:
            node.traits.mark_referenced(dml.globals.object_trait)
            ancestry_path = node.traits.ancestry_paths[dml.globals.object_trait][0]
            structref = node.traits.vtable_cname(ancestry_path[0])
            pointer = '(&%s)' % ('.'.join([structref] + [
                cident(t.name) for t in ancestry_path[1:]]))
        else:
            pointer = 'NULL'
        items.append(pointer)
    init = '{%s}' % (', '.join(items),)
    add_variable_declaration(
        f'void * const _object_vtables[{len(objects.Device.objects)}]', init)

def generate_port_object_assocs_array():
    items = []
    for node in objects.Device.objects:
        object_node = (node if node.objtype in {'device', 'bank',
                                                'subdevice', 'port'}
                       else node.object_parent)
        if (object_node is not dml.globals.device
            # Anonymous banks
            and (compat.dml12_misc not in dml.globals.enabled_compat
                 or object_node.ident is not None)):
            port_obj_offset = (
                f'offsetof({crep.structtype(dml.globals.device)}, '
                + f'{crep.cref_portobj(object_node, ())})')
            index_divisor = str(reduce(
                operator.mul,
                itertools.islice(node.dimsizes, object_node.dimensions, None),
                1))
        else:
            port_obj_offset = index_divisor = '0'
        items.append(f'{{{port_obj_offset}, {index_divisor}}}')
    init = f'{{{", ".join(items)}}}'
    add_variable_declaration(
        'const _dml_port_object_assoc_t '
        + f'_port_object_assocs[{len(objects.Device.objects)}]',
        init)

def generate_trait_method(m):
    code = m.codegen_body()
    out('/* %s */\n' % (str(m),))
    start_function_definition(m.declaration())
    with allow_linemarks():
        site_linemark(m.astbody.site)
        out('{\n', postindent=1)
        code.toc_inline()
        site_linemark(m.rbrace_site)
        out('}\n', preindent=-1)

def generate_adjustor_thunk(traitname, name, inp, outp, throws, independent,
                            vtable_path, def_path, hardcoded_impl=None):
    generated_name = "__adj_%s__%s__%s" % (
        traitname, '__'.join(t.name for t in vtable_path), name)
    rettype = c_rettype(outp, throws)
    out('static ' + rettype.declaration('\n%s' % (generated_name,)))
    vtable_trait = vtable_path[-1]
    assert vtable_trait is def_path[-1]
    implicit_inargs = vtable_trait.implicit_args()
    preargs = crep.maybe_dev_arg(independent) + implicit_inargs
    inargs = c_inargs(inp, outp, throws)
    out('(%s)\n{\n' % (", ".join(t.declaration(n) for (n, t) in (preargs
                                                                 + inargs))),
        postindent=1)
    # TODO should ideally be agnostic of what implicit_inargs is
    [(vt_name, vtable_trait_type)] = implicit_inargs
    assert vtable_trait_type.trait is vtable_trait
    out('%s.trait = &((struct _%s *) DOWNCAST(%s, %s, %s).trait)->%s;\n' % (
        vt_name, cident(traitname), vt_name, cident(traitname),
        '.'.join(cident(t.name) for t in vtable_path),
        '.'.join(cident(t.name) for t in def_path)))
    if not rettype.void:
        out('return ')
    fun = hardcoded_impl or ('((struct _%s *) %s.trait)->%s'
                             % (cident(vtable_trait.name), vt_name, name))
    out('%s(%s);\n' % (fun, ", ".join(['_dev'] * (not independent) + [vt_name]
                                      + [name for (name, _) in inargs])))
    out('}\n', preindent=-1)
    return generated_name

def tinit_args(trait):
    '''Return the arguments of a trait's tinit method, excluding the first
    '_indices' argument. Represented as a list of vtable member names,
    also used as argument names.'''
    return sorted(itertools.chain(
        trait.vtable_methods,
        trait.vtable_params,
        trait.vtable_sessions,
        trait.vtable_hooks,
        trait.vtable_memoized_outs,
        (name for name in trait.ancestor_vtables
         if (name not in trait.method_impl_traits
             or any(impl_trait.method_impls[name].overridable
                    for impl_trait in trait.method_impl_traits[name])))))

def method_tinit_arg(trait, parent, name, scrambled_name):
    '''Return the argument passed by a trait's tinit method to its
    parent's tinit method, as a string. 'name' is the trait member
    corresponding to the argument'''
    vtable_trait = trait.ancestor_vtables[name]
    ancestry_paths = trait.ancestry_paths[vtable_trait]
    canonical_path = ancestry_paths[0]
    impl_traits = trait.method_impl_traits.get(name, [])
    if parent is canonical_path[0]:
        # An overriding function assumes it will be placed in
        # the vtable entry on the canonical path. So any
        # non-NULL arg to tinit will be passed on as-is to the
        # parent trait.
        override = trait.method_impls.get(name, None)
        if override:
            # Parent's method is always overridden, either by
            # this trait or by a subtrait
            if override.overridable:
                return '%s == NULL ? %s : %s' % (scrambled_name,
                                                 override.cname(),
                                                 scrambled_name)
            else:
                return override.cname()
        elif not impl_traits:
            # Abstract method, so method is guaranteed to be
            # overridden (meaning that the 'name' parameter of this
            # trait's tinit method will always be non-NULL)
            return scrambled_name
        elif len(impl_traits) > 1:
            # If multiple ancestors provide an implementation, then
            # there is no correct default implementation: it must be provided.
            return scrambled_name

        [impl_trait] = impl_traits
        if parent.implements(impl_trait):
            # if this function is called, it means that 'name' is
            # overridable in parent. Thus, if the implementation is in
            # a superclass of parent, then the implementation is
            # overridable, meaning that the current trait is also
            # parameterized by the method.
            assert name in tinit_args(trait)
            # If method is overridden, then 'name' is the override. If
            # the method is not overridden, then 'name' is NULL, but
            # the parent's default implementation does the right
            # thing, so we can pass on the NULL value.
            return scrambled_name
        else:
            # The correct default implementation is provided
            # by some other trait, which apparently is not on
            # the canonical path. The default implementation
            # requires its trait parameter to be an upcast of
            # an impl_trait instance. So we create an adjustor
            # thunk which re-casts the trait to such an
            # instance, and calls the default implementation
            # with that trait using a direct call. (It would
            # also have been correct to use an indirect call,
            # but that would be slower).
            # We pick the canonical instance of impl_trait;
            # any other instance would also have worked, but
            # the canonical one is likely to be faster
            impl_path = []
            if trait is not impl_trait:
                impl_path.extend(trait.ancestry_paths[impl_trait][0])
            if impl_trait is not vtable_trait:
                impl_path.extend(impl_trait.ancestry_paths[vtable_trait][0])
            # impl_path[0] is a direct parent which provides
            # the correct default implementation
            assert impl_path[0] != parent
            (_, inp, outp, throws, independent, _, _) = \
                vtable_trait.vtable_methods[name]
            method_impl = impl_trait.method_impls[name]
            thunk = generate_adjustor_thunk(
                trait.name, name, inp, outp, throws, independent,
                canonical_path, impl_path,
                method_impl.cname())
            if not method_impl.overridable:
                # in this case, 'name' is not a parameter of this
                # trait's tinit function
                return thunk
            return "%s == NULL ? %s : %s" % (scrambled_name,
                                             thunk,
                                             scrambled_name)
    else:
        # not on canonical path; need to create an adjustor thunk
        # that trampolines to the implementation on the canonical path
        vtable_path = (parent,) + parent.ancestry_paths[vtable_trait][0]
        (_, inp, outp, throws, independent, _, _) = \
            vtable_trait.vtable_methods[name]
        # Avoid an indirect call in the adjustor thunk, for
        # methods that are not overridable
        if impl_traits and not impl_traits[0].method_impls[name].overridable:
            assert len(impl_traits) == 1
            [impl_trait] = impl_traits
            hardcoded_fun = impl_trait.method_impls[name].cname()
            def_path = []
            if trait is not impl_trait:
                def_path.extend(trait.ancestry_paths[impl_trait][0])
            if impl_trait is not vtable_trait:
                def_path.extend(impl_trait.ancestry_paths[vtable_trait][0])
        else:
            hardcoded_fun = None
            def_path = canonical_path
        thunk = generate_adjustor_thunk(
            trait.name, name, inp, outp, throws, independent,
            vtable_path, def_path,
            hardcoded_fun)
        if len(impl_traits) == 1 and parent.implements(impl_traits[0]):
            assert name in tinit_args(trait)
            # This parent's default implementation does the
            # right thing, unless overridden
            return "%s == NULL ? NULL : %s" % (scrambled_name, thunk)
        else:
            return thunk

def print_vtable_struct_declaration(trait):
    out('struct _%s {\n' % cident(trait.name), postindent=1)
    for p in trait.direct_parents:
        out("struct _%s %s;\n" % (cident(p.name), cident(p.name)))
    for (name, (_, ptype)) in list(trait.vtable_params.items()):
        if isinstance(realtype(ptype), TTraitList):
            out(f"_each_in_param_t {name};\n")
        else:
            out(f'{TPtr(ptype).declaration(name)};\n')
    for name in trait.vtable_sessions:
        out(f'uint32 {name};\n') # device struct offset
    for name in trait.vtable_hooks:
        out(f'uint32 {name};\n') # hook unique id
    for (name, (_, inp, outp, throws, independent, startup,
                memoized)) in trait.vtable_methods.items():
        t = trait.vtable_method_type(inp, outp, throws, independent)
        out(f'{t.declaration(name)};\n')

    for (name, memo_outs_struct) in trait.vtable_memoized_outs.items():
        decl = TPtr(memo_outs_struct).declaration(name)
        out(f'{decl};\n')
    out('};\n', preindent=-1)

def generate_tinit(trait):
    '''Generate a _tinit_* function, which initializes a trait vtable.

The function has one argument for every overridable method in the trait.
Argument for abstract methods must be non-NULL, while arguments for default
methods are optional, and NULL denotes that the default implementation is used.

If a method appears multiple times in the trait's vtable (can happen
on multiple inheritance), then the corresponding _tinit_* argument
will be stored on the canonical ancestry path, i.e., in the first
occurrence of the method in the vtable. The _tinit_* function is
responsible for filling in adjustor thunks in the remaining vtable
fields.
    '''
    # prevent argument names from shadowing trait types
    def scramble_argname(name):
        return "_%s_%s" % (trait.name, name)
    initializers = []
    for name in sorted(trait.vtable_methods):
        scrambled_name = scramble_argname(name)
        if name in trait.method_impls:
            value = '%s == NULL ? %s : %s' % (
                scrambled_name, trait.method_impls[name].cname(),
                scrambled_name)
        else:
            # abstract method, so cannot be NULL
            value = scrambled_name
        initializers.append('.%s = %s' % (name, value))
    for name in sorted(trait.vtable_params):
        initializers.append('.%s = %s' % (name, scramble_argname(name)))
    for name in sorted(trait.vtable_sessions):
        initializers.append('.%s = %s' % (name, scramble_argname(name)))
    for name in sorted(trait.vtable_hooks):
        initializers.append('.%s = %s' % (name, scramble_argname(name)))
    for name in sorted(trait.vtable_memoized_outs):
        initializers.append('.%s = %s' % (name, scramble_argname(name)))

    tinit_calls = []
    for parent in trait.direct_parents:
        args = []
        for name in tinit_args(parent):
            scrambled_name = scramble_argname(name)
            kind = parent.member_kind(name)
            if kind in {'parameter', 'session', 'hook', 'memoized_outs'}:
                args.append(scrambled_name)
            else:
                assert kind == 'method'
                args.append(method_tinit_arg(trait, parent,
                                             name, scrambled_name))
        tinit_calls.append("_tinit_%s(%s);\n" % (
            parent.name, ", ".join(['&_ret->' + cident(parent.name)]
                                   + args)))

    out('static void __attribute__((optimize("O0")))\n')
    out('_tinit_%s(struct _%s *_ret' % (trait.name,
                                        cident(trait.name)))
    inargs = tinit_args(trait)
    for name in inargs:
        scrambled_name = scramble_argname(name)
        vtable_trait = trait.ancestor_vtables.get(name, trait)
        member_kind = trait.member_kind(name)
        if member_kind == 'method':
            (_, inp, outp, throws, independent, _, _) \
                = vtable_trait.vtable_methods[name]
            mtype = vtable_trait.vtable_method_type(inp, outp, throws,
                                                    independent)
            out(', %s' % (mtype.declaration(scrambled_name),))
        elif member_kind == 'parameter':
            (site, typ) = vtable_trait.vtable_params[name]
            if isinstance(realtype(typ), TTraitList):
                out(f', _each_in_param_t {scrambled_name}')
            else:
                out(f', {TPtr(typ).declaration(scrambled_name)}')
        elif member_kind == 'session':
            out(', uint32 %s' % (scrambled_name)) # device struct offset
        elif member_kind == 'hook':
            out(', uint32 %s' % (scrambled_name)) # hook unique id
        else:
            assert member_kind == 'memoized_outs'
            memo_outs_struct = vtable_trait.vtable_memoized_outs[name]
            out(f', {TPtr(memo_outs_struct).declaration(scrambled_name)}')

    out(')\n{\n', postindent=1)
    if initializers:
        out('*_ret = (struct _%s){\n' % (cident(trait.name),),
            postindent=1)
        for initializer in initializers:
            out("%s,\n" % (initializer,))
        out("};\n", postindent=-1)
    for tinit_call in tinit_calls:
        out(tinit_call)
    out("}\n", preindent=-1)

def trait_trampoline_name(method, vtable_trait):
    return "%s__trampoline_from_%s" % (
        crep.cref_method(method), vtable_trait.name)

def flatten_object_subtree(node):
    '''return a list of all composite subobjects inside node'''
    return [node] + node.get_recursive_components(
        'event', 'port', 'implement', 'attribute', 'connect',
        'interface', 'bank', 'group', 'register', 'field', 'subdevice')

class ParamValue(ABC):
    indexed = False
    @staticmethod
    def indexvars(node):
        return (f'_i{i}' for i in range(node.dimensions))

class IndexedParamValue(ParamValue):
    indexed = True
    @abstractmethod
    def decl(self, varname): pass
    @abstractmethod
    def init(self, varname): pass
    @abstractmethod
    def tinit_arg(self, varname): pass

@dataclasses.dataclass
class SingleParamValue(ParamValue):
    # evaluates to the parameter value
    value: str
    ptype: DMLType
    def tinit_arg(self):
        if deep_const(self.ptype):
            k = TArray(self.ptype,
                       mkIntegerLiteral(logging.SimpleSite(
                           "<SingleParamValue>"), 1)).declaration("")
            size = f'sizeof({self.ptype.declaration("")})'
            return f'memcpy(malloc({size}), ({k}) {{ {self.value} }}, {size})'
        else:
            # Reasons for alignment explained along with test
            # in 1.4/structure/T_trait.dml
            decl = (f'static {self.ptype.declaration("_tmp")}'
                    ' __attribute__((aligned(2)))')
            return f'({{{decl}; _tmp = {self.value}; &_tmp; }})'

@dataclasses.dataclass
class SingleEachInParamValue(ParamValue):
    '''a parameter value representing a single 'in each', which is encoded
    into an _each_in_param_t. Either the 'in each' expression does not
    depend on an index variable, or it is the sentinel value that
    represents `each T in this` inside an array.
    '''
    base_idx : str
    num : int
    array_idx : str
    array_size : int
    def tinit_arg(self):
        return (f'(_each_in_param_t){{.base_idx = {self.base_idx}'
                f', .num = {self.num}, .array_idx = {self.array_idx}'
                f', .array_size = {self.array_size}}}')

@dataclasses.dataclass
class ArrayParamValue(IndexedParamValue):
    # evaluates to the value in one
    value : str
    ptype : DMLType
    node : objects.DMLObject
    def decl(self, var):
        array_type = self.ptype
        for d in reversed(self.node.dimsizes):
            array_type = TArray(array_type,
                                mkIntegerLiteral(self.node.site, d))
        return (f'{TPtr(array_type).declaration(var)} = malloc('
                f'sizeof(*{var}));\n')
    def init(self, var):
        param_indices = ''.join(f'[{i}]' for i in self.indexvars(self.node))
        if deep_const(self.ptype):
            # partially const values requires memcpy for
            # initialization
            k = (TArray(self.ptype, mkIntegerLiteral(self.node.site, 1))
                 .declaration(""))
            target = f'(void *)&(*{var}){param_indices}'
            source = f'({k}) {{ {self.value} }}'
            size = f'sizeof({self.ptype.declaration("")})'
            return f'memcpy({target}, {source}, {size});\n'
        else:
            return f'(*{var}){param_indices} = {self.value};\n'
    def tinit_arg(self, var):
        return f'(void *)((uintptr_t){var} + 1)'

@dataclasses.dataclass
class EachInArrayParamValue(IndexedParamValue):
    '''An indexed "each in" expressions other than "each X in this". This
    is represented as a pointer to an _each_in_t, marked by the
    sentinel value array_size=0. The "each X in this" case has another
    special representation, see the "not indexed" case.
    '''

    # evaluates to _each_in_t, depends on index variable
    value : str
    node : objects.DMLObject

    def decl(self, var):
        sizes = ''.join(f'[{sz}]' for sz in self.node.dimsizes)
        return f'_each_in_t (*{var}){sizes} = malloc(sizeof(*{var}));\n'
    def init(self, var):
        param_indices = ''.join(f'[{i}]' for i in self.indexvars(self.node))
        return f'(*{var}){param_indices} = {self.value};\n'
    def tinit_arg(self, var):
        return ('(_each_in_param_t){.array_size = 0,'
                f' .array = &(*{var}){"[0]" * self.node.dimensions}}}')

def init_trait_vtable(node, trait, param_overrides):
    method_overrides = node.traits.method_overrides
    # List of strings with the arguments passed to the _tinit_* function
    args = []
    # Sometimes, the vtable of an object array has one or more
    # arguments that must be initialized by evaluating an expression
    # once for each index, e.g. `param offset = i > 2 ? 10 - i : i`
    # requires an argument of type uint64[SIZE], where each array
    # element is initialized to the offset expression.  In this case,
    # we create a C block that declares one temporary variable for
    # each such parameter, followed by a for loop that initializes all
    # indices, followed by the _tinit_* call. param_decl is a list of
    # strings with declarations of array variables, and param_init is
    # a list of strings for initializing these inside the for loop.
    # For each argument, either there is no corresponding
    # param_decl/param_init, or both exist and `args` is a reference
    # to the declared array.
    param_decl = []
    param_init = []
    for name in tinit_args(trait):
        member_kind = trait.member_kind(name)
        if member_kind == 'method':
            args.append(
                trait_trampoline_name(
                    method_overrides[name], trait.vtable_trait(name))
                if name in method_overrides else "NULL")
        elif member_kind == 'parameter':
            override = param_overrides[name]
            var = f'_param_{name}'
            if override.indexed:
                param_decl.append(override.decl(var))
                param_init.append(override.init(var))
                args.append(override.tinit_arg(var))
            else:
                args.append(override.tinit_arg())
        elif member_kind == 'session':
            session_node = node.get_component(name)
            assert session_node.objtype in {'session', 'saved'}
            args.append('offsetof(%s, %s)' % (
                crep.structtype(dml.globals.device),
                crep.cref_session(
                    session_node,
                    (mkIntegerConstant(node.site, 0, False),)
                    * node.dimensions)))
        elif member_kind == 'hook':
            hook_node = node.get_component(name)
            assert hook_node.objtype == 'hook'
            args.append(str(hook_node.uniq))
        else:
            assert member_kind == 'memoized_outs'
            typ = trait.vtable_trait(name).vtable_memoized_outs[name]
            if node.dimensions:
                for d in reversed(node.dimsizes):
                    typ = TArray(typ, mkIntegerLiteral(node.site, d))
                arg = f'calloc(1, sizeof({typ.declaration("")}))'
            else:
                arg = f'({{static {typ.declaration("_tmp")}; &_tmp; }})'
            args.append(arg)
    # initialize vtable instance
    vtable_arg = f'&{node.traits.vtable_cname(trait)}'
    init_call = ('_tinit_%s(%s);\n'
         % (trait.name, ', '.join([vtable_arg] + args)))

    if param_decl:
        out('{\n', postindent=1)
        for decl in param_decl:
            out(decl)
        for (v, d) in zip(IndexedParamValue.indexvars(node), node.dimsizes):
            out(f'for (unsigned {v} = 0; {v} < {d}; ++{v}) {{\n',
                postindent=1)
        for init in param_init:
            out(init)
        for _ in range(node.dimensions):
            out('}\n', preindent=-1)
        out(init_call)
        out('}\n', preindent=-1)
    else:
        assert not param_init
        out(init_call)

def generate_init_trait_vtables(node, param_values):
    '''Return a list of pairs (dimsizes, 'call();\n')
    meaning that call() is to be invoked with variables (_i0, _i1, ..) given by
    all possible indices within dimsizes'''
    generate_vtable_instances(node)
    for trait in traits.Trait.referenced:
        generate_tinit(trait)
    all_nodes = list(flatten_object_subtree(node))
    for subnode in all_nodes:
        generate_trait_trampolines(subnode)
    # Hack: Split up trait initialization in small functions, to avoid
    # performance problems with `gcc -fvar-tracking`. This is
    # required because of a GCC bug that is fixed in GCC 12:
    # https://gcc.gnu.org/bugzilla/show_bug.cgi?id=104468
    chunks = [all_nodes[i:i+20] for i in range(0, len(all_nodes), 20)]
    for (i, chunk) in enumerate(chunks):
        out('static void __attribute__((optimize("O0")))'
            f' _initialize_traits{i}(void)\n')
        out('{\n', postindent=1)
        for subnode in chunk:
            if subnode.traits:
                param_overrides = param_values[subnode]
                for trait in subnode.traits.referenced:
                    init_trait_vtable(subnode, trait, param_overrides)
        out('}\n', preindent=-1)

    start_function_definition('void __attribute__((optimize("O0")))'
                              ' _initialize_traits(void)')
    out('{\n', postindent=1)
    for i in range(len(chunks)):
        out(f'_initialize_traits{i}();\n')
    out('}\n', preindent=-1)
    splitting_point()

def trait_param_value(node, param_type_site, param_type):
    is_sequence = isinstance(realtype(param_type), TTraitList)
    try:
        try:
            expr = node.get_expr(static_indices(node))
            if isinstance(expr, NonValue):
                raise expr.exc()
            expr = source_for_assignment(expr.site, param_type, expr)
        except EIDXVAR:
            indices = tuple(mkLit(node.site, v, TInt(32, False))
                            for v in IndexedParamValue.indexvars(node))
            expr = node.get_expr(indices)
            if isinstance(expr, NonValue):
                raise expr.exc()
            expr = source_for_assignment(expr.site, param_type, expr)
            if is_sequence:
                if (isinstance(expr, EachIn)
                    and expr.node is node.parent
                    and expr.indices == indices[:node.dimensions]):
                    (subobjs, base_idx, num, array_idx,
                     array_size) = expr.each_in_t_members()
                    expr.mark_referenced(subobjs)
                    return SingleEachInParamValue(base_idx, num, '0xffffffff',
                                                  array_size)
                else:
                    return EachInArrayParamValue(expr.read(), node)
            else:
                return ArrayParamValue(expr.read(), param_type, node)
        else:
            if is_sequence:
                assert isinstance(expr, EachIn), repr(expr)
                (subobjs, base_idx, num, array_idx,
                 array_size) = expr.each_in_t_members()
                expr.mark_referenced(subobjs)
                return SingleEachInParamValue(base_idx, num, array_idx,
                                              array_size)
            else:
                return SingleParamValue(expr.read(), param_type)
    except DMLError as e:
        report(e)
        return SingleParamValue("NULL", param_type)

def resolve_trait_param_values(node):
    '''Generate code for parameter initialization of all traits
    implemented by a node, as a dict name -> str (as C code)'''
    traits = node.traits
    return {
        name: trait_param_value(
            pnode, *traits.ancestor_vtables[name].vtable_params[name])
        for (name, pnode) in list(traits.param_nodes.items())}

def generate_trait_trampoline(method, vtable_trait):
    implicit_inargs = vtable_trait.implicit_args()
    explicit_inargs = c_inargs(list(method.inp), method.outp, method.throws)
    inparams = ", ".join(
        t.declaration(n)
        for (n, t) in (crep.maybe_dev_arg(method.independent) + implicit_inargs
                       + explicit_inargs))
    rettype = c_rettype(method.outp, method.throws)

    # guaranteed to exist; created by ObjTraits.mark_referenced
    func = method.funcs[None]
    out('static %s\n{\n' % (
        rettype.declaration('%s(%s)' % (
            trait_trampoline_name(method, vtable_trait), inparams))),
        postindent=1)
    with (crep.DeviceInstanceContext() if not method.independent
          else nullcontext()):
        [(tname, ttype)] = implicit_inargs
        site = method.site
        obj = method.parent
        if obj.dimensions:
            out(f'uint32 _flat_index = {tname}.id.encoded_index;\n')
        indices = [
            mkLit(site, '((_flat_index / %d) %% %d)' % (
                reduce(operator.mul, obj.dimsizes[dim + 1:], 1),
                obj.dimsizes[dim]), TInt(32, False))
            for dim in range(obj.dimensions)]
        args = [mkLit(site, n, t) for (n, t) in explicit_inargs]
        call_expr = mkcall_method(site, func, indices)(args)
    if not rettype.void:
        out('return ')
    out('%s;\n' % call_expr.read())
    out('}\n', preindent=-1)

def generate_trait_trampolines(node):
    '''Generate trampolines for all used trait methods overridden in an
    object, excluding subobjects'''
    overridden = Set(node.traits.method_overrides)
    for trait in node.traits.referenced:
        for name in overridden.intersection(trait.vtable_methods):
            generate_trait_trampoline(
                node.traits.method_overrides[name], trait)
        for name in overridden.intersection(trait.ancestor_vtables):
            generate_trait_trampoline(
                node.traits.method_overrides[name],
                trait.ancestor_vtables[name])

def generate_vtable_instances(devnode):
    for subnode in flatten_object_subtree(devnode):
        if subnode.traits:
            for trait in subnode.traits.referenced:
                add_variable_declaration(
                    'struct _%s %s'
                     % (cident(trait.name), subnode.traits.vtable_cname(trait)))

def calculate_saved_userdata(node, dimsizes, attr_name, sym_spec = None):
    if node.objtype == 'method':
        (sym, local_name) = sym_spec
        attr_name += "_" + local_name
        decl_site = sym.site
        # Reference the first index of this
        # static variable
        var_ref = mkStaticVariable(node.site, sym)
        for _ in range(node.dimensions):
            var_ref = mkIndex(decl_site, var_ref,
                              mkIntegerConstant(decl_site, 0, False))
        device_member = sym.value
    elif node.objtype == 'saved':
        assert sym_spec is None
        decl_site = node.site
        # Reference the first struct member of this
        # saved variable
        loop_vars = (mkIntegerConstant(decl_site, 0, False),) * node.dimensions
        var_ref = mkNodeRef(node.site, node, loop_vars)
        device_member = crep.cref_session(node, loop_vars)
    else:
        assert False

    relative_offset = "offsetof(%s, %s)" % (
        crep.structtype(dml.globals.device),
        device_member)

    c_type = var_ref.ctype()
    attr_type = serialize.map_dmltype_to_attrtype(decl_site, c_type)

    for dimension in reversed(dimsizes):
        attr_type = "[%s{%d}]" % (attr_type, dimension)
    dimension_strides = []
    stride = f'sizeof({c_type.declaration("")})'
    for dimsize in reversed(node.dimsizes):
        dimension_strides.append(stride)
        stride += f' * {dimsize}'
    dimension_strides = tuple(reversed(dimension_strides))

    serialize_name = serialize.lookup_serialize(c_type)
    deserialize_name = serialize.lookup_deserialize(c_type)

    # dimension_strides include port dimensions, but dimsizes does not
    return (decl_site, attr_name, attr_type, relative_offset, dimsizes,
            dimension_strides, deserialize_name, serialize_name)

def generate_saved_userdata(node, dimensions, prefix):
    # Generate attributes for all subcomponents.
    for child in sorted(node.get_components(),
                        key=lambda o: o.name or ''):
        if child.objtype == 'saved':
            try:
                yield calculate_saved_userdata(child, dimensions,
                                               prefix + child.ident)
            except ESERIALIZE as e:
                report(e)
        elif child.objtype == 'method':
            # if the method is not generated, the variable is dead and
            # we dont have to bother
            if child in saved_method_variables:
                for sym_spec in saved_method_variables[child]:
                    try:
                        yield calculate_saved_userdata(
                            child, dimensions, prefix + child.ident,
                            sym_spec)
                    except ESERIALIZE as e:
                        report(e)
        elif (isinstance(child, objects.CompositeObject)
              and child.objtype not in {'bank', 'port', 'subdevice'}):
            if child.ident is None:
                assert (dml.globals.dml_version == (1, 2)
                        and child.objtype == 'field')
                if (child.get_recursive_components('saved')
                    or any(meth in saved_method_variables
                           for meth in
                           child.get_recursive_components('method'))):
                    raise ICE(child.site,
                              'implicit fields may not contain saved variables'
                               + ' or methods with saved variables')
            else:
                yield from generate_saved_userdata(
                    child, dimensions + child.arraylens(),
                    prefix + child.name_anonymized + "_")

def register_saved_attributes(initcode, dev):
    ports = [dev]
    ports.extend(dev.get_recursive_components('bank', 'port', 'subdevice'))
    for node in ports:
        saved_userdata = list(generate_saved_userdata(node, (), ''))
        if not saved_userdata:
            continue

        if not node.name:
            # anonymous bank
            assert dml.globals.dml_version == (1, 2) and node.objtype == "bank"
            raise ICE(node.site,
                      'anonymous banks may not (indirectly) contain saved '
                      + 'variables')

        if node is dev:
            cls = 'class'
            getter = '_get_saved_variable'
            setter = '_set_saved_variable'
        else:
            cls = port_class_ident(node)
            getter = '_get_port_saved_variable'
            setter = '_set_port_saved_variable'

        initcode.out('{\n', postindent=1)
        initcode.out("const struct {\n")
        initcode.out("  const char *name;\n")
        initcode.out("  const char *type;\n")
        initcode.out("} saved_attrinfo[%d] = {\n" % (len(saved_userdata),),
                     postindent=1)
        for (site, attr_name, attr_type, relative_offset, dimsizes,
             dimension_strides, deserialize_name,
             serialize_name) in saved_userdata:
            register_attribute(
                site, None if node is dml.globals.device else node, attr_name)
            initcode.out('{"%s", "%s"},\n' % (attr_name, attr_type))
        initcode.out('};\n', preindent=-1)
        initcode.out('static const _saved_userdata_t saved_userdata[%d] = {\n'
                     % (len(saved_userdata),), postindent=1)
        for (site, attr_name, attr_type, relative_offset, dimsizes,
             dimension_strides, deserialize_name,
             serialize_name) in saved_userdata:
            initcode.out('{%s, %d, %s, %s, %s, %s},\n' % (
                relative_offset, len(dimsizes),
                tuple_as_uint32_array(site, dimsizes,
                                      'dims_' + attr_name).read(),
                tuple_as_uint32_array(site, dimension_strides,
                                      'strides_' + attr_name).read(),
                deserialize_name,
                serialize_name))
        initcode.out('};\n', preindent = -1)

        initcode.out("for (unsigned int idx = 0;"
                     + " idx < %d;" % (len(saved_userdata),)
                     + " ++idx) {\n", postindent = 1)
        # Reasonably, all saved variables are optional, inside the model
        # they will be initialized to 0-bit values by DML
        initcode.out(f'SIM_register_attribute_with_user_data({cls}, '
                     + 'saved_attrinfo[idx].name,\n', postindent = 1)
        initcode.out(f'{getter}, (lang_void *)&saved_userdata[idx],\n')
        initcode.out(f'{setter}, (lang_void *)&saved_userdata[idx],\n')
        initcode.out('Sim_Attr_Optional | Sim_Attr_Internal,\n')
        initcode.out('saved_attrinfo[idx].type, "saved variable");\n',
                     postindent = -1)
        initcode.out('}\n', preindent=-1)
        initcode.out('}\n', preindent=-1)

def generate_hook_attr_names(node, prefix):
    # Generate attributes for all subcomponents.
    for child in sorted(node.get_components(),
                        key=lambda o: o.name or ''):
        if child.objtype == 'hook':
            yield (child, prefix + child.ident)
        elif (isinstance(child, objects.CompositeObject)
              and child.objtype not in {'bank', 'port', 'subdevice'}):

            if child.ident is None:
                assert (dml.globals.dml_version == (1, 2)
                        and child.objtype == 'field')
                if child.get_recursive_components('hook'):
                    raise ICE(child.site,
                              'implicit fields may not contain hooks')
            else:
                yield from generate_hook_attr_names(
                    child, prefix + child.name_anonymized + "_")

def register_hook_attributes(initcode, dev):
    ports = [dev]
    ports.extend(dev.get_recursive_components('bank', 'port', 'subdevice'))
    for node in ports:
        hook_attr_names = list(generate_hook_attr_names(node, ''))
        if not hook_attr_names:
            continue

        if not node.name:
            # anonymous bank
            assert dml.globals.dml_version == (1, 2) and node.objtype == "bank"
            raise ICE(node.site,
                      'anonymous banks may not (indirectly) contain hooks')

        if node is dev:
            cls = 'class'
            getter = '_DML_get_hook_attr'
            setter = '_DML_set_hook_attr'
        else:
            cls = port_class_ident(node)
            getter = '_DML_get_port_hook_attr'
            setter = '_DML_set_port_hook_attr'

        initcode.out('{\n', postindent=1)
        initcode.out('const char *hook_attr_names[%d] = {\n'
                     % (len(hook_attr_names),), postindent=1)
        for (hook, attr_name) in hook_attr_names:
            register_attribute(
                hook.site, None if node is dml.globals.device else node,
                attr_name)
            initcode.out(f'"{attr_name}",\n')
        initcode.out('};\n', preindent=-1)
        initcode.out(f'const uint32 hook_ids[{len(hook_attr_names)}] = {{\n',
                     postindent=1)
        for (hook, _) in hook_attr_names:
            initcode.out(f'{hook.uniq},\n')
        initcode.out('};\n', preindent=-1)
        initcode.out('for (uint32 idx = 0; idx < %d; ++idx) {\n'
                     % (len(hook_attr_names),) , postindent = 1)
        initcode.out(
            f'_DML_register_hook_attribute({cls}, hook_attr_names[idx], '
            + f'{getter}, {setter}, &_hook_aux_infos[hook_ids[idx] - 1], '
            + f'_hook_id_infos[hook_ids[idx] - 1], {node.dimensions});\n')
        initcode.out('}\n', preindent=-1)
        initcode.out('}\n', preindent=-1)


def generate_hook_attribute_funs():
    if not objects.Device.hooks:
        return

    start_function_definition(
        'attr_value_t _DML_get_hook_attr('
        + 'conf_object_t *obj, lang_void *hook_access)')
    body = '''{
    _dml_hook_aux_info_t *acc = (_dml_hook_aux_info_t *) hook_access;
    _dml_hook_get_set_aux_data_t data = {
        &_typeseq_after_on_hook_hts[acc->typeseq_uniq],
        &_id_info_ht, _id_infos };
    _id_info_t info = _hook_id_infos[acc->hook_id - 1];
    uint32 dimension_strides[info.dimensions];
    _DML_init_dimension_strides_from_dimsizes(
        dimension_strides, info.dimsizes, info.dimensions,
        sizeof(_dml_hook_t));

    attr_value_t val = _get_device_member((char *)obj + acc->device_offset,
                                          info.dimsizes,
                                          dimension_strides,
                                          info.dimensions,
                                          _DML_get_single_hook_attr,
                                          (uintptr_t) &data);
    return val;
}\n'''
    out(body)

    start_function_definition(
        'set_error_t _DML_set_hook_attr('
        + 'conf_object_t *obj, attr_value_t *val, lang_void *hook_access)')
    body = '''{
    _dml_hook_aux_info_t *acc = (_dml_hook_aux_info_t *) hook_access;
    _dml_hook_get_set_aux_data_t data = {
        &_typeseq_after_on_hook_hts[acc->typeseq_uniq],
        &_id_info_ht, _id_infos };
    _id_info_t info = _hook_id_infos[acc->hook_id - 1];
    uint32 dimension_strides[info.dimensions];
    _DML_init_dimension_strides_from_dimsizes(
        dimension_strides, info.dimsizes, info.dimensions,
        sizeof(_dml_hook_t));
    set_error_t error = _set_device_member(*val,
                                           (char *)obj + acc->device_offset,
                                           info.dimsizes,
                                           dimension_strides,
                                           info.dimensions,
                                           _DML_set_single_hook_attr,
                                           (uintptr_t) &data);
    return error;
}\n'''
    out(body)

    start_function_definition(
        'attr_value_t _DML_get_port_hook_attr('
        + 'conf_object_t *_portobj, lang_void *hook_access)')
    body = '''{
    _port_object_t *portobj = (_port_object_t *)_portobj;
    conf_object_t *obj = portobj->dev;
    _dml_hook_aux_info_t *acc = (_dml_hook_aux_info_t *) hook_access;
    _dml_hook_get_set_aux_data_t data = {
        &_typeseq_after_on_hook_hts[acc->typeseq_uniq],
        &_id_info_ht, _id_infos };
    _id_info_t info = _hook_id_infos[acc->hook_id - 1];
    uint32 dimension_strides[info.dimensions];
    _DML_init_dimension_strides_from_dimsizes(
        dimension_strides, info.dimsizes, info.dimensions,
        sizeof(_dml_hook_t));
    char *ptr = (char *)obj + acc->device_offset;
    for (int i = 0; i < portobj->ndims; ++i) {
            ptr += portobj->indices[i] * dimension_strides[i];
    }


    attr_value_t val = _get_device_member(ptr,
                                          info.dimsizes + portobj->ndims,
                                          dimension_strides + portobj->ndims,
                                          info.dimensions - portobj->ndims,
                                          _DML_get_single_hook_attr,
                                          (uintptr_t) &data);
    return val;
}\n'''
    out(body)

    start_function_definition(
        'set_error_t _DML_set_port_hook_attr('
        + 'conf_object_t *_portobj, attr_value_t *val, '
        + 'lang_void *hook_access)')
    body = '''{
    _port_object_t *portobj = (_port_object_t *)_portobj;
    conf_object_t *obj = portobj->dev;
    _dml_hook_aux_info_t *acc = (_dml_hook_aux_info_t *) hook_access;
    _dml_hook_get_set_aux_data_t data = {
        &_typeseq_after_on_hook_hts[acc->typeseq_uniq],
        &_id_info_ht, _id_infos };
    _id_info_t info = _hook_id_infos[acc->hook_id - 1];
    uint32 dimension_strides[info.dimensions];
    _DML_init_dimension_strides_from_dimsizes(
        dimension_strides, info.dimsizes, info.dimensions,
        sizeof(_dml_hook_t));
    char *ptr = (char *)obj + acc->device_offset;
    for (int i = 0; i < portobj->ndims; ++i) {
            ptr += portobj->indices[i] * dimension_strides[i];
    }
    set_error_t error = _set_device_member(*val,
                                           ptr,
                                           info.dimsizes + portobj->ndims,
                                           dimension_strides + portobj->ndims,
                                           info.dimensions - portobj->ndims,
                                           _DML_set_single_hook_attr,
                                           (uintptr_t) &data);
    return error;
}\n'''
    out(body)

def generate_startup_call_loops(startup_methods):
    by_dims = {}
    for (method_data_tuple, dims) in startup_methods:
        by_dims.setdefault(dims, []).append(method_data_tuple)
    for dims in by_dims:
        for i in range(len(dims)):
            idxvar = '_i%d' % (i,)
            out('for (uint32 %s = 0; %s < %d; %s++) {\n'
                % (idxvar, idxvar, dims[i], idxvar), postindent=1)

        for (site, gen_call, data) in by_dims[dims]:
            idxvars = ['_i%d' % (i,) for i in range(len(dims))]
            gen_call(data, idxvars)

        for i in range(len(dims)):
            out('}\n', preindent=-1)

def generate_startup_trait_calls(data, idxvars):
    (node, traits) = data
    site = node.site

    indices = tuple(mkLit(site, idx, TInt(32, False)) for idx in idxvars)

    out('{\n', postindent=1)
    out('_traitref_t _tref;\n')
    for (trait, trait_methods) in traits.items():
        ref = ObjTraitRef(site, node, trait, indices)
        out(f'_tref = {ref.read()};\n')
        for method in trait_methods:
            outargs = [mkLit(method.site,
                             ('*((%s) {0})'
                              % ((TArray(t, mkIntegerLiteral(method.site, 1))
                                  .declaration('')),)),
                             t)
                       for (_, t) in method.outp]

            method_ref = TraitMethodDirect(
                method.site, mkLit(method.site, '_tref', TTrait(trait)), method)
            with IgnoreFailure(site):
                codegen_call_traitmethod(method.site, method_ref, [],
                                         outargs).toc()
    out('}\n', preindent=-1)

def generate_startup_regular_call(method, idxvars):
    site = method.site
    indices = tuple(mkLit(site, idx, TInt(32, False)) for idx in idxvars)
    outargs = [mkLit(site,
                     ('*((%s) {0})'
                      % ((TArray(t, mkIntegerLiteral(site, 1))
                          .declaration('')),)),
                     t)
               for (_, t) in method.outp]
    # startup memoized methods can throw, which is ignored during startup.
    # Memoization of the throw then allows for the user to check whether
    # or not the method did throw during startup by calling the method
    # again.
    with IgnoreFailure(site):
        codegen_call(method.site, method, indices, [], outargs).toc()

def generate_startup_calls_entry_function(devnode):
    start_function_definition('void _startup_calls(void)')
    out('{\n', postindent=1)

    trait_startups = []
    trait_memoized_startups = []
    for subnode in flatten_object_subtree(devnode):
        if subnode.traits:
            startups = {}
            startup_memoizeds = {}
            for trait in subnode.traits.ancestors:
                for trait_method in trait.method_impls.values():
                    if trait_method.startup:
                        target_dict = (
                            startup_memoizeds if trait_method.memoized
                            else startups)
                        target_dict.setdefault(trait, []).append(trait_method)
                        subnode.traits.mark_referenced(trait)

            if startups:
                trait_startups.append((subnode, startups))

            if startup_memoizeds:
                trait_memoized_startups.append((subnode, startup_memoizeds))

    all_startups = \
        ([((method.site, generate_startup_regular_call, method),
           method.dimsizes)
          for method in structure.startups]
         + [((node.site, generate_startup_trait_calls, (node, startups)),
             node.dimsizes)
            for (node, startups) in trait_startups])
    all_memoized_startups = \
        ([((method.site, generate_startup_regular_call, method),
           method.dimsizes)
          for method in structure.memoized_startups]
         + [((node.site, generate_startup_trait_calls, (node, startups)),
             node.dimsizes)
            for (node, startups) in trait_memoized_startups])
    for startups in (all_startups, all_memoized_startups):
        generate_startup_call_loops(startups)
    out('}\n', preindent=-1)


class MultiFileOutput(FileOutput):
    def __init__(self, stem, header):
        FileOutput.__init__(self, stem + '-0.c')
        self.stem = stem
        self.header = header
        self.index = 0
        self.write(self.header)
    def advance(self):
        '''Finish this C file and start writing the next one'''
        self.close()
        self.commit()
        self.index += 1
        new_filename = self.stem + '-%d.c' % (self.index,)
        self.set_file(open(new_filename + '.tmp', 'w'), new_filename)
        self.out(self.header)

def assert_global_c_scope():
    assert output.current().indent == 0 or logging.failure

def add_variable_declaration(decl, init=None):
    assert_global_c_scope()
    prototypes.append(decl)
    if init:
        linkage = '' if c_split_threshold else 'static '
        out('%s%s UNUSED = %s;\n' % (linkage, decl, init))
    elif c_split_threshold:
        out(decl + ' UNUSED;\n')

def start_function_definition(decl):
    assert_global_c_scope()
    linkage = '' if c_split_threshold else 'static '
    out("%s%s\n" % (linkage, decl))
    prototypes.append(decl)

def splitting_point():
    '''Called after generating the definition of a function or
    variable. When --split-c-file is used, this is a place where the
    .c file can be split.'''
    assert output.current() == c_file
    assert_global_c_scope()
    if c_split_threshold and c_file.tell() > c_split_threshold:
        c_file.advance()

c_file = None
def generate_cfile(device, footers,
                   filename_prefix, hfilename, protofilename, source_files,
                   full_module):
    global c_file

    sym = 'SIMICS_%s_API' % ('4_8' if dml.globals.api_version == compat.api_4_8
                             else dml.globals.api_version.str)
    api_define = '#ifndef %s\n#define %s\n#endif\n' % (
        sym, sym)

    c_top = '\n'.join([
        '/*',
        ' * Generated by dmlc, do not edit!',
        ' *',
        ' * Source files:',
        '\n'.join(f' *   {Path(f).resolve()}' for f in source_files),
        ' */',
        '',
        api_define,
        '#include "%s"' % (os.path.basename(hfilename),),
        '#include "%s"' % (os.path.basename(protofilename),),
        ''])

    if c_split_threshold:
        c_file = MultiFileOutput(filename_prefix, c_top)
    else:
        c_file = FileOutput(filename_prefix + '.c')
        c_file.out(c_top)
    with c_file, device.use_for_codegen():
        generate_cfile_body(device, footers, full_module, filename_prefix)
    c_file.close()
    if not logging.failure:
        c_file.commit()
    c_file = None

def generate_cfile_body(device, footers, full_module, filename_prefix):

    # An output buffer for code that should be included in the init function
    init_code = StrOutput()
    init_code.out('', postindent=1)

    # The marker must be generated before any lines annotated with #line
    if dml.globals.debuggable:
        generate_marker(device)
    if dml.globals.dml_version == (1, 2):
        generate_register_tables(device)
    with crep.DeviceInstanceContext():
        generate_port_classes(init_code, device)
        generate_attributes(init_code, device)
        generate_subobj_connects(init_code, device)
        generate_implements(init_code, device)
    if dml.globals.dml_version != (1, 2) or dml.globals.state_change_dml12:
        generate_state_notify(device)
        generate_state_existence_callback(device)

    generate_alloc(device)
    generate_initialize(device)
    generate_finalize(device)
    generate_objects_finalized(device)
    generate_pre_delete(device)
    generate_deinit(device)
    generate_dealloc(device)
    generate_events(device)
    generate_identity_data_decls()
    generate_object_vtables_array()
    if not (dml.globals.dml_version == (1, 2)
            and compat.shared_logs_on_device in dml.globals.enabled_compat):
        generate_port_object_assocs_array()
    generate_class_var_decl()
    generate_startup_calls_entry_function(device)
    generate_init_data_objs(device)
    if dml.globals.dml_version == (1, 2):
        generate_reset(device, 'hard')
        generate_reset(device, 'soft')

    # These parameter values are output into static context, so make sure
    # the expressions do not use _dev
    with crep.TypedParamContext():
        trait_param_values = {
            node: resolve_trait_param_values(node)
            for node in flatten_object_subtree(device)
            if node.traits}


    gather_size_statistics = os.environ.get('DMLC_GATHER_SIZE_STATISTICS', '')
    size_statistics = {}

    for t in list(dml.globals.traits.values()):
        for m in list(t.method_impls.values()):
            if gather_size_statistics:
                ctx = StrOutput(filename=output.current().filename,
                                lineno=output.current().lineno)
                with ctx:
                    generate_trait_method(m)
                size_statistics[m.site.loc()] = [len(ctx.buf)]
                out(ctx.buf)
            else:
                generate_trait_method(m)
    # Note: methods may be added to method_queue while doing this,
    # so don't try to be too smart
    generated_funcs = set()
    while method_queue:
        func = method_queue.pop()
        if func in generated_funcs:
            continue
        generated_funcs.add(func)
        code = codegen_method_func(func)

        specializations = [(n, 'undefined' if undefined(v) else v.value)
                           for (n, v) in func.inp
                           if isinstance(v, Expression)]

        if gather_size_statistics:
            ctx = StrOutput(lineno=output.current().lineno,
                            filename=output.current().filename)
        else:
            ctx = nullcontext()
        with ErrorContext(func.method), ctx:
            if specializations:
                out('/* %s\n' % func.get_name())
                for (n, v) in specializations:
                    out('     %s = %s\n' % (n, v))
                out('*/\n')
            else:
                out('/* %s */\n' % func.get_name())

            start_function_definition(func.prototype)
            with allow_linemarks():
                site_linemark(func.method.astcode.site)
                out('{\n', postindent=1)
                try:
                    code.toc_inline()
                except DMLError as e:
                    report(e)
                    # Any errors should have been caught during
                    # codegen_method, when all Expression and Statement
                    # objects are instantiated.
                    raise ICE(e.site, 'error during late compile stage')
                site_linemark(func.method.rbrace_site)
                out('}\n', preindent=-1)
            out('\n')
        if gather_size_statistics:
            size_statistics.setdefault(func.method.site.loc(), []).append(
                len(ctx.buf))
            out(ctx.buf)
        splitting_point()


    if gather_size_statistics:
        Path(filename_prefix + "-size-stats.json").write_text(json.dumps(
            sorted(((sum(sizes), len(sizes), loc)
                    for (loc, sizes) in size_statistics.items()),
                   reverse=True),
            indent=2))

    generate_hook_auxiliary_info_array()

    with crep.DeviceInstanceContext():
        register_saved_attributes(init_code, device)
        register_hook_attributes(init_code, device)
        generate_init(device, init_code, filename_prefix)

    generate_trait_deserialization_hashtables(device)
    generate_init_trait_vtables(device, trait_param_values)
    generate_each_in_tables()
    generate_after_on_hooks_artifacts(device)
    generate_immediate_after_callbacks(device)
    generate_hook_attribute_funs()
    generate_simple_events(device)
    generate_simple_event_only_domains_funs()
    generate_simple_events_control_methods(device)
    generate_init_identity_hashtable()
    generate_register_events(device)
    generate_init_port_objs(device)
    generate_init_static_vars(device)
    # generate_serialize must take place after register_saved_attributes
    # and generate_simple_events
    generate_serialize(device)

    for func in statically_exported_methods:
        generate_static_trampoline(func)

    for (name, (func, export_site)) in list(exported_methods.items()):
        if export_site.dml_version() == (1, 2):
            generate_extern_trampoline_dml12(name, func)
        else:
            generate_extern_trampoline(name, func)

    generate_index_enumerations()
    generate_tuple_table()

    with allow_linemarks():
        for c in footers:
            c.toc()
            out('\n')

    if full_module:
        # caught as error earlier on
        assert dml.globals.api_version == compat.api_4_8
        out('\n')
        out('EXPORTED void\n')
        out('init_local(void)\n')
        out('{\n', postindent = 1)
        out('%s();\n' % (init_function_name(device, filename_prefix),))
        out('}\n', preindent = -1)

    if method_queue:
        # this could e.g. be triggered if _cancel_all in
        # dml-builtins.dml would call another method
        raise ICE(None, 'late additions to method_queue')

    if dml.globals.dml_version == (1, 2):
        for error in EBADFAIL_dml12.all_errors():
            report(error)

    if logging.show_porting:
        with output.NoOutput():
            for method in PWUNUSED.typed_methods:
                if method.site.filename().endswith('dml-builtins.dml'):
                    # don't waste time on generating methods from dml-builtins
                    continue
                if method.site.dml_version() != (1, 2):
                    # don't waste time on methods that are already converted
                    continue
                # run code generation for all dead methods
                func = method_instance(method)
                if func not in generated_funcs:
                    with logging.suppress_errors():
                        codegen_method_func(func).toc_inline()

        for (site, method) in list(PWUNUSED.inline_methods.items()):
            if site in PWUNUSED.inlined_methods:
                report(PNO_WUNUSED(method.site, 'method', method.logname()))
            else:
                report(PWUNUSED(method.site, 'method', method.logname()))

def tmprename(base):
    try:
        os.remove(base)
    except OSError:
        pass
    os.rename(base+'.tmp', base)

def generate(device, headers, footers, prefix, source_files, full_module):
    global structfilename

    structfilename = prefix + "-struct.h"
    hfilename = prefix + ".h"
    protofilename = prefix + "-protos.c"

    generate_cfile(device, footers, prefix, hfilename,
                   protofilename, source_files, full_module)

    outfile = FileOutput(structfilename)
    with outfile:
        generate_structfile(device, structfilename, prefix)
    outfile.close()
    if not logging.failure:
        outfile.commit()

    outfile = FileOutput(hfilename)
    with outfile:
        generate_hfile(device, headers, hfilename)
    outfile.close()
    if not logging.failure:
        outfile.commit()

    outfile = FileOutput(protofilename)
    with outfile:
        generate_protofile(device)
    outfile.close()
    if not logging.failure:
        outfile.commit()
