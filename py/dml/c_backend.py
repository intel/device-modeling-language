# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import sys, os
import itertools
import operator
import re
from functools import reduce

from . import objects, logging, crep, output, ctree, serialize
from . import traits
import dml.globals
from .logging import *
from .messages import *
from .output import *
from .ctree import *
from .expr import *
from .expr_util import *
from .symtab import *
from .codegen import *
from .types import *

prototypes = []
c_split_threshold = None

log_object_t = TNamed("log_object_t")
conf_object_t = TNamed("conf_object_t")
attr_value_t = TNamed('attr_value_t')

structfilename = None

def get_attr_flags(obj):
    conf = param_str(obj, 'configuration')
    persist = param_bool(obj, 'persistent')
    internal = (param_bool(obj, 'internal', fallback=True)
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

def get_attr_name(prefix, node):
    if node.objtype == 'register' and node.is_confidential():
        return get_anonymized_name(node)
    else:
        return prefix + node.name

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
        doc = param_str(node, 'documentation', "")
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
        for arraylen in reversed(node.arraylens()):
            typ = TArray(typ, mkIntegerLiteral(node.site, arraylen))
        return typ

    def composite_ctype(node, unfiltered_members, label=None):
        '''Helper. Unfiltered_members is a list of (name, type) pairs, where
        type=None entries are ignored.'''
        members = [(name, typ) for (name, typ) in unfiltered_members if typ]
        if not members:
            return None
        if not label:
            # mangle names so x_y.z and x.y_z give different idents
            label = '__devstruct_' + '_'.join("%d%s" % (s.count('_'), s)
                                    for s in crep.ancestor_cnames(node))
        structtype = TStruct(members, label)
        structtype.print_struct_definition()
        return arraywrap(node, structtype)

    if node.objtype == 'device':
        members = [("obj", conf_object_t)]
        for v in node.staticvars:
            members.append((v.value, v.type))
        if node.use_qname_cache:
            members.append(("_qname_cache", TNamed("dml_qname_cache_t")))
        return composite_ctype(node,
                               members + [(crep.cname(sub), print_device_substruct(sub))
                                          for sub in node.get_components()],
                               label=crep.cname(node))

    elif ((node.objtype == 'session' or node.objtype == 'saved')
          or (dml.globals.dml_version == (1, 2)
              and node.objtype == 'interface')):
        return crep.node_storage_type(node, node.site)

    elif (node.objtype in ('register', 'field')
          and dml.globals.dml_version == (1, 2)):
        allocate = param_bool(node, 'allocate', fallback=True)
        if node.simple_storage:
            return (arraywrap(node, crep.node_storage_type(node))
                    if allocate else None)
        else:
            @list
            @apply
            def members():
                if allocate:
                    yield ('__DMLfield', crep.node_storage_type(node))
                for sub in node.get_components():
                    if not sub.ident:
                        # implicit field: Don't add anything, storage
                        # is inherited from the parent register
                        assert sub.objtype == 'field'
                        continue
                    yield (crep.cname(sub), print_device_substruct(sub))
            return composite_ctype(node, members)

    elif node.objtype in ('bank', 'port'):
        if node.name is None:
            assert dml.globals.dml_version == (1, 2)
            obj = []
        else:
            # really a _port_object_t* rather than conf_object_t*, but
            # there is no DML type for the former
            obj = [("_obj", TPtr(conf_object_t))]
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
    elif node.objtype in ('parameter', 'method'):
        return None

    else:
        raise Exception("Unknown group node " + repr(node))

def reset_line_directive():
    if dml.globals.linemarks:
        o = output.current()
        out('#line %d "%s"\n' % (o.lineno + 1, quote_filename(o.filename)))

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
                                ", ".join(["conf_object_t *_obj"]
                                          + [t.declaration(n)
                                             for (n, t) in func.cparams[1:]]))))
    emit_guard_end(filename)

def generate_hfile(device, headers, filename):
    out("/* Generated by dmlc, do not edit! */\n\n")
    emit_guard_start(filename)
    out('#define DML_PREFIX(x) '+crep.cname(device)+'_##x\n\n')

    for c in headers:
        c.toc()
        out('\n')

    reset_line_directive()
    out('\n')

    out('#include "'+structfilename+'"\n\n')

    for name in dml.globals.traits:
        out('typedef struct _%s *%s;\n' % (cident(name), cident(name)))

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

    for (name, t) in list(dml.globals.traits.items()):
        t.type().print_struct_declaration()

    print_device_substruct(device)

    if dml.globals.log_groups:
        out('typedef enum %s_log_group {\n' % crep.cname(device),
            postindent = 1)
        i = 1
        for g in dml.globals.log_groups:
            out('%s = %d,\n' % (crep.cloggroup(g), i))
            i <<= 1
        out('} %s_log_group_t;\n' % crep.cname(device), preindent = -1)
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
             '(void *_, conf_object_t *_obj, attr_value_t *_val, attr_value_t *_idx)')
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

    with NoFailure(node.site):
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
    reset_line_directive()
    if dimsizes:
        # abort on first bad value
        out('if (_status != Sim_Set_Ok) return _status;\n')
        for _ in dimsizes:
            out('}\n', preindent = -1)
        output_dml_state_change("_dev")
        out('return Sim_Set_Ok;\n')
    else:
        output_dml_state_change("_dev")
        out('return _status;\n')
    out('}\n\n', preindent = -1)
    splitting_point()

def generate_attr_getter(fname, node, port, dimsizes, cprefix, loopvars):
    device = dml.globals.device
    proto = ('attr_value_t ' + fname+'(void *_, conf_object_t *_obj, attr_value_t *_idx)')
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

    with NoFailure(node.site):
        getcode = codegen_inline_byname(
            node, port_indices + loopvars,
            '_get_attribute' if dml.globals.dml_version == (1, 2)
            else 'get_attribute',
            [], [valuevar], node.site)
        code = mkCompound(node.site, declarations(fscope) + [getcode])
        code.toc_inline()
        reset_line_directive()

    for depth, loopvar in reversed(list(enumerate(loopvars))):
        out('SIM_attr_list_set_item(&_val%d, %s, _val%d);\n'
            % (depth, loopvar.read(), depth + 1))
        out('}\n', preindent = -1)

    output_dml_state_change("_dev")
    out('return _val0;\n')
    out('}\n\n', preindent = -1)
    splitting_point()

# dimsizes, loopvars, prefix are relative to port.
def generate_attribute_common(initcode, node, port, dimsizes, prefix,
                              loopvars):
    attrname = get_attr_name(prefix, node)

    config_param = param_str(node, 'configuration', fallback='none')
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
    elif (node.objtype in ['attribute', 'connect']
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
    attr_type = param_str(
        node, 'attr_type' if dml.globals.dml_version == (1, 2)
        else '_attr_type')

    for dim in reversed(dimsizes):
        if allow_cutoff:
            attr_type = "[%s{0:%d}]" % (attr_type, dim)
        else:
            attr_type = "[%s{%d}]" % (attr_type, dim)

    register_attribute(node.site, port, attrname)
    if port:
        if port.dimensions == 0:
            register_attribute(node.site, None, "%s_%s" % (port.name, attrname))
            initcode.out(
                '_register_port_attr(class, %s, offsetof(%s, %s._obj), %s,'
                % (port_class_ident(port),
                   crep.structtype(dml.globals.device),
                   crep.cref_node(port, ()),
                   'true' if port.objtype == "bank" else 'false')
                + ' "%s", "%s", %s, %s, %s, "%s", %s);\n'
                % (port.name, attrname, getter, setter,
                   attr_flag, attr_type, doc.read()))
        elif port.dimensions == 1:
            # Generate an accessor attribute for legacy reasons
            register_attribute(node.site, None, "%s_%s" % (port.name, attrname))
            member = crep.cref_node(
                port, (mkLit(port.site, '0', TInt(32, False)),))
            (dimsize,) = port.dimsizes
            initcode.out(
                '_register_port_array_attr(class, %s, offsetof(%s, %s._obj),'
                % (port_class_ident(port),
                   crep.structtype(dml.globals.device),
                   member)
                + ' sizeof(((%s*)0)->%s), %d, %s, "%s", "%s", %s, %s, %s, "%s",'
                % (crep.structtype(dml.globals.device), member, dimsize,
                   'true' if port.objtype == "bank" else 'false',
                   port.name, attrname, getter, setter, attr_flag, attr_type)
                + ' %s);\n' % (doc.read(),))
        else:
            initcode.out(
                '_register_port_attr_no_aux(%s,'
                ' "%s", %s, %s, %s, "%s", %s);\n'
                % (port_class_ident(port),
                   attrname, getter, setter,
                   attr_flag, attr_type, doc.read()))

    else:
        initcode.out('SIM_register_typed_attribute(class, "'+attrname+'",'+
                     '\n                             '+
                 getter+', 0, '+
                 setter+', 0,'+
                 '\n                             '+
                 attr_flag + ', ' + '"'+attr_type+'", NULL,'+
                 '\n                             '+
                 doc.read() + ');\n')

# Output register attribute functions and return a string with
# initialization code
def generate_attributes(initcode, node, port=None,
                        dimsizes=(), prefix='', loopvars=()):
    if node.objtype in {'connect', 'attribute', 'register'}:
        try:
            generate_attribute_common(
                initcode, node, port, dimsizes, prefix, loopvars)
        except DMLError as e:
            report(e)
        return

    if node.objtype in {'parameter', 'method', 'session', 'saved'}:
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
    elif node.objtype in {'device', 'bank', 'port'}:
        assert prefix == ''
        assert dimsizes == ()
        assert loopvars == ()
        if node.objtype in ('bank', 'port') and (
                # anonymous bank
                dml.globals.dml_version != (1, 2) or node.name != None):
            port = node
        for child in children:
            generate_attributes(initcode, child, port, dimsizes, prefix,
                                loopvars)
    else:
        raise ICE(node, f"unknown object type {node.objtype}")

def generate_subobj_connects(init_code, device):
    if dml.globals.dml_version != (1, 2):
        t = dml.globals.traits['init_as_subobj']
        for node in device.get_recursive_components('connect'):
            if t in node.traits.ancestors:
                classname = mkStringConstant(
                    None, param_str(node, 'classname')).quoted
                desc = (mkStringConstant(None, param_str(node, "desc")).quoted
                        if param_defined(node, 'desc') else 'NULL')
                for indices in node.all_indices():
                    init_code.out(
                        '_DML_register_subobj_connect(class, '
                        + f'{classname}, "{node.logname_anonymized(indices)}",'
                        + f' {desc});\n')

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
    if retvar:
        decl = mkDeclaration(meth.site, retvar, rettype,
                             init = get_initializer(meth.site, rettype,
                                                    None, None, None))
        decl.toc()

    with LogFailure(meth.site, meth, indices):
        inargs = [mkLit(meth.site, v, t) for v, t in meth.inp]
        outargs = [mkLit(meth.site, v, t) for v, t in meth.outp]
        codegen_call(meth.site, meth,
                     indices,
                     inargs, outargs).toc()
    output_dml_state_change("_dev")

    reset_line_directive()
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
        ifacemethtype = ifacestruct.member_type(meth.name)
        if not ifacemethtype:
            raise EMEMBER(meth.site, meth.parent.name, meth.name)
        ifacemethtype = safe_realtype(ifacemethtype)
        if isinstance(ifacemethtype, TPtr):
            ifacemethtype = ifacemethtype.base
        if not isinstance(ifacemethtype, TFunction):
            raise EBTYPE(meth.site, ifacemethtype,
                         TPtr(TFunction([], TVoid())))
        iface_input_types = ifacemethtype.input_types[1:]
        iface_num_outputs = 0 if ifacemethtype.output_type.void else 1

        # Check the signature
        if len(meth.inp) != len(iface_input_types):
            raise EMETH(meth.site, None,
                        'different number of input parameters')
        if len(meth.outp) != iface_num_outputs:
            raise EMETH(meth.site, None,
                        'different number of output parameters')
        if ifacemethtype.varargs:
            # currently impossible to implement a varargs interface
            # method in DML
            raise EMETH(meth.site, None, 'interface method is variadic')
        for ((mp, mt), it) in zip(meth.inp, iface_input_types):
            if safe_realtype(mt).cmp(safe_realtype(it)) != 0:
                raise EARGT(meth.site, 'implement', meth.name,
                            mt, mp, it, 'method')
        if iface_num_outputs and dml.globals.dml_version != (1, 2):
            [(_, mt)] = meth.outp
            if safe_realtype(mt).cmp(
                    safe_realtype(ifacemethtype.output_type)) != 0:
                raise EARGT(meth.site, 'implement', meth.name,
                            mt, '<return value>', ifacemethtype.output_type,
                            'method')
        if indices is PORTOBJ:
            name = '_DML_PIFACE_' + crep.cref(meth)
        else:
            name = '_DML_IFACE_' + crep.cref(meth) + "".join([
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

    portexpr = param_expr(impl, "parent")
    assert isinstance(portexpr, NodeRef)
    (port, indices) = portexpr.get_ref()
    if not port.name:
        # anonymous bank
        assert dml.globals.dml_version == (1, 2)
        raise EANONPORT(impl.site, port)
    if not port.parent:
        # device
        port = None

    code.out("{\n", postindent = 1)

    if not port:
        code.out("static const %s = %s;\n" %
                 (ifacetype.declaration(varname),
                  interface_block(device, ifacestruct, methods, ())))
        code.out('SIM_register_interface(class, "%s", &%s);\n' %
                 (impl.name, varname))
    else:
        assert ((impl.dimensions == 0 and not port.isindexed()) or
                (impl.dimensions == port.local_dimensions()))

        desc = string_literal(get_short_doc(port))

        code.out("static const %s = %s;\n" % (
            ifacetype.declaration(varname),
            interface_block(device, ifacestruct, methods, PORTOBJ)))
        code.out('SIM_register_interface(%s, "%s", &%s);\n'
                 % (port_class_ident(port), impl.name, varname))
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

def port_class_ident(port):
    '''The C identifier used for a port class within the device class
    initialization function'''
    return '_port_class_' + port.attrname()

def generate_port_class(code, device, port):
    # this will break when we add support for nested ports. Then,
    # the inner port class should be expressed as a port class of
    # the outer port class, rather than of the device.
    assert port.parent.parent is None
    name = port.name
    desc = string_literal(get_short_doc(port))
    doc = string_literal(get_long_doc(port))
    code.out('conf_class_t *%s = _register_port_class("%s.%s", %s, %s);\n' % (
        port_class_ident(port), param_str(device, 'classname', fallback=''),
        name, desc, doc))
    port_prefix = {objects.Bank: 'bank.', objects.Port: 'port.'}[type(port)]
    if port.dimensions:
        for (i, sz) in enumerate(port.dimsizes):
            code.out('for (int _i%d = 0; _i%d < %d; ++_i%d) {\n'
                % (i, i, sz, i), postindent=1)
        code.out('strbuf_t portname = sb_newf("%s%s"%s);\n'
                     % (port_prefix,
                        port.logname_anonymized(('%d',) * port.dimensions),
                        ''.join(', _i%d' % (i,)
                                for i in range(port.dimensions))))
        code.out('SIM_register_port(class, sb_str(&portname), %s, %s);\n' % (
            port_class_ident(port), desc))
        code.out('sb_free(&portname);\n')
        for _ in range(port.dimensions):
            code.out('}\n', preindent=-1)
    else:
        code.out('SIM_register_port(class, "%s%s", %s, %s);\n' % (
            port_prefix, port.logname_anonymized(), port_class_ident(port),
            desc))

def generate_port_classes(code, device):
    for port in device.get_recursive_components('port', 'bank'):
        if port.name is None:
            assert dml.globals.dml_version == (1, 2)
            continue
        generate_port_class(code, device, port)

def generate_implements(code, device):
    for impl in device.get_recursive_components('implement'):
        try:
            generate_implement(code, device, impl)
        except DMLError as e:
            report(e)

def simple_event_fun_deconstruct_data(method, data):
    if method.dimensions > 0 and len(method.inp) > 0:
        out(f'ASSERT(SIM_attr_list_size({data}) == 2);\n')
        out(f'attr_value_t indices = SIM_attr_list_item({data}, 0);\n')
        out(f'attr_value_t params = SIM_attr_list_item({data}, 1);\n')
    elif method.dimensions > 0 or len(method.inp) > 0:
        varname = 'indices' if method.dimensions > 0 else 'params'
        out(f'attr_value_t {varname} = {data};\n')

    if method.dimensions > 0:
        out(f'ASSERT(SIM_attr_list_size(indices) == {method.dimensions});\n')

    if len(method.inp) > 0:
        out(f'ASSERT(SIM_attr_list_size(params) == {len(method.inp)});\n')

def generate_simple_events(device):
    handled = set()
    done = False

    while not done:
        done = True
        items = list(simple_events.items())
        for method, fun in items:
            if method in handled:
                continue
            site = method.site
            done = False
            handled.add(method)

            (callback_fun, destroy_fun, get_value_fun, set_value_fun) = fun
            start_function_definition(
                f'void {callback_fun}(conf_object_t *_obj, void *_data)')
            out('{\n', postindent = 1)
            out(crep.structtype(device) + ' *_dev UNUSED = ('
                + crep.structtype(device) + '*)_obj;\n')
            scope = Symtab(global_scope)

            if method.dimensions > 0 or len(method.inp) > 0:
                out('ASSERT(_data);\n')
                simple_event_fun_deconstruct_data(method,
                                                  '*(attr_value_t*)_data');

                if len(method.inp) > 0:
                    # _p_attrs deliberately named to avoid possible name
                    # conflicts with _param_*
                    out(f'attr_value_t _p_attrs[{len(method.inp)}];\n')
                    for i in range(len(method.inp)):
                        out(f'_p_attrs[{i}] = '
                            + f'SIM_attr_list_item(params, {i});\n')

                    for (i, (pname, ptype)) in enumerate(method.inp):
                        ptype = safe_realtype(ptype)
                        out(f'{ptype} _param_{pname};\n')
                        out(f'{serialize.lookup_deserialize(ptype)}'
                            + f'(&_p_attrs[{i}], &_param_{pname});\n')

                indices = tuple(mkLit(site,
                                    'SIM_attr_integer(SIM_attr_list_item('
                                    + f'indices, {i}))',
                                    TInt(32, False))
                              for i in range(method.dimensions))
                params = tuple(mkLit(site, f'_param_{pname}', ptype)
                               for pname, ptype in method.inp)
                with LogFailure(site, method, indices):
                    code = codegen_call(site, method, indices, params, ())
                code = mkCompound(site, declarations(scope) + [code])
                code.toc()
                out('SIM_attr_free((attr_value_t*)_data);\n');
                out('MM_FREE(_data);\n');
            else:
                with LogFailure(site, method, ()):
                    code = codegen_inline(site, method, (), [], [])
                code = mkCompound(site, declarations(scope) + [code])
                code.toc()
                output_dml_state_change("_dev")
            out('}\n\n', preindent = -1)
            splitting_point()

            if method.dimensions > 0 or len(method.inp) > 0:
                start_function_definition(
                    f'void {destroy_fun}(conf_object_t *_obj, void *data)')
                out('{\n', postindent = 1)
                out('ASSERT(data);\n')
                simple_event_fun_deconstruct_data(method,
                                                  '*(attr_value_t*)data');
                out('SIM_attr_free((attr_value_t*)data);\n');
                out('MM_FREE(data);\n');
                out('}\n\n', preindent = -1)
                splitting_point()

                start_function_definition(
                    f'attr_value_t {get_value_fun}'
                    + '(conf_object_t *_obj, void *data)')
                out('{\n', postindent = 1)
                out('ASSERT(data);\n')
                simple_event_fun_deconstruct_data(method,
                                                  '*(attr_value_t*)data');
                out('return SIM_attr_copy(*(attr_value_t*)data);\n');
                out('}\n\n', preindent = -1)
                splitting_point()

                start_function_definition(
                    f'void *{set_value_fun}'
                    + '(conf_object_t *_obj, attr_value_t val)')
                out('{\n', postindent = 1)
                simple_event_fun_deconstruct_data(method, 'val');
                out('attr_value_t *data = MM_MALLOC(1, attr_value_t);\n');
                out('*data = SIM_attr_copy(val);\n');
                out('return data;\n');
                out('}\n\n', preindent = -1)
                splitting_point()

    reset_line_directive()

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
            crep.cref(method),
            '_'.join(str(i) for i in indices))))
    return result

def generate_register_events(device):
    events = device.get_recursive_components('event')
    for (method, _) in list(simple_events.items()):
        add_variable_declaration(
            'event_class_t *%s' % (crep.get_evclass(method),))
    start_function_definition('void _register_events(conf_class_t *class)')
    out('{\n', postindent = 1)
    if not events and not simple_events:
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
                           for (_, cname) in event_callbacks(event, indices))))
        for (method, fun) in list(simple_events.items()):
            out('%s = SIM_register_event("%s", class, 0, %s, %s, %s, %s, NULL);\n'
                % ((crep.get_evclass(method),
                    method.logname_anonymized()) + fun))

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
    scope = Symtab(global_scope)
    fail = ReturnFailure(meth.site)
    with fail:
        inargs = [mkLit(meth.site, n, t) for n, t in meth.inp]
        outargs = [mkLit(meth.site, "*" + n, t) for n, t in meth.outp]
        code = [codegen_call(
                meth.site, meth,
                tuple(mkLit(meth.site, 'indices[%d]' % i, TInt(32, False))
                      for i in range(meth.dimensions)),
                inargs, outargs)]

    code = mkCompound(meth.site, declarations(scope) + code + [fail.nofail()])
    code.toc()
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
            generate_reg_callback(getter, '_DML_MI_%s' % crep.cref(getter),
                                  )
            generate_reg_callback(setter, '_DML_MI_%s' % crep.cref(setter))
            name = r.node.logname_anonymized(tuple("" for _ in dims),
                                             relative='bank')
            regs.append((name,
                         len(dims),
                         '_DML_MI_%s' % crep.cref(getter),
                         '_DML_MI_%s' % crep.cref(setter)))
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

def generate_alloc(device):
    start_function_definition(
        'conf_object_t *\n' + crep.cname(device)+'_alloc(conf_class_t *cls)')
    out('{\n', postindent = 1)
    out(crep.structtype(device) + ' *_dev = MM_ZALLOC(1, '
        + crep.structtype(device) + ');\n')
    objarg = '&_dev->obj'
    out('return %s;\n' % objarg)
    out('}\n\n', preindent = -1)

def generate_initialize(device):
    if dml.globals.api_version <= '6':
        start_function_definition(
            ('void %s_pre_del_notify(conf_object_t *_subscriber,'
         ' conf_object_t *_notifier, lang_void *_data)') % crep.cname(device))
        out('{\n', postindent = 1)
        out(crep.cname(device)+'_deinit(_notifier);\n')
        out('}\n\n', preindent = -1)

    start_function_definition(
        'lang_void *\n' +
        crep.cname(device)+'_init(conf_object_t *_obj)')
    out('{\n', postindent = 1)
    devstruct = crep.structtype(device)
    out(devstruct+' *_dev = ('+devstruct+'*)_obj;\n')

    out('_init_port_objs(_dev);\n')
    out('_init_static_vars(_dev);\n')
    out('_init_data_objs(_dev);\n')
    if dml.globals.dml_version == (1, 2):
        # Functions called from init_object shouldn't throw any
        # exceptions. But we don't want to force them to insert try-catch
        # in the init method.
        with InitFailure(device.site):
            # Inline the init method
            init = codegen_inline_byname(device, (), 'init', [], [], device.site)
            # Call hard_reset
            hard_reset = codegen_call_byname(device.site, device, (),
                                             'hard_reset', [], [])

        mkCompound(device.site, [init, hard_reset]).toc()
    else:
        codegen_inline_byname(device, (), '_init', [], [], device.site).toc()

    reset_line_directive()
    if dml.globals.api_version <= '6':
        out('SIM_add_notifier(_obj, Sim_Notify_Object_Delete, _obj, '
            + crep.cname(device) + '_pre_del_notify, NULL);\n')

    # Initialize table for tracking log-once feature
    out('ht_init_int_table(&(_dev->_subsequent_log_ht));\n')

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

    if dml.globals.dml_version == (1, 2):
        # Functions called from new_instance shouldn't throw any
        # exceptions.  But we don't want to force them to insert try-catch
        # in the init method.
        with LogFailure(device.site, device, ()):
            code = codegen_inline_byname(device, (), 'post_init', [], [],
                                         device.site)
    else:
        code = codegen_inline_byname(device, (), '_post_init', [], [],
                                     device.site)
    code.toc()
    out('}\n\n', preindent = -1)
    reset_line_directive()

def generate_deinit(device):
    start_function_definition(
        'void %s_deinit(conf_object_t *_obj)' % (
            crep.cname(device),))
    out('{\n', postindent = 1)
    out(crep.structtype(device) + ' *_dev UNUSED = ('
        + crep.structtype(device) + ' *)_obj;\n')
    scope = Symtab(global_scope)
    code = []

    # Cancel all events
    events = device.get_recursive_components('event')

    for event in events:
        method = event.get_component('_cancel_all', 'method')
        for index_ints in event.all_indices():
            # Functions called from pre_delete_instance shouldn't throw any
            # exceptions.  But we don't want to force them to insert try-catch
            # in the init method.
            indices = tuple(
                mkIntegerLiteral(device.site, i) for i in index_ints)
            with LogFailure(device.site, event, indices):
                code.append(codegen_inline(
                        device.site, method, indices, [], []))

        for (method, _) in list(simple_events.items()):
            code.append(mkExpressionStatement
                        (device.site,
                         mkLit(None,
                               'SIM_event_cancel_time(_obj, %s, _obj, 0, NULL)'
                               % crep.get_evclass(method),
                               TVoid())))

    with LogFailure(device.site, device, ()):
        code.append(codegen_inline_byname(device, (), 'destroy', [], [],
                                          device.site))
    # Free the tables used for log_once after all calls into device code
    # are done
    table_ptr = TPtr(TNamed("ht_int_table_t"))
    code.append(mkExpressionStatement(
        device.site,
        mkApply(device.site,
                mkLit(device.site, "_free_table",
                      TFunction([table_ptr], TVoid())),
                [mkLit(device.site, '&(_dev->_subsequent_log_ht)', table_ptr)]
                )))
    code = mkCompound(device.site, declarations(scope) + code)
    code.toc()
    out('}\n\n', preindent = -1)
    reset_line_directive()
    splitting_point()

def generate_reset(device, hardness):
    out('void\n')
    out(hardness + '_reset_' + crep.cname(device) + '('
        + crep.structtype(device) + ' *_obj)\n')
    out('{\n', postindent = 1)
    out(crep.structtype(device) + ' *_dev UNUSED = ('
        + crep.structtype(device) + ' *)_obj;\n\n')
    scope = Symtab(global_scope)
    with LogFailure(device.site, device, ()):
        code = codegen_call_byname(device.site, device, (),
                                   hardness+'_reset', [], [])
    code = mkCompound(device.site, declarations(scope) + [code])
    code.toc()
    out('}\n\n', preindent = -1)
    reset_line_directive()

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
    for port in device.get_recursive_components('port', 'bank'):
        if port.name is None:
            # anonymous bank
            continue
        port_prefix = {objects.Bank: 'bank.', objects.Port: 'port.'}[type(port)]
        if port.dimensions:
            index_enumeration = get_index_enumeration(port.site, port.dimsizes)
            for (i, sz) in enumerate(port.dimsizes):
                out('for (int _i%d = 0; _i%d < %d; ++_i%d) {\n'
                    % (i, i, sz, i), postindent=1)
            out('strbuf_t portname = sb_newf("%s%s"%s);\n'
                % (port_prefix,
                   port.logname_anonymized(('%d',) * port.dimensions),
                   ''.join(', _i%d' % (i,)
                           for i in range(port.dimensions))))
            loop_indices = tuple(
                mkLit(port.site, '_i%d' % i, TInt(32, False))
                for i in range(port.dimensions))
            index_array = "%s%s" % (
                index_enumeration.read(),
                ''.join('[_i%d]' % (i,) for i in range(port.dimensions)))
            out('_dev->%s._obj = _init_port_object(&_dev->obj'
                % (crep.cref_node(port, loop_indices),)
                + ', sb_str(&portname), %d, %s);\n'
                % (port.dimensions, index_array))
            out('sb_free(&portname);\n')
            for _ in range(port.dimensions):
                out('\n}', preindent=-1)
        else:
            out('_dev->%s._obj = _init_port_object('
                % (crep.cref_node(port, ()),)
                + '&_dev->obj, "%s%s", 0, NULL);\n'
                % (port_prefix, port.logname_anonymized()))
    out('}\n', preindent=-1)

def generate_init_static_vars(device):
    start_function_definition(
        'void _init_static_vars(%s *_dev)' % (crep.structtype(device),))
    out('{\n', postindent = 1)
    for v in device.staticvars:
        if v.init:
            assert isinstance(v.init, Initializer)
            var = mkStaticVariable(v.site, v)
            v.init.assign_to(var, var.sym.type)
    out('}\n\n', preindent = -1)
    reset_line_directive()

def generate_init_data_objs(device):
    start_function_definition(
        'void _init_data_objs(%s *_dev)' % (crep.structtype(device),))
    out('{\n', postindent = 1)
    for node in device.initdata:
        # Usually, the initializer is constant, but we permit that it
        # depends on index. When the initializer is constant, we
        # use a loop to initialize all instances of the variable across arrays;
        # if the initializer depends on an index variable, then we unfold the
        # loop to allow the initializer to be evaluated as a constant
        # expression once for every index.
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
            for indices in node.all_indices():
                index_exprs = tuple(mkIntegerLiteral(node.site, i)
                                    for i in indices)
                nref = mkNodeRef(node.site, node, index_exprs)
                try:
                    init = eval_initializer(node.site, node._type, node.astinit,
                                            Location(node.parent, index_exprs),
                                            global_scope, True)
                except DMLError as e:
                    report(e)
                else:
                    init.assign_to(nref, node._type)
        else:
            index_exprs = ()
            for (i, sz) in enumerate(node.dimsizes):
                var = 'i%d' % (i,)
                out('for (int %s = 0; %s < %s; ++%s) {\n' % (var, var, sz, var),
                    postindent=1)
                index_exprs += (mkLit(node.site, var, TInt(64, True)),)
            nref = mkNodeRef(node.site, node, index_exprs)
            init.assign_to(nref, node._type)
            for _ in range(node.dimensions):
                out('}\n', postindent=-1)
    out('}\n\n', preindent = -1)
    reset_line_directive()
    splitting_point()

ident_chars = set(chr(i)
    for (start, end) in [('A', 'Z'), ('a', 'z'), ('0', '9'), ('_', '_')]
    for i in range(ord(start), ord(end) + 1))
def init_function_name(device, outprefix):
    if dml.globals.api_version in {'4.8'}:
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
    if dml.globals.api_version >= '7':
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
        + param_str(device, 'classname', fallback='') + '", &funcs);\n')
    out('if (SIM_clear_exception() != SimExc_No_Exception) {\n', postindent = 1)
    out('fprintf(stderr, "Failed to register class '
        + param_str(device, 'classname', fallback='')
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
        for port in device.get_recursive_components('port', 'bank'):
            if port.name is None:
                assert dml.globals.dml_version == (1, 2)
            else:
                out('SIM_log_register_groups(%s, log_groups);\n'
                    % (port_class_ident(port),))
    out('\n')
    out('return class;\n')
    out('}\n\n', preindent = -1)
    splitting_point()

def generate_extern_trampoline(exported_name, func):
    params = [("_obj", TPtr(TNamed("conf_object_t")))] + func.cparams[1:]
    out("extern %s\n" % (func.rettype.declaration(
                         "%s(%s)" % (exported_name,
                                     ", ".join(t.declaration(n)
                                               for (n, t) in params)))))
    out("{\n", postindent=1)
    (name, typ) = func.cparams[0]
    out("%s = (%s)_obj;\n" % (typ.declaration(name), typ.declaration("")))
    out("%s%s(%s);\n" % ("" if func.rettype.void
                         else func.rettype.declaration("result") + " = ",
                         func.get_cname(),
                         ", ".join(n for (n, t) in func.cparams)))
    output_dml_state_change(name)
    if not func.rettype.void:
        out("return result;")
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

def som_linemark(site):
    if dml.globals.linemarks:
        out('#line %d "%s"\n' % (site.lineno, quote_filename(site.filename())))

def eom_linemark(site):
    if dml.globals.linemarks:
        out('#line %d "%s"\n' % (site.lineno, quote_filename(site.filename())))

def generate_each_in_table(node, trait, subnodes):
    ident = EachIn.ident(node, trait)
    items = []
    for sub in subnodes:
        # vtables exist, because trait instances are marked referenced
        # when an 'each in' instance is referenced
        ancestry_path = sub.traits.ancestry_paths[trait][0]
        base = '&%s%s%s' % (
            sub.traits.vtable_cname(ancestry_path[0]),
            '[0]' * sub.dimensions,
            ''.join('.' + cident(t.name)
                    for t in ancestry_path[1:]))
        num = reduce(operator.mul, sub.dimsizes, 1)
        offset = 'sizeof(struct _%s)' % (cident(ancestry_path[0].name),)
        items.append("{(uintptr_t)%s, %d, %s}\n" % (base, num, offset))
    init = '{\n%s}' % (',\n'.join('    %s' % (item,) for item in items),)
    add_variable_declaration(
        'const _vtable_list_t %s[%d]' % (ident, len(subnodes)), init)

def generate_trait_method(m):
    code = m.codegen_body()
    out('/* %s */\n' % (str(m),))
    start_function_definition(m.declaration())
    out('{\n', postindent=1)
    code.toc_inline()
    out('}\n', preindent=-1)

def generate_adjustor_thunk(traitname, name, inp, outp, throws,
                            vtable_path, def_path, hardcoded_impl=None):
    generated_name = "__adj_%s__%s__%s" % (
        traitname, '__'.join(t.name for t in vtable_path), name)
    rettype = c_rettype(outp, throws)
    out('static ' + rettype.declaration('\n%s' % (generated_name,)))
    vtable_trait = vtable_path[-1]
    assert vtable_trait is def_path[-1]
    inargs = c_inargs(vtable_trait.implicit_args() + inp, outp, throws)
    out('(%s)\n{\n' % (", ".join(t.declaration(n) for (n, t) in inargs)),
        postindent=1)
    (vt_name, vtable_trait_type) = inargs[1]
    assert vtable_trait_type.trait is vtable_trait
    out('%s _vtable = &DOWNCAST(%s, struct _%s, %s)->%s;\n' % (
        cident(vtable_trait.name), vt_name, cident(traitname),
        '.'.join(cident(t.name) for t in vtable_path),
        '.'.join(cident(t.name) for t in def_path)))
    if not rettype.void:
        out('return ')
    fun = hardcoded_impl or '_vtable->%s' % (name,)
    out('%s(_dev, %s);\n' % (fun, ", ".join(['_vtable'] + [
        name for (name, _) in inargs[2:]])))
    out('}\n', preindent=-1)
    return generated_name

def tinit_args(trait):
    '''Return the arguments of a trait's tinit method, excluding the first
    '_indices' argument. Represented as a list of vtable method names,
    also used as argument names.'''
    return sorted(itertools.chain(
        trait.vtable_methods,
        trait.vtable_params,
        trait.vtable_sessions,
        (name for name in trait.ancestor_vtables
         if (name not in trait.method_impl_traits
             or trait.method_impl_traits[name].method_impls[
                 name].overridable))))

def method_tinit_arg(trait, parent, name, scrambled_name):
    '''Return the argument passed by a trait's tinit method to its
    parent's tinit method, as a string. 'name' is the trait member
    corresponding to the argument'''
    vtable_trait = trait.ancestor_vtables[name]
    ancestry_paths = trait.ancestry_paths[vtable_trait]
    canonical_path = ancestry_paths[0]
    impl_trait = trait.method_impl_traits.get(name, None)
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
        elif not impl_trait:
            # Abstract method, so method is guaranteed to be
            # overridden (meaning that the 'name' parameter of this
            # trait's tinit method will always be non-NULL)
            return scrambled_name
        elif parent.implements(impl_trait):
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
            (_, inp, outp, throws) = vtable_trait.vtable_methods[name]
            method_impl = impl_trait.method_impls[name]
            thunk = generate_adjustor_thunk(
                trait.name, name, inp, outp, throws, canonical_path,
                impl_path,
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
        (_, inp, outp, throws) = vtable_trait.vtable_methods[name]
        # Avoid an indirect call in the adjustor thunk, for
        # methods that are not overridable
        if impl_trait and not impl_trait.method_impls[name].overridable:
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
            trait.name, name, inp, outp, throws, vtable_path, def_path,
            hardcoded_fun)
        if parent.implements(impl_trait):
            assert name in tinit_args(trait)
            # This parent's default implementation does the
            # right thing, unless overridden
            return "%s == NULL ? NULL : %s" % (scrambled_name, thunk)
        else:
            return thunk

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

    tinit_calls = []
    for parent in trait.direct_parents:
        args = []
        for name in tinit_args(parent):
            scrambled_name = scramble_argname(name)
            kind = parent.member_kind(name)
            if kind in ('parameter', 'session'):
                args.append(scrambled_name)
            else:
                assert kind == 'method'
                args.append(method_tinit_arg(trait, parent,
                                             name, scrambled_name))
        tinit_calls.append("_tinit_%s(%s);\n" % (
            parent.name, ", ".join(['&_ret->' + cident(parent.name)] + args)))

    out('static void\n')
    out('_tinit_%s(struct _%s *_ret' % (trait.name,
                                        cident(trait.name)))
    inargs = tinit_args(trait)
    for name in inargs:
        scrambled_name = scramble_argname(name)
        vtable_trait = trait.ancestor_vtables.get(name, trait)
        member_kind = trait.member_kind(name)
        if member_kind == 'method':
            (_, inp, outp, throws) = vtable_trait.vtable_methods[name]
            mtype = vtable_trait.vtable_method_type(inp, outp, throws)
            out(', %s' % (mtype.declaration(scrambled_name),))
        elif member_kind == 'parameter':
            (site, typ) = vtable_trait.vtable_params[name]
            out(', %s' % (typ.declaration(scrambled_name)))
        else:
            assert member_kind == 'session'
            out(', uint32 %s' % (scrambled_name))
    out(')\n{\n', postindent=1)
    if initializers:
        out('*_ret = (struct _%s){\n' % (cident(trait.name),), postindent=1)
        for initializer in initializers:
            out("%s,\n" % (initializer,))
        out("};\n", postindent=-1)
    for tinit_call in tinit_calls:
        out(tinit_call)
    out("}\n", preindent=-1)

def trait_trampoline_name(method, vtable_trait):
    return "%s__trampoline_from_%s" % (
        crep.cref(method), vtable_trait.name)

def flatten_object_subtree(node):
    '''return a list of all composite subobjects inside node'''
    return [node] + node.get_recursive_components(
        'event', 'port', 'implement', 'attribute', 'connect',
        'interface', 'bank', 'group', 'register', 'field')

def init_trait_vtables_for_node(node, param_values, dims, parent_indices):
    init_calls = []
    if node.isindexed():
        if all(not o.traits for o in flatten_object_subtree(node)):
            return
        indices = ()
        for (arridx, arrlen) in enumerate(node.arraylens()):
            idx = "_i%d" % (arridx + node.nonlocal_dimensions())
            dims = dims + (arrlen,)
            indices += (mkLit(node.site, idx, TInt(32, True)),)
        indices = parent_indices + indices
    else:
        indices = parent_indices
    if node.traits:
        index_str = ''.join('[%s]' % (i,) for i in indices)
        method_overrides = node.traits.method_overrides
        param_overrides = param_values[node]
        for trait in node.traits.referenced:
            args = []
            for name in tinit_args(trait):
                member_kind = trait.member_kind(name)
                if member_kind == 'method':
                    args.append(
                        trait_trampoline_name(
                            method_overrides[name], trait.vtable_trait(name))
                        if name in method_overrides else "NULL")
                elif member_kind == 'parameter':
                    # param_overrides contains C expression strings
                    args.append(param_overrides[name])
                else:
                    assert member_kind == 'session'
                    session_node = node.get_component(name)
                    assert session_node.objtype in ('session', 'saved')
                    args.append('offsetof(%s, %s)' % (
                        crep.structtype(dml.globals.device),
                        crep.cref_node(session_node, indices)))
            # initialize vtable instance
            vtable_arg = '&%s%s' % (node.traits.vtable_cname(trait), index_str)
            init_calls.append(
                (dims, '_tinit_%s(%s);\n'
                 % (trait.name, ', '.join([vtable_arg] + args))))
    for subnode in node.get_components():
        if isinstance(subnode, objects.CompositeObject):
            init_calls.extend(init_trait_vtables_for_node(
                subnode, param_values, dims, indices))
    return init_calls

def generate_trait_trampoline(method, vtable_trait):
    implicit_inargs = vtable_trait.implicit_args()
    explicit_inargs = c_inargs(list(method.inp), method.outp, method.throws)
    inparams = ", ".join(
        t.declaration(n)
        for (n, t) in implicit_inargs + explicit_inargs)
    rettype = c_rettype(method.outp, method.throws)

    # guaranteed to exist; created by ObjTraits.mark_referenced
    func = method.funcs[None]
    out('static %s\n{\n' % (
        rettype.declaration('%s(%s)' % (
            trait_trampoline_name(method, vtable_trait), inparams))),
        postindent=1)
    [_, (tname, ttype)] = implicit_inargs
    site = method.site
    obj = method.parent
    path = obj.traits.ancestry_paths[vtable_trait][0]
    if obj.dimensions:
        if path[0] is vtable_trait:
            downcast = tname
        else:
            downcast = 'DOWNCAST(%s, struct _%s, %s)' % (
                tname, cident(path[0].name),
                '.'.join(cident(t.name) for t in path[1:]))
        out('int _flat_index = %s - &%s%s;\n' % (
            downcast, obj.traits.vtable_cname(path[0]),
            '[0]' * obj.dimensions))
    indices = [
        mkLit(site, '((_flat_index / %d) %% %d)' % (
            reduce(operator.mul, obj.dimsizes[dim + 1:], 1),
            obj.dimsizes[dim]), TInt(32, True))
        for dim in range(obj.dimensions)]
    args = [mkLit(site, n, t) for (n, t) in explicit_inargs]
    call_expr = mkcall_method(
        site, func.cfunc_expr(site), indices)(args)
    if not rettype.void:
        out('return ')
    out('%s;\n' % call_expr.read())
    out('}\n', preindent=-1)

def generate_trait_trampolines(node):
    '''Generate trampolines for all used trait methods overridden in an
    object, excluding subobjects'''
    overridden = set(node.traits.method_overrides)
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
                    'struct _%s %s%s'
                     % (cident(trait.name), subnode.traits.vtable_cname(trait),
                       ''.join('[%d]' % i for i in subnode.dimsizes)))

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
        device_member = crep.cref_node(node, loop_vars)
    else:
        assert False

    relative_offset = "offsetof(%s, %s)" % (
        crep.structtype(dml.globals.device),
        device_member)

    attr_type = serialize.map_dmltype_to_attrtype(decl_site, var_ref.ctype())
    c_type = var_ref.ctype()

    for dimension in reversed(dimsizes):
        attr_type = "[%s{%d}]" % (attr_type, dimension)
    # For variables that are spread out throughout the device struct, build
    # up the correct strides through their dimensions
    if node.objtype == 'saved':
        curr_node = node.parent
        dimension_strides = []
        while curr_node.objtype != 'device':
            # if the current node is not an array, we skip it
            if curr_node.local_dimensions() == 0:
                curr_node = curr_node.parent
                continue
            loopvars = ((mkIntegerConstant(decl_site, 0, False),) *
                        curr_node.dimensions)
            # If size is constant below this point, we stop
            if not loopvars:
                break
            dimension_strides.append("sizeof(((%s*)0)->%s)" % (
                crep.structtype(dml.globals.device),
                crep.cref_node(curr_node, loopvars)))
            curr_node = curr_node.parent
        dimension_strides = tuple(reversed(dimension_strides))
    else:
        # Static variables are easier to access, with a predictable stride
        dimension_strides = tuple(
            "sizeof(((%s*)0)->%s%s)" % (
                crep.structtype(dml.globals.device),
                device_member, "[0]" * (depth + 1))
            for depth in range(node.dimensions))

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
              and child.objtype not in ('bank', 'port')):
            yield from generate_saved_userdata(
                child, dimensions + child.arraylens(),
                prefix + param_str(child, "name") + "_")

def register_saved_attributes(initcode, node):
    if node is dml.globals.device:
        for port in node.get_components('bank', 'port'):
            register_saved_attributes(initcode, port)
        cls = 'class'
        getter = '_get_saved_variable'
        setter = '_set_saved_variable'
    else:
        if dml.globals.dml_version == (1, 2) and not node.name:
            # anonymous bank
            return
        cls = port_class_ident(node)
        getter = '_get_port_saved_variable'
        setter = '_set_port_saved_variable'

    saved_userdata = list(generate_saved_userdata(node, (), ''))
    if not saved_userdata:
        return

    initcode.out('{\n', postindent=1)
    initcode.out("const struct {\n")
    initcode.out("  const char *name;\n")
    initcode.out("  const char *type;\n")
    initcode.out("} saved_attrinfo[%d] = {\n" % (len(saved_userdata),),
                 postindent=1)
    for (site, attr_name, attr_type, relative_offset, dimsizes,
         dimension_strides, deserialize_name, serialize_name) in saved_userdata:
        register_attribute(
            site, None if node is dml.globals.device else node, attr_name)
        initcode.out('{"%s", "%s"},\n' % (attr_name, attr_type))
    initcode.out('};\n', preindent=-1)
    initcode.out('static const _saved_userdata_t saved_userdata[%d] = {\n'
                 % (len(saved_userdata),), postindent=1)
    for (site, attr_name, attr_type, relative_offset, dimsizes,
         dimension_strides, deserialize_name, serialize_name) in saved_userdata:
        initcode.out('{%s, %d, %s, %s, %s, %s},\n' % (
            relative_offset, len(dimsizes),
            tuple_as_uint32_array(site, dimsizes, 'dims_' + attr_name).read(),
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
    initcode.out('SIM_register_typed_attribute(%s, ' % (cls,)
                 + 'saved_attrinfo[idx].name,\n', postindent = 1)
    initcode.out('%s, (lang_void *)&saved_userdata[idx],\n' % (getter,))
    initcode.out('%s, (lang_void *)&saved_userdata[idx],\n' % (setter,))
    initcode.out('Sim_Attr_Optional | Sim_Attr_Internal,\n')
    initcode.out('saved_attrinfo[idx].type, NULL, "saved variable");\n',
                 postindent = -1)
    initcode.out('}\n', preindent=-1)
    initcode.out('}\n', preindent=-1)

def generate_init_trait_vtables(node, param_values):
    '''Return a list of pairs (dimsizes, 'call();\n')
    meaning that call() is to be invoked with variables (_i0, _i1, ..) given by
    all possible indices within dimsizes'''
    generate_vtable_instances(node)
    for trait in traits.Trait.referenced:
        generate_tinit(trait)
    for subnode in flatten_object_subtree(node):
        generate_trait_trampolines(subnode)
    init_calls = init_trait_vtables_for_node(node, param_values, (), ())
    by_dims = {}
    for (dims, call) in init_calls:
        by_dims.setdefault(dims, []).append(call)
    all_dims = sorted(by_dims)
    fun_count = 0
    call_count = 0
    def finish_current_function(last_dims):
        for _ in last_dims:
            out('}\n', preindent=-1)
        out('}\n', preindent=-1)

    prev_dims = ()
    for dims in all_dims:
        for call in by_dims[dims]:
            # split into chunks of ~20 objects each.
            # Tested in a device with ~1500 objects,
            # where optimum chunk size seems to be between 10 and 100.
            if call_count % 20 == 0:
                if fun_count != 0:
                    finish_current_function(prev_dims)
                    prev_dims = ()
                out('static void _initialize_traits%d(void) {\n'
                    % (fun_count,), postindent=1)
                fun_count += 1
            common_dims = []
            for (a, b) in zip(dims, prev_dims):
                if a == b:
                    common_dims.append(a)
                else:
                    break
            for _ in range(len(common_dims), len(prev_dims)):
                out('}\n', preindent=-1)
            for i in range(len(common_dims), len(dims)):
                idxvar = '_i%d' % (i,)
                out('for (int %s = 0; %s < %d; %s++) {\n'
                    % (idxvar, idxvar, dims[i], idxvar),
                    postindent=1)
            prev_dims = dims
            out(call)
            call_count += 1
    if fun_count:
        finish_current_function(prev_dims)
    start_function_definition('void _initialize_traits(void)')
    out('{\n', postindent=1)
    for i in range(fun_count):
        out('_initialize_traits%d();\n' % (i,))
    out('}\n', preindent=-1)
    splitting_point()

def trait_param_value(node, param_type_site, param_type):
    try:
        expr = node.get_expr(
            tuple(mkLit(node.site, '_i%d' % (dim,), TInt(32, False))
                  for dim in range(node.dimensions)))
        if isinstance(expr, NonValue):
            raise expr.exc()
        return source_for_assignment(expr.site, param_type, expr).read()

    except DMLError as e:
        report(e)
        return "0"

def resolve_trait_param_values(node):
    '''Generate code for parameter initialization of all traits
    implemented by a node, as a dict name -> str (as C code)'''
    traits = node.traits
    return {
        name: trait_param_value(
            pnode, *traits.ancestor_vtables[name].vtable_params[name])
        for (name, pnode) in list(traits.param_nodes.items())}

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
        reset_line_directive()

c_file = None
def generate_cfile(device, footers,
                   filename_prefix, hfilename, protofilename, source_files,
                   full_module):
    global c_file

    if dml.globals.api_version == 'internal':
        api_define = ''
    else:
        sym = 'SIMICS_%s_API' % dml.globals.api_version.replace('.', '_')
        api_define = '#ifndef %s\n#define %s\n#endif\n' % (
            sym, sym)

    c_top = '\n'.join([
        '/*',
        ' * Generated by dmlc, do not edit!',
        ' *',
        ' * Source files:',
        '\n'.join([' *   ' + f for f in source_files]),
        ' */',
        '',
        api_define,
        '#include "%s"' % (hfilename,),
        '#include "%s"' % (protofilename,),
        ''])

    if c_split_threshold:
        c_file = MultiFileOutput(filename_prefix, c_top)
    else:
        c_file = FileOutput(filename_prefix + '.c')
        c_file.out(c_top)
    with c_file:
        generate_cfile_body(device, footers, full_module, filename_prefix)
    c_file.close()
    if not logging.failure:
        c_file.commit()
    c_file = None

def generate_cfile_body(device, footers, full_module, filename_prefix):

    # An output buffer for code that should be included in the init function
    init_code = StrOutput()
    init_code.out('', postindent=1)

    if dml.globals.dml_version == (1, 2):
        generate_register_tables(device)
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
    generate_dealloc(device)
    generate_events(device)
    if dml.globals.dml_version == (1, 2):
        generate_reset(device, 'hard')
        generate_reset(device, 'soft')

    trait_param_values = {
        node: resolve_trait_param_values(node)
        for node in flatten_object_subtree(device)
        if node.traits}

    for t in list(dml.globals.traits.values()):
        for m in list(t.method_impls.values()):
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

        with ErrorContext(func.method):
            if specializations:
                out('/* %s\n' % func.get_name())
                for (n, v) in specializations:
                    out('     %s = %s\n' % (n, v))
                out('*/\n')
            else:
                out('/* %s */\n' % func.get_name())

            start_function_definition(func.prototype)
            som_linemark(func.method.astcode.site)
            out('{\n', postindent = 1)
            try:
                code.toc_inline()
            except DMLError as e:
                report(e)
                # Any errors should have been caught during
                # codegen_method, when all Expression and Statement
                # objects are instantiated.
                raise ICE(e.site, 'error during late compile stage')
            eom_linemark(func.method.rbrace_site)
            out('}\n\n', preindent = -1)
            reset_line_directive()
            splitting_point()

    register_saved_attributes(init_code, device)
    generate_init(device, init_code, filename_prefix)

    generate_init_trait_vtables(device, trait_param_values)
    for ((node, trait), subobjs) in list(EachIn.instances.items()):
        generate_each_in_table(node, trait, subobjs)
    generate_simple_events(device)
    generate_register_events(device)
    generate_init_port_objs(device)
    generate_init_static_vars(device)
    generate_init_data_objs(device)
    generate_deinit(device)
    # generate_serialize must take place after register_saved_attributes
    # and generate_simple_events
    generate_serialize(device)

    for (name, (func, export_site)) in list(exported_methods.items()):
        if export_site.dml_version() == (1, 2):
            generate_extern_trampoline_dml12(name, func)
        else:
            generate_extern_trampoline(name, func)

    generate_index_enumerations()
    generate_tuple_table()

    for c in footers:
        c.toc()
        out('\n')

    reset_line_directive()

    if full_module:
        # caught as error earlier on
        assert dml.globals.api_version in ['4.8']
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
