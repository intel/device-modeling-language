# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

# This module tries to contain all the code that describes the
# encoding of device state in the C device structure.

import dml.globals
from .objects import *
from .types import *
from .logging import *
from .expr_util import *
from .messages import *
from . import compat

__all__ = (
    'cname',
    'cref_method',
    'cref_portobj',
    'cref_session',
    'ctype',
    'conf_obj',
    'cloggroup',
    'dev',
    'require_dev',
    'maybe_dev_arg',
    'DeviceInstanceContext',
    'TypedParamContext',
    )


class DeviceInstanceContext:
    '''Signifies that the device instance is accessible for any
    generated statements, and is retrievable through the '_dev' identifier.
    '''
    active = False

    def __enter__(self):
        self.prev_context = DeviceInstanceContext.active
        DeviceInstanceContext.active = True
    def __exit__(self, exc_type, exc_val, exc_tb):
        DeviceInstanceContext.active = self.prev_context

class TypedParamContext:
    '''Signifies that expressions are being generated as part of the
    definition of a typed parameter, and are thus subject to the restrictions
    placed upon typed parameter definitions.
    '''
    active = False

    def __enter__(self):
        self.prev_typed_context = TypedParamContext.active
        TypedParamContext.active = True
    def __exit__(self, exc_type, exc_val, exc_tb):
        TypedParamContext.active = self.prev_typed_context

def dev(site):
    require_dev(site)
    return '_dev'

def require_dev(site):
    if not DeviceInstanceContext.active:
        raise EINDEPENDENTVIOL(site)

def maybe_dev_arg(independent):
    return ([] if independent
            else [('_dev', TDevice(structtype(dml.globals.device)))])



def cname(node):
    if dml.globals.dml_version != (1, 2):
        assert node.name
        return node.name
    elif node.objtype == 'interface':
        # this is weird... kept for compatibility
        name = param_str(node, 'c_name').replace('-', '_')
        if name != node.name and (
                compat.dml12_misc not in dml.globals.enabled_compat):
            report(WDEPRECATED(param_expr_site(node, 'c_name'),
                               'parameter c_name'))
        return name
    elif node.name:
        return node.name
    else:
        if node.objtype == 'bank':
            return '__anon'
        elif node.objtype == 'field':
            # can happen, intelbug 1806043154
            return '__implicit_field'
        else:
            raise ICE(node, 'unexpected anonymous object')

def structtype(device):
    assert device.objtype == 'device'
    return cname(device) + '_t'

def ancestor_cnames(node):
    '''return a list of names for all ancestor nodes, starting with device
    and ending with the node itself'''
    names = []
    while node:
        names.append(cname(node))
        node = node.parent
    return list(reversed(names))

def cref_method(method_node):
    assert method_node.objtype == 'method'
    # This might actually conflict, but the chances are small.
    return '__'.join(ancestor_cnames(method_node)[1:])

def cref_portobj(node, indices):
    assert node.objtype in {'port', 'bank', 'subdevice'}
    components = ['_obj']
    parent = node
    while parent.parent:
        components.append(cname(parent))
        parent = parent.parent
    return ('.'.join(reversed(components))
            + ''.join(f'[{i.read()}]' for i in indices))

def cref_session(node, indices):
    assert (node.objtype in {'session', 'saved'}
            or (dml.globals.dml_version == (1, 2)
                and node.objtype in {'field', 'register', 'interface',
                                     'attribute', 'device'})), (
                    node.objtype)
    assert isinstance(indices, tuple)
    if node.name is None and node.objtype == 'field':
        assert dml.globals.dml_version == (1, 2)
        # implicit field, inherits everything from parent register
        return cref_session(node.parent, indices)

    components = []
    if node.objtype in {'register', 'field'} and not node.simple_storage:
        assert dml.globals.dml_version == (1, 2)
        components.append('__DMLfield')
    parent = node
    while parent.parent:
        components.append(cname(parent))
        parent = parent.parent
    return ('.'.join(reversed(components))
            + ''.join(f'[{i.read()}]' for i in indices))

def cref_hook(hook, indices):
    assert hook.objtype == 'hook'
    assert isinstance(indices, tuple)
    components = []
    parent = hook
    while parent.parent:
        components.append(cname(parent))
        parent = parent.parent
    return ('.'.join(reversed(components))
            + ''.join(f'[{i.read()}]' for i in indices))

def node_storage_type(node, site = None):
    "Return the storage type for a node, or None"
    if node.objtype == 'session' or node.objtype == 'saved':
        return node._type
    elif dml.globals.dml_version == (1, 2):
        return node_storage_type_dml12(node, site)
    else:
        return None

def node_storage_type_dml12(node, site):
    if node.objtype == 'attribute':
        if param_defined(node, 'allocate_type'):
            allocate_type = param_str(node, 'allocate_type')
            if allocate_type == "string":
                return TPtr(TNamed('char'))
            else:
                return parse_type(allocate_type)
        else:
            return None
    elif node.objtype == 'method':
        if node.fully_typed:
            from . import codegen
            return codegen.method_instance(node).cfunc_type
        else:
            return None
    elif node.objtype == 'implement':
        if compat.dml12_misc in dml.globals.enabled_compat:
            typename = param_str(node, 'c_type')
            t = TNamed(typename)
            t.declaration_site = node.site
            return t
        else:
            return None
    elif node.objtype == 'interface':
        typename = param_str(node, 'c_type')
        return TPtr(TNamed(typename, const=True))
    elif node.objtype == 'device':
        return TDevice(structtype(node))
    elif node.objtype == 'register':
        # Preferably, this should never happen.  But unfortunately,
        # we have to handle this case, which is triggered when someone (e.g.
        # method get from template register) writes 'typeof($reg)' where
        # '$reg' is a register with explicit fields.
        signed = param_bool(node, 'signed')
        return TInt(param_int(node, 'bitsize'), signed)
    elif node.objtype == 'field':
        # TODO: this access to ctree is unholy. We should probably
        # make bitsize a property of the field object instead, but for
        # that to be done cleanly, we first need to fix SIMICS-9411.
        from . import ctree
        # structure.register_fields() verifies that bitsize is
        # constant across register indices; it could however happen that
        # structure.register_fields().
        indices = (ctree.mkIntegerLiteral(node.site, 0),) * node.dimensions
        msb = expr_intval(param_expr(node, 'msb', indices))
        lsb = expr_intval(param_expr(node, 'lsb', indices))
        signed = param_bool(node, 'signed')
        return TInt(msb - lsb + 1, signed)
    elif node.objtype in {'bank', 'group', 'event', 'port', 'connect',
                          'subdevice'}:
        return None
    else:
        raise ICE(site or node, "No storage type for a "+node.objtype)

def conf_object(site, node, indices):
    '''return a C expression for the conf_object_t* the given node belongs to'''
    while node.objtype not in {'bank', 'port', 'device', 'subdevice'}:
        node = node.parent
    if node.objtype == 'device' or (dml.globals.dml_version == (1, 2)
                                    and node.name is None):
        return f'&{dev(site)}->obj'
    else:
        return '%s->%s' % (dev(site),
                           cref_portobj(node, indices[:node.dimensions]))

def cloggroup(name):
    if compat.dml12_misc in dml.globals.enabled_compat:
        return name
    else:
        return '_dml_loggroup_' + name

evclasses = {}
def get_evclass(obj):
    if obj not in evclasses:
        if isinstance(obj, Method):
            evclasses[obj] = '_evclass_' + '_'.join(ancestor_cnames(obj)[1:])
        else:
            evclasses[obj] = f'_send_now_evclass_{obj.uniq}'
    return evclasses[obj]
