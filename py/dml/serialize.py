# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

# This module contains the functions used to generate methods to convert between
# dml values and attribute values

from . import ctree, expr, types, logging, symtab, messages, output, logging
from . import objects
from .types import *
from .logging import *
from .expr_util import *
from .set import Set
import dml.globals

__all__ = (
    'map_dmltype_to_attrtype',
    'serialize_prototypes',
    'serialize_function_code',
    'lookup_serialize',
    'lookup_deserialize',
    )

attr_value_t = TNamed('attr_value_t')
set_error_t = TNamed('set_error_t')
uint64_t = TInt(64, False)
const_void = void.clone()
const_void.const = True

serializer_t = TFunction([TPtr(const_void), TPtr(void)], attr_value_t)
deserializer_t = TFunction([attr_value_t, TPtr(void), TPtr(void)],
                           set_error_t)

# code to insert before body
serialize_prototypes = []
serialize_function_code = []


class SerializedTraits:
    def __init__(self):
        self.traits = Set()

    def add(self, trait):
        if trait not in self.traits:
            self.traits.add(trait)
            for node in dml.globals.trait_instances.get(trait, ()):
                node.traits.mark_referenced(trait)

# list of tuples (dml_descriptor, mapping fun)
# used to convert dmltype to attr_value_t
serialize_function_list = []
def lookup_serialize(lookup_t):
    lookup_t = safe_realtype(lookup_t)
    descriptor = type_signature(lookup_t, True)
    for (t, f) in serialize_function_list:
        # we will consider two types equal if they would get the same
        # description signature
        if t == descriptor:
            return f
    new_serialize = generate_serialize(lookup_t)
    serialize_function_list.append((descriptor, new_serialize))

    return new_serialize
# list of tuples (dmltype, mapping fun), used to convert attr_value_t to dmltype
deserialize_function_list = []
def lookup_deserialize(lookup_t):
    lookup_t = safe_realtype(lookup_t)
    descriptor = type_signature(lookup_t, False)
    for (t, f) in deserialize_function_list:
        # we will consider two types equal if they would get the same
        # description signature
        if t == descriptor:
            return f
    new_deserialize = generate_deserialize(lookup_t)
    deserialize_function_list.append((descriptor, new_deserialize))
    return new_deserialize

# Some utility functions
def declare_variable(site, name, type, init_expr = None):
    if init_expr is not None:
        init = ctree.ExpressionInitializer(init_expr)
    else:
        init = None
    return (ctree.mkDeclaration(site, name, type, init = init),
            ctree.mkLocalVariable(
                site, symtab.LocalSymbol(name, name, type, site=site)))

def prepare_array_de_serialization(site, t):
    assert(isinstance(t, TArray))
    dims = []
    base = t
    while isinstance(base, TArray):
        dims.append(base.size)
        base = base.base

    sizeof_base = expr.mkLit(site, f"sizeof({base.declaration('')})",
                             TNamed('size_t'))
    dimsizes_lit = ('(const uint32 []) { %s }'
                    % ((', '.join(dim.read() for dim in dims)),))
    dimsizes_expr = expr.mkLit(site, dimsizes_lit, TPtr(TInt(32, False)))
    return (base, dims, sizeof_base, dimsizes_expr)

# This works on the assumption that args do not need to be hard-cast
# to fit the actual fun signature
def apply_c_fun(site, fun, args, rettype):
    function_type = TFunction([a.ctype() for a in args], rettype)
    lit = expr.mkLit(site, fun, function_type)
    return expr.mkApply(site, lit, args)

def call_c_fun(site, fun, args):
    return ctree.mkExpressionStatement(
        site, apply_c_fun(site, fun, args, void))

# serialize current_expr, interpreted as real_type, and assign
# to target_expr
def serialize(real_type, current_expr, target_expr):
    current_site = current_expr.site
    def construct_assign_apply(funname, intype):
        apply_expr = apply_c_fun(current_site, funname,
                                 [current_expr], attr_value_t)
        return ctree.mkAssignStatement(current_site, target_expr,
                                       ctree.ExpressionInitializer(apply_expr))
    if real_type.is_int:
        if real_type.signed:
            funname = "SIM_make_attr_int64"
        else:
            funname = "SIM_make_attr_uint64"
        if real_type.is_endian:
            converted_arg = ctree.as_int(current_expr)
            function_type = TFunction([converted_arg.ctype], attr_value_t)
            apply_expr = expr.Apply(current_site,
                                    expr.mkLit(current_site,
                                               funname,
                                               function_type),
                                    [converted_arg],
                                    function_type)
            return ctree.mkCompound(current_site,
                                    [ctree.mkAssignStatement(
                                        current_site, target_expr,
                                        ctree.ExpressionInitializer(
                                            apply_expr))])
        else:
            return construct_assign_apply(funname, real_type)
    elif isinstance(real_type, TBool):
        return construct_assign_apply("SIM_make_attr_boolean", real_type)
    elif isinstance(real_type, TFloat):
        return construct_assign_apply("SIM_make_attr_floating", real_type)
    elif isinstance(real_type, TArray):
        (base, dimsizes, sizeof_base,
         dimsizes_expr) = prepare_array_de_serialization(current_site,
                                                         real_type)
        # NULL is used as a signal to serialize the final dimension as a data
        # attribute value.
        # This is only done for unsigned integer types of width 8 bits.
        serializer_ptr = ('NULL' if (base.is_int and not base.signed
                                     and base.bits == 8)
                          else lookup_serialize(base))

        elem_serializer = expr.mkLit(current_site, serializer_ptr,
                                     TPtr(serializer_t))
        apply_expr = apply_c_fun(current_site, '_serialize_array',
                                 [ctree.mkAddressOf(current_site,
                                                    current_expr),
                                  sizeof_base, dimsizes_expr,
                                  ctree.mkIntegerLiteral(current_site,
                                                         len(dimsizes)),
                                  elem_serializer],
                                 attr_value_t)
        return ctree.mkAssignStatement(current_site, target_expr,
                                       ctree.ExpressionInitializer(apply_expr))

    elif isinstance(real_type, (TStruct, TVector)):
        apply_expr = apply_c_fun(
            current_site, lookup_serialize(real_type),
            [ctree.mkAddressOf(current_site, current_expr)], attr_value_t)
        return ctree.mkAssignStatement(current_site, target_expr,
                                       ctree.ExpressionInitializer(apply_expr))
    elif isinstance(real_type, TTrait):
        id_infos = expr.mkLit(current_site, '_id_infos',
                              TPtr(TNamed('_id_info_t', const = True)))
        identity_expr = ctree.StructMember(current_site, current_expr, "id",
                                           TNamed("_identity_t"), ".")
        apply_expr = apply_c_fun(current_site, "_serialize_identity",
                                 [id_infos, identity_expr], attr_value_t)
        return ctree.mkAssignStatement(current_site, target_expr,
                                       ctree.ExpressionInitializer(apply_expr))
    elif isinstance(real_type, THook):
        id_infos = expr.mkLit(current_site,
                              '_hook_id_infos' if objects.Device.hooks
                              else 'NULL',
                              TPtr(TNamed('_id_info_t', const = True)))
        apply_expr = apply_c_fun(current_site, "_serialize_identity",
                                 [id_infos, current_expr], attr_value_t)
        return ctree.mkAssignStatement(current_site, target_expr,
                                       ctree.ExpressionInitializer(apply_expr))
    else:
        # Callers are responsible for checking that the type is serializeable,
        # which should be done with the mark_for_serialization function
        raise ICE(current_site, "Unexpectedly asked to serialize %s" % (
            real_type))

# deserialize current_expr, interpreted as real_type, and assign
# to target_expr. error_out constructs statements to fail deserialization
# with a given set_error_t and message.
def deserialize(real_type, current_expr, target_expr, error_out):
    current_site = current_expr.site
    def construct_assign_apply(attr_typ, intype):
        check_expr = apply_c_fun(current_site, 'SIM_attr_is_' + attr_typ,
                                 [current_expr], TBool())
        apply_expr = apply_c_fun(current_site, 'SIM_attr_' + attr_typ,
                                 [current_expr], intype)
        error_stmts = error_out('Sim_Set_Illegal_Type', 'expected ' + attr_typ)

        target = target_expr
        if real_type.const and not realtype(target_expr.ctype()).const:
            # This can happen if constness is inherited from a parent
            # deserialization of a struct type.
            # Rewrite target_expr to an equivalent lvalue with constified type,
            # so that ExpressionInitializer's deep_const logic kicks in.
            target = ctree.mkDereference(
                current_site,
                ctree.mkCast(current_site,
                             ctree.mkAddressOf(current_site, target_expr),
                             TPtr(real_type)))

        return ctree.mkIf(current_site,
                          check_expr,
                          ctree.mkAssignStatement(
                              current_site, target,
                              ctree.ExpressionInitializer(apply_expr)),
                          ctree.mkCompound(current_site, error_stmts))

    def addressof_target_unconst():
        base = ctree.mkAddressOf(current_site, target_expr)
        if deep_const(real_type):
            base = ctree.mkCast(current_site, base, TPtr(void))
        return base

    def construct_subcall(apply_expr):
        (sub_success_decl, sub_success_arg) = \
            declare_variable(current_site, "_sub_success", set_error_t)
        assign_stmt = ctree.mkAssignStatement(
            current_site, sub_success_arg,
            ctree.ExpressionInitializer(apply_expr))
        check_expr = ctree.mkLit(current_site,
                                 f'{sub_success_arg.read()} != Sim_Set_Ok',
                                 TBool())
        return ctree.mkCompound(current_site,
                                [sub_success_decl, assign_stmt,
                                 ctree.mkIf(current_site, check_expr,
                                      ctree.mkCompound(
                                          current_site,
                                          error_out(sub_success_arg.read(),
                                                    None)))])

    if real_type.is_int:
        if real_type.is_endian:
            real_type = TInt(real_type.bits, real_type.signed)
        return construct_assign_apply("integer", real_type)
    elif isinstance(real_type, TBool):
        return construct_assign_apply("boolean", real_type)
    elif isinstance(real_type, TFloat):
        return construct_assign_apply("floating", real_type)
    elif isinstance(real_type, TArray):
        (base, dimsizes, sizeof_base,
         dimsizes_expr) = prepare_array_de_serialization(current_site,
                                                         real_type)
        elem_deserializer = expr.mkLit(current_site, lookup_deserialize(base),
                                       TPtr(deserializer_t))
        # elems_are_bytes informs if the final dimension may either be
        # deserialized as a list or a data attribute value.
        # This is true for all integer types of width 8 bits
        elems_are_bytes = ctree.mkBoolConstant(current_site,
                                               base.is_int and base.bits == 8)
        apply_expr = apply_c_fun(
            current_site, '_deserialize_array',
            [current_expr, addressof_target_unconst(),
             sizeof_base, dimsizes_expr,
             ctree.mkIntegerLiteral(current_site, len(dimsizes)),
             elem_deserializer, elems_are_bytes], set_error_t)
        return construct_subcall(apply_expr)
    elif isinstance(real_type, (TStruct, TVector)):
        apply_expr = apply_c_fun(
            current_site, lookup_deserialize(real_type),
            [current_expr, addressof_target_unconst()],
            set_error_t)
        return construct_subcall(apply_expr)
    elif isinstance(real_type, TTrait):
        id_info_ht = expr.mkLit(current_site, '&_id_info_ht',
                                TPtr(TNamed('ht_str_table_t')))
        assert dml.globals.object_trait
        if real_type.trait is dml.globals.object_trait:
            object_vtable_array = expr.mkLit(current_site, '_object_vtables',
                                             TPtr(TPtr(void, const=True)))
            apply_expr = apply_c_fun(
                current_site, '_deserialize_object_trait_reference',
                [id_info_ht, object_vtable_array,
                 current_expr, addressof_target_unconst()],
                set_error_t)
        else:
            vtable_name = real_type.trait.name
            vtable_ht = expr.mkLit(current_site,
                                   'NULL' if real_type.trait.empty()
                                   else f'&_{cident(vtable_name)}_vtable_ht',
                                   TPtr(TNamed('ht_int_table_t')))
            apply_expr = apply_c_fun(
                current_site, '_deserialize_trait_reference',
                [id_info_ht, vtable_ht,
                 ctree.mkStringConstant(current_site, vtable_name),
                 current_expr, addressof_target_unconst()],
                set_error_t)
        return construct_subcall(apply_expr)
    elif isinstance(real_type, THook):
        id_info_ht = expr.mkLit(current_site,
                                '&_hook_id_info_ht'
                                if objects.Device.hooks else 'NULL',
                                TPtr(TNamed('ht_str_table_t')))
        hook_aux_infos = expr.mkLit(current_site,
                                    '_hook_aux_infos'
                                    if objects.Device.hooks else 'NULL',
                                    TPtr(const_void))
        from .codegen import get_type_sequence_info
        expected_typ_uniq = ctree.mkIntegerConstant(
            current_site,
            get_type_sequence_info(real_type.msg_types).uniq,
            False)

        apply_expr = apply_c_fun(
            current_site, '_deserialize_hook_reference',
            [id_info_ht, hook_aux_infos, expected_typ_uniq, current_expr,
             addressof_target_unconst()],
            set_error_t)
        return construct_subcall(apply_expr)
    else:
        raise ICE(current_site, "Unexpectedly asked to deserialize %s" % (
            real_type))

def map_dmltype_to_attrtype(site, dmltype):
    "return attrtype string describing this dmltype"
    # raw data instead
    real_type = safe_realtype(dmltype)
    # tints, endian_ints, and their bitfield variants can all be safely
    # stored in integer attributes
    if isinstance(real_type, IntegerType):
        return 'i'
    if isinstance(real_type, TBool):
        return 'b'
    if isinstance(real_type, TFloat):
        return 'f'
    if isinstance(real_type, TStruct):
        return '[%s]' % "".join([map_dmltype_to_attrtype(site, mt)
                                 for mt in real_type.members.values()])
    if isinstance(real_type, TArray):
        assert real_type.size.constant
        arr_attr_type = map_dmltype_to_attrtype(site, real_type.base)
        arr_length = expr_intval(real_type.size)
        # Byte arrays may use data values
        or_data = '|d' * (real_type.base.is_int and real_type.base.bits == 8)
        return '[%s{%s}]' % (arr_attr_type, arr_length) + or_data
    if isinstance(real_type, (TTrait, THook)):
        return '[s[i*]]'
    # TODO should be implemented
    #if isinstance(real_type, TVector):
        # return '[%s*]' % (map_dmltype_to_attrtype(site, real_type.base))
    raise ICE(site, 'unserializable type: %r' % (dmltype,))

def mark_for_serialization(site, dmltype):
    '''check a dml type for serializability and ensure artifacts needed for
    calls of serialize/deserialize to result in valid C are generated
    '''
    real_type = safe_realtype(dmltype)
    if isinstance(real_type, TStruct):
        for mt in real_type.members.values():
            mark_for_serialization(site, mt)
    elif isinstance(real_type, TArray):
        # Can only serialize constant-size arrays
        if not real_type.size.constant:
            raise messages.ESERIALIZE(site, dmltype)
        mark_for_serialization(site, real_type.base)
    elif isinstance(real_type, TTrait):
        dml.globals.serialized_traits.add(real_type.trait)
    elif isinstance(real_type, THook):
        real_type.validate(dmltype.declaration_site or site)
        from .codegen import get_type_sequence_info
        get_type_sequence_info(real_type.msg_types, create_new=True)
    elif not isinstance(real_type, (IntegerType, TBool, TFloat)):
        raise messages.ESERIALIZE(site, dmltype)

# generate a part of the function name from a description of the dmltype
# Each type maps uniquely to a string, obeying the following invariants:
# * Types that serialize differently must map to different strings (but
#   the converse does not hold; two types with identical serialization may map
#   to different strings)
# * The first character of each type is a unique capital letter
# * One valid type string can never be a prefix of another valid type string
# * Compound types can be parameterized by at most one type, and the parameter
#   type's signature is a suffix of the compound type's signature.
#
# The is_for_serialization parameter is to determine whether the type signature
# will be used for the name of a serialization function or a deserialization
# function. The distinction matters for types where the serialization function
# can be shared across multiple variants of the same type, but the
# deserialization function can't; so in the serialization case the signatures
# should be the same across all variants of the type, but in the
# deserialization case the signatures should be separate.
# Trait references are an example of such a type.
def type_signature(dmltype, is_for_serialization):
    dmltype = realtype(dmltype)
    if isinstance(dmltype, TLong):
        return f'IL{"s" if dmltype.signed else "u"}'
    if isinstance(dmltype, TSize):
        return f'IS{"s" if dmltype.signed else "u"}'
    if isinstance(dmltype, IntegerType):
        return 'I%d%s%s' % (dmltype.bits,
                            dmltype.byte_order[0] if dmltype.is_endian else "h",
                            "s" if dmltype.signed else "u")
    if isinstance(dmltype, TBool):
        return 'B'
    if isinstance(dmltype, TFloat):
        return {'double': 'Fd', 'float': 'Fs'}[dmltype.name]
    if isinstance(dmltype, TStruct):
        return 'S' + dmltype.label
    if isinstance(dmltype, TArray):
        assert dmltype.size.constant
        arr_attr_type = type_signature(dmltype.base, is_for_serialization)
        arr_length = expr_intval(dmltype.size)
        return 'A%d%s' % (arr_length, arr_attr_type)
    if isinstance(dmltype, TVector):
        return 'V%s' % type_signature(dmltype.base, is_for_serialization)
    if isinstance(dmltype, TTrait):
        return 'T' + (cident(dmltype.trait.name)
                      if not is_for_serialization else '')
    if isinstance(dmltype, THook):
        from .codegen import get_type_sequence_info
        suffix = (str(get_type_sequence_info(dmltype.msg_types).uniq)
                  if not is_for_serialization else '')
        return 'H' + suffix
    assert False

def serialize_sources_to_list(site, sources, out_attr):
    sources = tuple(sources)
    size = len(sources)
    attr_alloc_expr = apply_c_fun(
        site, "SIM_alloc_attr_list",
        [ctree.mkIntegerConstant(site, size, False)],
        attr_value_t)
    attr_assign_statement = ctree.mkAssignStatement(
        site, out_attr, ctree.ExpressionInitializer(attr_alloc_expr))
    imm_attr_decl, imm_attr_ref = declare_variable(
        site, "_imm_attr", attr_value_t)
    statements = []
    for (i, source_info) in enumerate(sources):
        index = ctree.mkIntegerConstant(site, i, False)
        if source_info is None:
            sub_serialize = ctree.mkInline(site,
                                           '_imm_attr = SIM_make_attr_nil();')
        else:
            (source, typ) = source_info
            if typ is not None:
                sub_serialize = serialize(typ, source, imm_attr_ref)
            else:
                sub_serialize = ctree.mkAssignStatement(
                    site, imm_attr_ref, ctree.ExpressionInitializer(source))
        sim_attr_list_set_statement = call_c_fun(
            site, "SIM_attr_list_set_item", [ctree.mkAddressOf(site, out_attr),
                                             index,
                                             imm_attr_ref])
        statements += [sub_serialize, sim_attr_list_set_statement]
    ctree.mkCompound(
        site, [attr_assign_statement, imm_attr_decl] + statements).toc()

def generate_serialize(real_type):
    site = logging.SimpleSite(
        f"<generated serialization function for {real_type}>")
    function_name = "DML_serialize_%s" % type_signature(real_type, True)

    in_arg_ty = TPtr(real_type)
    (_, in_arg_uncasted) = declare_variable(site, "_in", TPtr(const_void))
    (in_arg_decl, in_arg) = declare_variable(
        site, "in", in_arg_ty, ctree.mkCast(site, in_arg_uncasted, in_arg_ty))
    (out_arg_decl, out_arg) = declare_variable(site, "out", attr_value_t)
    function_decl = "attr_value_t %s(%s)" % (
        function_name,
        TPtr(const_void).declaration("_in"))
    serialize_prototypes.append(function_decl)

    func_code = output.StrOutput()
    with func_code:
        output.out(function_decl + " {\n", postindent = 1)
        # cast void* inarg to the correct type
        in_arg_decl.toc()
        out_arg_decl.toc()
        if isinstance(real_type, TStruct):
            sources = ((ctree.mkSubRef(site, in_arg, name, "->"),
                        safe_realtype(typ))
                       for (name, typ) in real_type.members.items())
            serialize_sources_to_list(site, sources, out_arg)
        elif isinstance(real_type, TVector):
            raise ICE(site, "TODO: serialize vector")
        elif isinstance(real_type, (IntegerType, TBool, TFloat, TTrait,
                                    TArray, THook)):
            serialize(real_type,
                      ctree.mkDereference(site, in_arg),
                      out_arg).toc()
        else:
            assert False
        output.out("return out;\n")
        output.out("}\n", preindent = -1)
    serialize_function_code.append(func_code.buf)
    return function_name

def deserialize_list_to_targets(site, val_attr, targets, error_out_at_index,
                                error_context=None):
    targets = tuple(targets)
    imm_attr_decl, imm_attr_ref = declare_variable(
        site, "_imm_attr", attr_value_t)
    statements = [imm_attr_decl]
    for (i, target_info) in enumerate(targets):
        index = ctree.mkIntegerConstant(site, i, False)
        sim_attr_list_item = apply_c_fun(site, "SIM_attr_list_item",
            [val_attr, index], attr_value_t)
        imm_set = ctree.mkAssignStatement(
            site, imm_attr_ref,
            ctree.ExpressionInitializer(sim_attr_list_item))
        statements.append(imm_set)
        if target_info is not None:
            (target, typ) = target_info
            if typ is not None:
                def sub_error_out(exc, msg):
                    sub_error_context = (
                        f" during deserialization of list item {i}"
                        + f' during {error_context}' * bool(error_context))
                    return error_out_at_index(i,
                                              exc,
                                              msg + sub_error_context
                                              if msg is not None else msg)
                sub_deserialize = deserialize(typ, imm_attr_ref, target,
                                              sub_error_out)
            else:
                sub_deserialize = ctree.mkAssignStatement(
                    site, target, ctree.ExpressionInitializer(imm_attr_ref))
            statements.append(sub_deserialize)
        else:
            is_not_nil_expr = expr.mkLit(site, '!SIM_attr_is_nil(_imm_attr)',
                                         TBool())
            statements.append(ctree.mkIf(
                site, is_not_nil_expr,
                ctree.mkCompound(
                    site,
                    error_out_at_index(
                        i,
                        'Sim_Set_Illegal_Type',
                        f'expected list item {i} to be nil'
                        + f' during {error_context}' * bool(error_context)
                    ))))
    deserialization = ctree.mkCompound(site, statements)
    attr_is_list = apply_c_fun(site, "SIM_attr_is_list", [val_attr], TBool())

    attr_list_size = apply_c_fun(site, "SIM_attr_list_size", [val_attr],
                                 TInt(32, False))
    attr_list_size_check = ctree.mkEquals(
        site, attr_list_size,
        ctree.mkIntegerConstant(site, len(targets), False))

    is_list_expr = ctree.mkAnd(site, attr_is_list, attr_list_size_check)
    ctree.mkIf(
        site, is_list_expr, deserialization,
        ctree.mkCompound(
            site,
            error_out_at_index(
                0, # TODO: None instead?
                'Sim_Set_Illegal_Type',
                f'expected list of size {len(targets)}'
                + f' during {error_context}' * bool(error_context))
            )
    ).toc()

def generate_deserialize(real_type):
    site = logging.SimpleSite(
        f"<generated deserialization function for {real_type}>")
    function_name = "DML_deserialize_%s" % type_signature(real_type, False)

    out_arg_ty = TPtr(real_type)
    (_, in_arg) = declare_variable(site, "in", attr_value_t)
    (_, out_arg_uncasted) = declare_variable(site, "_out", TPtr(void))
    (out_arg_decl, out_arg) = declare_variable(
        site, "out", out_arg_ty,
        ctree.mkCast(site, out_arg_uncasted, out_arg_ty))
    function_decl = "set_error_t %s(%s, %s)" % (
        function_name,
        attr_value_t.declaration("in"),
        TPtr(void).declaration("_out"))
    serialize_prototypes.append(function_decl)

    func_code = output.StrOutput()
    with func_code:
        cleanup = []
        output.out(function_decl + " {\n", postindent = 1)
        out_arg_decl.toc()
        output.out("set_error_t _success UNUSED = Sim_Set_Ok;\n")
        def error_out(exc, msg):
            stmts = []
            stmts.append(ctree.mkInline(site, f'_success = {exc};'))
            if msg is not None:
                stmts.append(
                    ctree.mkInline(site, f'SIM_attribute_error("{msg}");'))
            stmts.append(ctree.mkInline(site, 'goto _exit;'))
            return stmts
        if isinstance(real_type, TStruct):
            (tmp_out_decl, tmp_out_ref) = declare_variable(
                site, "_tmp_out", TPtr(real_type),
                ctree.mkNew(site, real_type))
            cleanup_ref = (tmp_out_ref if not deep_const(real_type)
                           else ctree.mkCast(site, tmp_out_ref, TPtr(void)))
            cleanup.append(ctree.mkDelete(site, cleanup_ref))
            tmp_out_decl.toc()
            targets = tuple((ctree.mkSubRef(site, tmp_out_ref, name, "->"),
                             conv_const(real_type.const, safe_realtype(typ)))
                            for (name, typ) in real_type.members.items())
            def error_out_at_index(_i, exc, msg):
                return error_out(exc, msg)
            deserialize_list_to_targets(site, in_arg, targets,
                                        error_out_at_index,
                                        f'deserialization of {real_type}')
            ctree.mkAssignStatement(site,
                                    ctree.mkDereference(site, out_arg),
                                    ctree.ExpressionInitializer(
                                        ctree.mkDereference(
                                            site, tmp_out_ref))).toc()

        elif isinstance(real_type, TVector):
            raise ICE(site, "TODO: serialize vector")
        elif isinstance(real_type, (IntegerType, TBool, TFloat, TTrait,
                                    TArray, THook)):
            deserialize(real_type,
                        in_arg,
                        ctree.mkDereference(site, out_arg),
                        error_out).toc()
        else:
            assert False
        output.out("_exit:\n")
        for stmt in cleanup:
            stmt.toc()
        output.out("return _success;\n")
        output.out("}\n", preindent = -1)

    serialize_function_code.append(func_code.buf)
    return function_name
