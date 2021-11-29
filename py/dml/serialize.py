# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

# This module contains the functions used to generate methods to convert between
# dml values and attribute values

from . import ctree, expr, types, logging, symtab, messages, output, logging
from .types import *
from .logging import *
from .expr_util import *

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

serializer_t = TFunction([TPtr(const_void), TPtr(attr_value_t)], void)
deserializer_t = TFunction([TPtr(attr_value_t), TPtr(void)], set_error_t)

# code to insert before body
serialize_prototypes = []
serialize_function_code = []

# list of tuples (dml_descriptor, mapping fun)
# used to convert dmltype to attr_value_t
serialize_function_list = []
def lookup_serialize(lookup_t):
    lookup_t = safe_realtype(lookup_t)
    descriptor = type_signature(lookup_t)
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
    descriptor = type_signature(lookup_t)
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
            ctree.mkLocalVariable(site, symtab.LocalSymbol(name, name, type)))

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
    if isinstance(real_type, TInt):
        if real_type.signed:
            return construct_assign_apply("SIM_make_attr_int64", real_type)
        else:
            return construct_assign_apply("SIM_make_attr_uint64", real_type)
    if isinstance(real_type, TEndianInt):
        if real_type.signed:
            funname = "SIM_make_attr_int64"
        else:
            funname = "SIM_make_attr_uint64"
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
                                    ctree.ExpressionInitializer(apply_expr))])
    elif isinstance(real_type, TBool):
        return construct_assign_apply("SIM_make_attr_boolean", real_type)
    elif isinstance(real_type, TFloat):
        return construct_assign_apply("SIM_make_attr_floating", real_type)
    elif isinstance(real_type, TArray):
        (base, dimsizes, sizeof_base,
         dimsizes_expr) = prepare_array_de_serialization(current_site,
                                                         real_type)
        elem_serializer = expr.mkLit(current_site, lookup_serialize(base),
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
        return call_c_fun(current_site,
                          lookup_serialize(real_type),
                          [ctree.mkAddressOf(current_site, current_expr),
                           ctree.mkAddressOf(current_site, target_expr)])
    elif isinstance(real_type, TObjIdentity):
        id_infos = expr.mkLit(current_site, '_id_infos',
                              TPtr(TNamed('_id_info_t', const = True)))
        apply_expr = apply_c_fun(current_site, "_serialize_identity",
                                 [id_infos, current_expr], attr_value_t)
        return ctree.mkAssignStatement(current_site, target_expr,
                                       ctree.ExpressionInitializer(apply_expr))
    else:
        # Callers are responsible for checking that the type is serializeable
        # usually done with the map_dmltype_to_attrtype function
        raise ICE(current_site, "Unexpectedly asked to serialize %s" % (
            real_type))

# deserialize current_expr, interpreted as real_type, and assign
# to target_expr
def deserialize(real_type, current_expr, target_expr):
    current_site = current_expr.site
    def construct_assign_apply(funname, intype):
        apply_expr = apply_c_fun(current_site, funname,
                                 [current_expr], intype)
        return ctree.mkAssignStatement(current_site, target_expr,
                                       ctree.ExpressionInitializer(apply_expr))
    if isinstance(real_type, IntegerType):
        if real_type.is_endian:
            real_type = TInt(real_type.bits, real_type.signed)
        return construct_assign_apply("SIM_attr_integer", real_type)
    elif isinstance(real_type, TBool):
        return construct_assign_apply("SIM_attr_boolean", real_type)
    elif isinstance(real_type, TFloat):
        return construct_assign_apply("SIM_attr_floating", real_type)
    elif isinstance(real_type, TArray):
        (base, dimsizes, sizeof_base,
         dimsizes_expr) = prepare_array_de_serialization(current_site,
                                                         real_type)
        elem_deserializer = expr.mkLit(current_site, lookup_deserialize(base),
                                       TPtr(deserializer_t))
        return call_c_fun(current_site,
                          '_deserialize_array',
                          [current_expr,
                           ctree.mkAddressOf(current_site, target_expr),
                           sizeof_base, dimsizes_expr,
                           ctree.mkIntegerLiteral(current_site, len(dimsizes)),
                           elem_deserializer])
    elif isinstance(real_type, (TStruct, TVector)):
        return call_c_fun(current_site,
                          lookup_deserialize(real_type),
                          [ctree.mkAddressOf(current_site, current_expr),
                           ctree.mkAddressOf(current_site, target_expr)])
    elif isinstance(real_type, TObjIdentity):
        id_info_ht = expr.mkLit(current_site, '&_id_info_ht',
                                TPtr(TNamed('ht_str_table_t')))
        # _deserialize_identity returns a set_error_t which is currently unused
        return call_c_fun(current_site,
                          '_deserialize_identity',
                          [id_info_ht, current_expr,
                           ctree.mkAddressOf(current_site, target_expr)])
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
                                 for (_, mt) in real_type.members])
    if isinstance(real_type, TArray):
        # Can only save constant-size arrays
        if not real_type.size.constant:
            raise messages.ESERIALIZE(site, dmltype)
        arr_attr_type = map_dmltype_to_attrtype(site, real_type.base)
        arr_length = expr_intval(real_type.size)
        return '[%s{%s}]' % (arr_attr_type, arr_length)
    if isinstance(real_type, TObjIdentity):
        return '[s[i*]]'
    # TODO should be implemented
    #if isinstance(real_type, TVector):
        # return '[%s*]' % (map_dmltype_to_attrtype(site, real_type.base))
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
def type_signature(dmltype):
    dmltype = realtype(dmltype)
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
        arr_attr_type = type_signature(dmltype.base)
        arr_length = expr_intval(dmltype.size)
        return 'A%d%s' % (arr_length, arr_attr_type)
    if isinstance(dmltype, TVector):
        return 'V%s' % type_signature(dmltype.base)
    if isinstance(dmltype, TObjIdentity):
        return 'Id'
    assert False

def generate_serialize(real_type):
    site = logging.SimpleSite(
        "generated serialization function for %s" % real_type)
    function_name = "DML_serialize_%s" % type_signature(real_type)

    in_arg_ty = TPtr(real_type)
    out_arg_ty = TPtr(attr_value_t)
    (_, in_arg_uncasted) = declare_variable(site, "_in", TPtr(const_void))
    (in_arg_decl, in_arg) = declare_variable(
        site, "in", in_arg_ty, ctree.mkCast(site, in_arg_uncasted, in_arg_ty))
    (_, out_arg) = declare_variable(site, "out", out_arg_ty)
    function_decl = "void %s(%s, %s)" % (
        function_name,
        TPtr(const_void).declaration("_in"),
        out_arg_ty.declaration("out"))
    serialize_prototypes.append(function_decl)

    func_code = output.StrOutput()
    with func_code:
        output.out(function_decl + " {\n", postindent = 1)
        # cast void* inarg to the correct type
        in_arg_decl.toc()
        if isinstance(real_type, TStruct):
            size = len(real_type.members)
            attr_alloc_expr = apply_c_fun(
                site, "SIM_alloc_attr_list",
                [ctree.mkIntegerConstant(site, size, False)],
                attr_value_t)
            attr_assign_statement = ctree.mkAssignStatement(
                site, ctree.mkDereference(site, out_arg),
                ctree.ExpressionInitializer(attr_alloc_expr))
            imm_attr_decl, imm_attr_ref = declare_variable(
                site, "_imm_attr", attr_value_t)
            statements = []
            for i in range(size):
                index = ctree.mkIntegerConstant(site, i, False)
                (name, val_type) = real_type.members[i]
                val_ref = ctree.mkSubRef(site, in_arg, name, "->")
                sub_serialize = serialize(safe_realtype(val_type),
                                          val_ref, imm_attr_ref)
                sim_attr_list_set_statement = call_c_fun(
                    site, "SIM_attr_list_set_item", [out_arg,
                                                     index,
                                                     imm_attr_ref])
                statements += [sub_serialize, sim_attr_list_set_statement]
            ctree.mkCompound(
                site, [attr_assign_statement, imm_attr_decl] + statements).toc()
        elif isinstance(real_type, TVector):
            raise ICE(site, "TODO: serialize vector")
        elif isinstance(real_type, (IntegerType, TBool, TFloat, TObjIdentity,
                                    TArray)):
            serialize(real_type,
                      ctree.mkDereference(site, in_arg),
                      ctree.mkDereference(site, out_arg)).toc()
        else:
            assert False
        output.out("}\n", preindent = -1)
    serialize_function_code.append(func_code.buf)
    return function_name

def generate_deserialize(real_type):
    site = logging.SimpleSite(
        "generated deserialization function for %s" % (real_type,))
    function_name = "DML_deserialize_%s" % type_signature(real_type)

    in_arg_ty = TPtr(attr_value_t)
    out_arg_ty = TPtr(real_type)
    (_, in_arg) = declare_variable(site, "in", in_arg_ty)
    (_, out_arg_uncasted) = declare_variable(site, "_out", TPtr(void))
    (out_arg_decl, out_arg) = declare_variable(
        site, "out", out_arg_ty,
        ctree.mkCast(site, out_arg_uncasted, out_arg_ty))
    function_decl = "set_error_t %s(%s, %s)" % (
        function_name,
        in_arg_ty.declaration("in"),
        TPtr(void).declaration("_out"))
    serialize_prototypes.append(function_decl)

    func_code = output.StrOutput()
    with func_code:
        output.out(function_decl + " {\n", postindent = 1)
        out_arg_decl.toc()
        if isinstance(real_type, TStruct):
            size = len(real_type.members)
            statements = []
            imm_attr_decl, imm_attr_ref = declare_variable(
                site, "_imm_attr", attr_value_t)
            for i in range(size):
                index = ctree.mkIntegerConstant(site, i, False)
                (name, val_type) = real_type.members[i]
                val_ref = ctree.mkSubRef(site, out_arg, name, "->")
                sim_attr_list_item = apply_c_fun(
                    site, "SIM_attr_list_item",
                    [ctree.mkDereference(site, in_arg), index],
                    attr_value_t)
                imm_set = ctree.mkAssignStatement(
                    site, imm_attr_ref,
                    ctree.ExpressionInitializer(sim_attr_list_item))
                sub_deserialize = deserialize(
                    safe_realtype(val_type), imm_attr_ref, val_ref)
                statements += [imm_set, sub_deserialize]
            ctree.mkCompound(
                site, [imm_attr_decl] + statements).toc()
        elif isinstance(real_type, TVector):
            raise ICE(site, "TODO: serialize vector")
        elif isinstance(real_type, (IntegerType, TBool, TFloat, TObjIdentity,
                                    TArray)):
            deserialize(real_type,
                        ctree.mkDereference(site, in_arg),
                        ctree.mkDereference(site, out_arg)).toc()
        else:
            assert False
        output.out("return Sim_Set_Ok;\n")
        output.out("}\n", preindent = -1)

    serialize_function_code.append(func_code.buf)
    return function_name
