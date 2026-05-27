# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

from .expr import Lit, mkApply, mkLit
from . import ctree as c
from . import types as tp
from .symtab import global_scope, Symtab
from .crep import node_storage_type
from . import globals

reg_table_type    = tp.Ptr(tp.Named('_dml_reg_t', const=True))
regmap_table_type = tp.Ptr(tp.Named('_dml_reg_number_t', const=True))

make_regname = mkLit(None, '_DML_make_regname',
                     tp.Function([reg_table_type, regmap_table_type],
                               tp.Ptr(tp.Named('char', const=True))))
find_regnum = mkLit(None, '_DML_find_regnum',
                    tp.Function([regmap_table_type,
                               tp.Int(32, False),
                               tp.Int(32, False)],
                              regmap_table_type))
find_regname = mkLit(None, '_DML_find_regname',
                    tp.Function([regmap_table_type,
                               tp.Int(32, False),
                               tp.Ptr(tp.Named('char', const=True)),
                               reg_table_type],
                              regmap_table_type))

def return_success(site):
    return c.mkReturn(site, c.mkBoolConstant(site, False))

dml_reg_t = tp.ExternStruct({}, '_dml_reg_t', const=True)

def codegen_get_name(impl, indices, inargs, outargs, site):
    bank = impl.parent

    [num] = inargs
    [name] = outargs

    if not bank.numbered_registers:
        return c.mkCompound(
            site, [c.mkCopyData(site, Lit(None, 'NULL', tp.Ptr(tp.Void()), 1), name),
                   return_success(site)])

    reg_table = mkLit(None, '_DML_R_' + bank.name, tp.Ptr(dml_reg_t))
    regnum_table = mkLit(None, '_DML_RN_' + bank.name,
                         regmap_table_type)

    # name = make_regname(reg_table, find_regnum(regnum_table, num))
    regnum_table_len = mkLit(site, 'ALEN(_DML_RN_%s)' % bank.name,
                             tp.Int(32, False))
    reg = mkApply(site, find_regnum, [regnum_table, regnum_table_len, num])
    regname = mkApply(site, make_regname, [reg_table, reg])
    return c.mkCompound(
        site,
        [c.mkCopyData(site, regname, name),
         c.mkIf(site,
              c.mkNot(site, c.as_bool(name)),
              c.log_statement(site, c.log_object(site, bank, indices), "error",
                            None, None, "There is no register with number %d",
                            num)),
         return_success(site)])

def codegen_get_number(impl, indices, inargs, outargs, site):
    bank = impl.parent

    [name] = inargs
    [num] = outargs

    if not bank.numbered_registers:
        return c.mkCompound(
            site, [c.mkCopyData(site, c.mkIntegerConstant(site, -1, True), num),
                   return_success(site)])

    reg_table = mkLit(None, '_DML_R_' + bank.name, tp.Ptr(dml_reg_t))
    regnum_table = mkLit(None, '_DML_RN_' + bank.name,
                         regmap_table_type)

    # num = find_regname(regnum_table, name)->num
    regnum_table_len = mkLit(site, 'ALEN(_DML_RN_%s)' % bank.name,
                             tp.Int(32, False))
    reg = c.ExpressionInitializer(
        mkApply(site, find_regname, [regnum_table, regnum_table_len, name,
                                     reg_table]))

    scope = Symtab(global_scope)
    regvar = c.mkLocalVariable(site, scope.add_variable(
        'reg',
        type=regmap_table_type,
        init=reg,
        make_unique=True,
        site=site))

    return c.mkCompound(
        site,
        [c.sym_declaration(regvar.sym),
         c.mkIf(site,
              c.as_bool(regvar),
              c.mkCopyData(site,
                         c.mkSubRef(site, regvar, 'num', '->'),
                         num),
              c.mkCompound(site,
                         [c.mkCopyData(site,
                                     c.mkIntegerConstant(site, -1, True),
                                     num),
                          c.log_statement(site, c.log_object(site, bank, indices),
                                        "error", None, None,
                                        "There is no register with name %s",
                                        name)])),
         return_success(site)])

def codegen_read(impl, indices, inargs, outargs, site):
    bank = impl.parent
    [num] = inargs
    [val] = outargs

    if not bank.numbered_registers:
        return c.mkCompound(
            site, [c.mkCopyData(site, c.mkIntegerLiteral(site, 0), val),
                   return_success(site)])

    reg_table = mkLit(None, '_DML_R_' + bank.name, tp.Ptr(dml_reg_t))
    regmap_table = mkLit(None, '_DML_RN_' + bank.name,
                         regmap_table_type)
    regmap_table_len = mkLit(site, 'ALEN(_DML_RN_%s)' % bank.name,
                             tp.Int(32, False))

    scope = Symtab(global_scope)
    regvar = c.mkLocalVariable(site, scope.add_variable(
        'reg',
        type=regmap_table_type,
        init=c.ExpressionInitializer(
            mkApply(site, find_regnum,
                    [regmap_table, regmap_table_len, num])),
        make_unique=True,
        site=site))

    devtype = node_storage_type(globals.device)
    read_reg = mkLit(None, '_DML_read_reg',
                     tp.Function([devtype,
                                regmap_table_type,
                                reg_table_type],
                               tp.Int(64, False)))

    return c.mkCompound(
        site,
        [c.sym_declaration(regvar.sym),
         c.mkIf(site,
              c.as_bool(regvar),
              c.mkCopyData(site,
                         mkApply(site, read_reg,
                                 [mkLit(site, '_dev', devtype),
                                  regvar,
                                  reg_table]),
                         val),
              c.log_statement(site, c.log_object(site, bank, indices), "error",
                            None, None, "There is no register with number %d",
                            num)),
         return_success(site)])

def codegen_write(impl, indices, inargs, outargs, site):
    bank = impl.parent
    [num, val] = inargs

    if not bank.numbered_registers:
        return c.mkCompound(site, [return_success(site)])

    reg_table = mkLit(None, '_DML_R_' + bank.name, tp.Ptr(dml_reg_t))
    regmap_table = mkLit(None, '_DML_RN_' + bank.name,
                         regmap_table_type)
    regmap_table_len = mkLit(site, 'ALEN(_DML_RN_%s)' % bank.name,
                             tp.Int(32, False))

    scope = Symtab(global_scope)
    regvar = c.mkLocalVariable(site, scope.add_variable(
        'reg',
        site=site,
        type=regmap_table_type,
        init=c.ExpressionInitializer(
            mkApply(site, find_regnum,
                    [regmap_table, regmap_table_len, num])),
        make_unique=True))

    devtype = node_storage_type(globals.device)
    write_reg = mkLit(None, '_DML_write_reg',
                     tp.Function([devtype,
                                regmap_table_type,
                                reg_table_type,
                                tp.Int(64, False)],
                               tp.Void()))

    return c.mkCompound(
        site,
        [c.sym_declaration(regvar.sym),
         c.mkIf(site,
              c.as_bool(regvar),
              c.mkExpressionStatement(site,
                                    mkApply(site, write_reg,
                                            [mkLit(site, '_dev', devtype),
                                             regvar,
                                             reg_table,
                                             val])),
              c.log_statement(site, c.log_object(site, bank, indices), "error",
                            None, None, "There is no register with number %d",
                            num)),
         return_success(site)])
