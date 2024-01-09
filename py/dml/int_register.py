# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

from .expr import *
from .ctree import *
from .types import *
from .logging import dbg
from .symtab import *
from .crep import node_storage_type
from . import globals

reg_table_type    = TPtr(TNamed('_dml_reg_t', const=True))
regmap_table_type = TPtr(TNamed('_dml_reg_number_t', const=True))

make_regname = mkLit(None, '_DML_make_regname',
                     TFunction([reg_table_type, regmap_table_type],
                               TPtr(TNamed('char', const=True))))
find_regnum = mkLit(None, '_DML_find_regnum',
                    TFunction([regmap_table_type,
                               TInt(32, False),
                               TInt(32, False)],
                              regmap_table_type))
find_regname = mkLit(None, '_DML_find_regname',
                    TFunction([regmap_table_type,
                               TInt(32, False),
                               TPtr(TNamed('char', const=True)),
                               reg_table_type],
                              regmap_table_type))

def return_success(site):
    return mkReturn(site, mkBoolConstant(site, False))

dml_reg_t = TExternStruct({}, '_dml_reg_t', const=True)

def codegen_get_name(impl, indices, inargs, outargs, site):
    bank = impl.parent

    [num] = inargs
    [name] = outargs

    if not bank.numbered_registers:
        return mkCompound(
            site, [mkCopyData(site, Lit(None, 'NULL', TPtr(TVoid()), 1), name),
                   return_success(site)])

    reg_table = mkLit(None, '_DML_R_' + bank.name, TPtr(dml_reg_t))
    regnum_table = mkLit(None, '_DML_RN_' + bank.name,
                         regmap_table_type)

    # name = make_regname(reg_table, find_regnum(regnum_table, num))
    regnum_table_len = mkLit(site, 'ALEN(_DML_RN_%s)' % bank.name,
                             TInt(32, False))
    reg = mkApply(site, find_regnum, [regnum_table, regnum_table_len, num])
    regname = mkApply(site, make_regname, [reg_table, reg])
    return mkCompound(
        site,
        [mkCopyData(site, regname, name),
         mkIf(site,
              mkNot(site, as_bool(name)),
              log_statement(site, log_object(site, bank, indices), "error",
                            None, None, "There is no register with number %d",
                            num)),
         return_success(site)])

def codegen_get_number(impl, indices, inargs, outargs, site):
    bank = impl.parent

    [name] = inargs
    [num] = outargs

    if not bank.numbered_registers:
        return mkCompound(
            site, [mkCopyData(site, mkIntegerConstant(site, -1, True), num),
                   return_success(site)])

    reg_table = mkLit(None, '_DML_R_' + bank.name, TPtr(dml_reg_t))
    regnum_table = mkLit(None, '_DML_RN_' + bank.name,
                         regmap_table_type)

    # num = find_regname(regnum_table, name)->num
    regnum_table_len = mkLit(site, 'ALEN(_DML_RN_%s)' % bank.name,
                             TInt(32, False))
    reg = ExpressionInitializer(
        mkApply(site, find_regname, [regnum_table, regnum_table_len, name,
                                     reg_table]))

    scope = Symtab(global_scope)
    regvar = mkLocalVariable(site, scope.add_variable(
        'reg',
        type=regmap_table_type,
        init=reg,
        make_unique=True,
        site=site))

    return mkCompound(
        site,
        [sym_declaration(regvar.sym),
         mkIf(site,
              as_bool(regvar),
              mkCopyData(site,
                         mkSubRef(site, regvar, 'num', '->'),
                         num),
              mkCompound(site,
                         [mkCopyData(site,
                                     mkIntegerConstant(site, -1, True),
                                     num),
                          log_statement(site, log_object(site, bank, indices),
                                        "error", None, None,
                                        "There is no register with name %s",
                                        name)])),
         return_success(site)])

def codegen_read(impl, indices, inargs, outargs, site):
    bank = impl.parent
    [num] = inargs
    [val] = outargs

    if not bank.numbered_registers:
        return mkCompound(
            site, [mkCopyData(site, mkIntegerLiteral(site, 0), val),
                   return_success(site)])

    reg_table = mkLit(None, '_DML_R_' + bank.name, TPtr(dml_reg_t))
    regmap_table = mkLit(None, '_DML_RN_' + bank.name,
                         regmap_table_type)
    regmap_table_len = mkLit(site, 'ALEN(_DML_RN_%s)' % bank.name,
                             TInt(32, False))

    scope = Symtab(global_scope)
    regvar = mkLocalVariable(site, scope.add_variable(
        'reg',
        type=regmap_table_type,
        init=ExpressionInitializer(
            mkApply(site, find_regnum,
                    [regmap_table, regmap_table_len, num])),
        make_unique=True,
        site=site))

    devtype = node_storage_type(globals.device)
    read_reg = mkLit(None, '_DML_read_reg',
                     TFunction([devtype,
                                regmap_table_type,
                                reg_table_type],
                               TInt(64, False)))

    return mkCompound(
        site,
        [sym_declaration(regvar.sym),
         mkIf(site,
              as_bool(regvar),
              mkCopyData(site,
                         mkApply(site, read_reg,
                                 [mkLit(site, '_dev', devtype),
                                  regvar,
                                  reg_table]),
                         val),
              log_statement(site, log_object(site, bank, indices), "error",
                            None, None, "There is no register with number %d",
                            num)),
         return_success(site)])

def codegen_write(impl, indices, inargs, outargs, site):
    bank = impl.parent
    [num, val] = inargs

    if not bank.numbered_registers:
        return mkCompound(site, [return_success(site)])

    reg_table = mkLit(None, '_DML_R_' + bank.name, TPtr(dml_reg_t))
    regmap_table = mkLit(None, '_DML_RN_' + bank.name,
                         regmap_table_type)
    regmap_table_len = mkLit(site, 'ALEN(_DML_RN_%s)' % bank.name,
                             TInt(32, False))

    scope = Symtab(global_scope)
    regvar = mkLocalVariable(site, scope.add_variable(
        'reg',
        site=site,
        type=regmap_table_type,
        init=ExpressionInitializer(
            mkApply(site, find_regnum,
                    [regmap_table, regmap_table_len, num])),
        make_unique=True))

    devtype = node_storage_type(globals.device)
    write_reg = mkLit(None, '_DML_write_reg',
                     TFunction([devtype,
                                regmap_table_type,
                                reg_table_type,
                                TInt(64, False)],
                               TVoid()))

    return mkCompound(
        site,
        [sym_declaration(regvar.sym),
         mkIf(site,
              as_bool(regvar),
              mkExpressionStatement(site,
                                    mkApply(site, write_reg,
                                            [mkLit(site, '_dev', devtype),
                                             regvar,
                                             reg_table,
                                             val])),
              log_statement(site, log_object(site, bank, indices), "error",
                            None, None, "There is no register with number %d",
                            num)),
         return_success(site)])
