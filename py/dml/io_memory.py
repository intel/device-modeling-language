# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import operator
from .ctree import *
from .expr import *
from .expr_util import *
from .types import *
from .logging import report
from .messages import WEXPERIMENTAL_UNMAPPED
from .symtab import *
from .codegen import (require_fully_typed, mark_method_referenced,
                      method_instance, codegen_call_byname, declarations,
                      ReturnFailure)
from . import crep
import dml.globals

# Emit a warning if the hook methods are overridden, as they are
# introduced as an experimental feature, at the moment.   
def check_unmapped_access_handling(bank, isread):
    if isread:
        meth_node = bank.get_component('_unmapped_read_access', 'method')
    else:
        meth_node = bank.get_component('_unmapped_write_access', 'method')

    overridden = meth_node and meth_node.default_method.node
    if overridden:
        report(WEXPERIMENTAL_UNMAPPED(meth_node, meth_node.name))

def unmapped_access(site, bank, idx, scope, isread, overlapping, bigendian,
                    memop, offset, size, writevalue, size2, value2):
    if overlapping:
        # Only pass the first byte of the access to _unmapped_*_access
        if not isread:
            if bigendian:
                writevalue = mkShR(
                    site, writevalue, mkMult(
                        site,
                        mkSubtract(site, size, mkIntegerLiteral(site, 1)),
                        mkIntegerLiteral(site, 8)))
            else:
                writevalue = mkBitAnd(
                    site, writevalue, mkIntegerLiteral(site, 0xff))
        size = mkIntegerLiteral(site, 1)

    scope = Symtab(scope)
    code = []
    success = mkLocalVariable(site, scope.add_variable(
        'success', type=TBool(), site=site,
        init=ExpressionInitializer(mkBoolConstant(site, 0)),
        make_unique=True))

    if isread:
        code.append(codegen_call_byname(
            site,
            bank,
            idx,
            '_unmapped_read_access',
            [memop, offset, size],
            [success, value2]))
    else:
        code.append(codegen_call_byname(
                site,
                bank,
                idx,
                '_unmapped_write_access',
                [memop, offset, size, writevalue],
                [success]))

    code.extend([
        mkCopyData(
            site,
            mkIfExpr(site, success, size, mkIntegerLiteral(site, 0)),
            size2),
        mkReturn(site, mkBoolConstant(site, False))])

    return mkCompound(site, declarations(scope) + code)

# The basic idea of the register dispatcher is to statically
# create a static table with one item for each register instance,
# with a description of the register's location together with a
# callback. Then a binary search for each access, to see whether
# the offset hits a register, and if so, the callback is called.
# The signature of register access callbacks can vary depending on
# array dimensions; in order to get type-correct register tables,
# we group registers by callback signature and perform one table
# with a separate binary search for each such group.
#
# This can be improved in two ways:
#
# 1. We currently use separate register tables for read and write,
#    with identical register mapping information. It would
#    make sense to factor out the binary search to functions
#    shared between read and write.
#
# 2. Previously, there was some logic to avoid linear expansion for
#    register arrays with regular strides. If large array sizes will
#    ever be a problem, then it would be possible to achieve something
#    similar here. Basically, we would scrap the explicit binary
#    search, and instead construct a BSP-like decision tree: For
#    instance, if we have r[i in 0..16][j in 0..8] size 4 @ 47 + $i *
#    100 + $j * 10, then three conditions are necessary and sufficient
#    for a hit (given partial=true): (offset - 47) < 16 * 100, (offset
#    - 47) % 100 < 8 * 10 and (offset - 47) % 10 < 4.  We can extract
#    all such conditions for all register objects, and then create a
#    static binary tree structure where each node is one of these
#    conditions. If we construct the decision tree in a smart way
#    (which is non-trivial to do), then each comparison can subdivide
#    the search space in roughly equal partitions, which gives total
#    time and space O(log(number of register objects)). However, it
#    can happen that we need to make a comparison that holds for some,
#    but not all, elements of a register array. In this case, we need
#    to split the array in two or three sub-arrays, which will
#    increase the size of the decision tree. Also, the comparisons
#    assume that (dimsize * stride) of one dimension never exceeds the
#    stride of the next larger dimension; if this does not hold for an
#    array, we may also need to split the array into parts where the
#    assumption holds.
def codegen_access(bank, bank_indices, isread, memop, offset, size, writevalue,
                   size2, value2, site):
    '''Generate code for _read_one_reg or _write_one_reg.  The
    responsibility for this method is to perform the first bytes of an
    access, accessing up to one register: If offset is inside a
    register, then that register is accessed (unless partial=false and
    the offset is not the register's start offset). If the offset
    is outside registers, then the miss can either be forgiven (by
    _unmapped_{read,write}_access}), or the miss can be propagated to
    the caller by returning 0 in the size2
    parameter. _unmapped_*_access handles either one byte or the
    entire access, depending on whether overlapping=true.
    '''
    assert dml.globals.dml_version == (1, 2)
    if not param_defined(bank, 'byte_order'):
        # For compatibility; see bug 21209
        bigendian = False
    else:
        # dml-builtins.dml checks that it's either "big-endian" or
        # "little-endian"
        bigendian = param_str(bank, 'byte_order') == 'big-endian'
    overlapping = param_bool(bank, 'overlapping')
    partial = param_bool(bank, 'partial')

    # map number of dimensions to list of (reginstance, name of C callback)
    by_dims = {}
    method_name = '_%s_access_%s' % (
        'read' if isread else 'write', 'partial' if partial else 'nopartial')

    for reg in bank.mapped_registers:
        meth_node = reg.node.get_component(method_name, 'method')
        require_fully_typed(site, meth_node)
        func = method_instance(meth_node)
        mark_method_referenced(func)
        cname = func.get_cname()
        by_dims.setdefault(len(reg.layout[0].coord), []).extend(
            (instance, cname) for instance in reg.layout)

    lines = []

    def dim_sort_key(data):
        (rl, _) = data
        return rl.offset
    for dims in sorted(by_dims):
        regs = sorted(by_dims[dims], key=dim_sort_key)
        method_args = memop.ctype().declaration('') + ', '
        if partial:
            method_args += 'uint8, uint8, '
        method_args += 'uint64 *' if isread else 'uint64'
        regvar = 'regs%d' % (dims,)
        lines.append(
            'static const struct { uint64 offset; int size; '
            + ''.join('int idx%d; ' % (i,) for i in range(dims))
            + ' bool (*fun)(%s *, %s%s);} %s[%d] = {' % (
                crep.structtype(dml.globals.device),
                'uint32, ' * (len(bank_indices) + dims),
                method_args, regvar, len(regs)))
        for (rl, cname) in regs:
            lines.append('    {%dull, %d, %s},' % (
                rl.offset, rl.size, ', '.join(list(map(str, rl.coord)) + [cname])))
        lines.append('};')

        lines.extend([
            # Binary search, with a small twist: ends with first ==
            # last+1, where last is the largest element <= offset, or
            # -1. I.e., if partial=true, 'last' is the index of a
            # potentially matching register.
            'for (int first = 0, last = %d; true; ) {' % (len(regs) - 1),
            '    int middle = (first + last) / 2;',
            '    if (%s[middle].offset < offset) {' % (regvar,),
            '        first = middle + 1;',
            '    } else if (%s[middle].offset == offset) {' % (regvar,),
            '        first = middle + 1;',
            '        last = middle;',
            '    } else {',
            '        last = middle - 1;',
            '        if (last == -1)',
            '            break;',
            '    }',
            '    if (first > last) {'])
        indices = (''.join('%s, ' % idx.read() for idx in bank_indices)
                   + ''.join('%s[last].idx%d, ' % (regvar, i)
                             for i in range(dims)))
        # the first byte of the access may hit no other register than
        # regs[last]. Check if it does hit.
        if partial:
            lines.extend([
                '        int64 bytes ='
                + ' %s[last].offset + %s[last].size - offset;'
                % (regvar, regvar),
                '        if (bytes > 0) {'])
            # Truncate access to current register. The caller will take
            # care of the rest of the access (in case of
            # overlapping=false, the caller will cause a miss access that will
            # bypass unmapped_{read,write}_access)
            lines.extend([
                '            if (bytes > %s)' % (size.read(),),
                '                bytes = %s;' % (size.read(),)])
        else:
            lines.extend([
                '        int64 bytes = %s[last].size;' % (regvar,),
                '        if (%s[last].offset == offset && bytes <= %s) {' % (
                regvar, size.read())])
        lines.append(
                '            %s;' % (
            size2.write(mkLit(site, 'bytes', TInt(64, False)))))
        if partial:
            if bigendian:
                lines.extend([
                    '            int msb1 = '
                    + '(%s[last].offset - offset + %s[last].size) * 8;' % (
                        regvar, regvar),
                    '            int lsb = msb1 - bytes * 8;'])
            else:
                lines.extend([
                    '            int lsb = (offset - %s[last].offset) * 8;' % (
                        regvar),
                    '            int msb1 = lsb + bytes * 8;'])
            bytepos_args = ', msb1, lsb' if partial else ''
        else:
            bytepos_args = ''

        # Hit.
        if isread:
            lines.extend([
                '            uint64 val = 0;',
                '            bool ret = %s[last].fun(_dev, %s%s%s, &val);' % (
                    regvar, indices, memop.read(), bytepos_args),
                '            if (ret) return true;',
                '            %s;' % (
                    value2.write(mkLit(site, 'val', TInt(64, False)))),
                '            return false;'])
        else:
            # Shifting/masking can normally be skipped in banks with
            # overlapping=false. But if we do skip the
            # shifting/masking, then we will run into bug 24344 the
            # day an overlapping access is actually performed.
            # Maybe it would make sense to let overlapping violations
            # trigger a miss before performing an access.
            if bigendian:
                value = '%s >> ((%s - bytes) * 8)' % (
                    writevalue.read(), size.read())
            else:
                value = '%s & (DML_shlu(1, bytes * 8) - 1)' % (writevalue.read())
            lines.append(
                '            return %s[last].fun(_dev, %s%s%s, %s);' % (
                    regvar, indices, memop.read(), bytepos_args, value))
        lines.append('        }')
        # Miss.
        if not partial:
            # unmapped_access can not forgive a partial access.
            # (maybe it should?)
            lines.extend([
                '        if (offset >= %s[last].offset' % (regvar,)
                + ' && offset < %s[last].offset + %s[last].size) {'
                % (regvar, regvar),
                '            %s;' % (size2.write(mkIntegerLiteral(site, 0)),),
                '            return false;',
                '        }'])
        lines.extend([
            '        break;',
            '    }',
            '}'])

    scope = Symtab(global_scope)
    code = [mkInline(site, line) for line in lines]
    check_unmapped_access_handling(bank, isread)
    with ReturnFailure(site):
        code.append(unmapped_access(site, bank, bank_indices, scope, isread,
                                    overlapping, bigendian, memop,
                                    offset, size, writevalue, size2, value2))

    return mkCompound(
        site, declarations(scope) + code)

def codegen_write_access(bank, idx, inargs, outargs, site):
    (memop, offset, size, write_value) = inargs
    (consumed_size,) = outargs
    return codegen_access(bank, idx, False, memop, offset, size,
                           write_value, consumed_size, None, site)

def codegen_read_access(bank, idx, inargs, outargs, site):
    (memop, offset, access_size) = inargs
    (consumed_size, read_value) = outargs
    return codegen_access(bank, idx, True, memop, offset, access_size,
                           None, consumed_size, read_value, site)
