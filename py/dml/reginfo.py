# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

from collections import namedtuple
import operator
import itertools
from functools import reduce
import dml.globals
from . import logging
from .ctree import *
from .logging import *
from .messages import *
from .expr_util import *

__all__ = ('explode_registers',)

class RegInstance(object):
    __slots__ = ('coord', 'offset', 'size')
    def __init__(self, coord, offset, size):
        self.coord = coord
        self.offset = offset
        self.size = size

class RegInfo(object):
    __slots__ = (
        'node',   # the DMLObject
        'layout', # a list of RegInstance objects
        'dimsizes' # the sizes of the dimensions, if this is all
                   # instances of a register. None otherwise
        )
    def __init__(self, node, layout):
        self.node = node
        self.layout = layout
        dimsizes = node.dimsizes[len(node.dimsizes) - len(layout[0].coord):]
        self.dimsizes = (
            dimsizes
            if len(layout) == reduce(operator.mul, dimsizes, 1)
            else None)

    def node_instances(self):
        '''Return an iterable of all register instances represented by this
        RegInfo instance, possibly in compressed form: The returned value is a
        list of tuples (node, indices, dimsizes). 'node' is a register
        node, which is indexed, relative to the containing
        bank. 'indices' and 'dimsizes' show which register indices are
        used: A nonempty indices tuple means a single register
        instance using the given (bank-relative) index expressions,
        and a non-empty dimsizes means that the item represents
        multiple instances, one for each possible coordinate in
        dimsizes. Examples of return values:

        # r0 is a non-array
        [(r0, (), ())]
        # r1 is a two-dimensional register array, but the partition only
        # contains r1[3][1] and r1[4][5]. Can happen if register offset
        # is undefined for some, but not all, indices.
        [(r1, (mkIntegerLiteral(3), mkIntegerLiteral(1)), ()),
         (r1, (mkIntegerLiteral(4), mkIntegerLiteral(5)), ())]
        # r2 is a two-dimensional register array, and the partition contains
        # all 6 instances
        [(r2, (), (3, 2))]

        Note that
        node.dimensions == bank.dimensions + len(indices) + len(dimsizes).
        '''
        if self.dimsizes is None:
            return ((self.node, tuple(mkIntegerLiteral(self.node.site, i)
                                      for i in instance.coord), ())
                    for instance in self.layout)
        else:
            return [(self.node, (), self.dimsizes)]


def all_registers(parent):
    """Return all registers in a bank as a sequence of triples of
    RegInfo objects.  The elements in the triples represent mapped,
    unmapped, and numbered registers."""
    # the call order is considered to be undefined, but may affect
    # whether code works or not (e.g., bug 24345). So we pick an order
    # that is deterministic, platform-independent, and unaffected by
    # adding or removing objects.
    nodes = sorted(parent.get_components('group', 'register'),
                   key=operator.attrgetter('name'))
    regs = []
    while nodes:
        node = nodes.pop(0)

        if node.objtype == 'group':
            regs.extend(all_registers(node))
        else:
            assert node.objtype == 'register'
            regs.append(explode_register(node))
    return regs

def explode_register(node):
    """Return a triple of RegInfo objects for a register node.

    The elements are (MAPPED, UNMAPPED, NUMBERED) and represent the
    mapped instances, the unmapped instances, and the numbered
    instances, respectively.  Either element may be None."""

    mapped_layout = []
    unmapped_layout = []
    numbered_layout = []

    # dimsizes is a list of array sizes
    dimsizes = ()
    n = node

    while n and n.objtype != 'bank':
        if n.isindexed():
            dimsizes = n.arraylens() + dimsizes
        n = n.parent

    assert n.objtype == 'bank'
    assert len(dimsizes) == node.dimensions - n.dimensions

    if logging.show_porting:
        try:
            indices = static_indices(node, param_expr_site(node, 'offset'))
            offset = param_expr(node, 'offset', indices)
            if offset.undefined:
                report(PUNDEFOFFS(offset.site))
        except EIDXVAR:
            pass
    for indices in itertools.product(*(
            (mkIntegerLiteral(node.site, idx) for idx in range(dimsize))
            for dimsize in dimsizes)):
        coord = tuple(i.value for i in indices)
        roffset, rsize, regnum = one_register(node, indices, n)
        inst = RegInstance(coord, roffset, rsize)
        if roffset is not None:
            mapped_layout.append(inst)
        else:
            unmapped_layout.append(inst)
        if regnum is not None:
            numbered_layout.append(RegInstance(coord, regnum, 1))

    if mapped_layout:
        mapped = RegInfo(node, mapped_layout)
    else:
        mapped = None

    if unmapped_layout:
        unmapped = RegInfo(node, unmapped_layout)
    else:
        unmapped = None

    if numbered_layout:
        numbered = RegInfo(node, numbered_layout)
    else:
        numbered = None

    return mapped, unmapped, numbered

def one_register(node, indices, bank):
    bank_indices = static_indices(bank, param_expr_site(node, 'offset'))
    try:
        if dml.globals.dml_version == (1, 2):
            roffset = param_expr(node, 'offset', bank_indices + indices)
            roffset = expr_intval(roffset) if defined(roffset) else None
            regnum = param_expr(node, 'regnum', bank_indices + indices)
            regnum = expr_intval(regnum) if defined(regnum) else None
        else:
            roffset = param_int(node, 'offset', indices=bank_indices + indices)
            # in 1.4, we use the magic constant unmapped_offset to denote
            # unmapped registers
            if roffset == 0xffffffffffffffff:
                roffset = None
            # the regnum param is specific to DML 1.2
            regnum = None

        rsize = param_int(node, 'size')

        # roffset is undefined for unmapped registers
        if roffset and roffset < 0:
            report(WNEGOFFS(param_expr_site(node, 'offset'), roffset))
            roffset &= 0xffffffffffffffff

        return (roffset, rsize, regnum)
    except DMLError as e:
        report(e)
        return (None, 4, None)

def check_overlap(regs):
    instances = sorted((ri for reg in regs for ri in reg.layout),
                       key=lambda ri: ri.offset)
    for (ri1, ri2) in zip(instances, instances[1:]):
        if ri2.offset < ri1.offset + ri1.size:
            [[node1], [node2]] = [
                [reg.node for reg in regs if ri in reg.layout]
                for ri in [ri1, ri2]]
            report(EREGOL(node1, node2, ri1.coord, ri2.coord))

def explode_registers(bank):
    """Expand all registers of a bank, and calculate the offsets of every
    element of register arrays. Return three arrays of RegInfo
    objects, containing register instances with a defined offset
    parameter, with undefined offset parameter, and with defined
    regnum parameter, respectively."""

    mapped = []
    unmapped = []
    numbered = []
    for mreg, ureg, nreg in all_registers(bank):
        if ureg:
            unmapped.append(ureg)
        if nreg:
            numbered.append(nreg)
        if mreg:
            mapped.append(mreg)

    check_overlap(mapped)
    check_overlap(numbered)

    return mapped, unmapped, numbered
