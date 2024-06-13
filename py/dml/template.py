# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

# Process templates, from ASTs to Template objects

import os
import functools
from . import ast, logging
from . import compat
from .logging import *
from .messages import *
from .set import Set
import dml.globals
import dml.traits

__all__ = (
    'Rank',
    'RankDesc',
    'ObjectSpec',
    'InstantiatedTemplateSpec',
    'process_templates',
)

class RankDesc(object):
    '''Description of a rank. Its purpose is to help identifying how to
    assign some other declaration a superior rank.'''
    def __init__(self, kind, text, in_eachs=()):
        assert kind in {'file', 'template', 'verbatim'}
        self.text = text
        self.kind = kind
        # list of nested 'in each' blocks within a template or file,
        # innermost first
        self.in_eachs = in_eachs

    def __str__(self):
        if self.kind == 'file':
            desc = 'file %s' % (self.text,)
        elif self.kind == 'template':
            desc = 'template %s' % (self.text,)
        else:
            assert self.kind == 'verbatim'
            desc = self.text
        for templates in self.in_eachs:
            desc = "'in each (%s)' block from %s" % (', '.join(templates), desc)
        return desc

class Rank(object):
    '''Each ObjectSpec is associated with a Rank when instantiated. The
    Rank objects of two ObjectSpec:s determine the override order of
    methods and parameters.'''
    __slots__ = ('inferior', 'desc')
    def __init__(self, inferior, desc):
        # set of Rank. If a parameter or method is declared in two
        # ObjectSpec objects, then one ObjectSpec takes precedence if
        # its Rank's inferior set contains the other ObjectSpec's
        # rank.
        self.inferior = inferior.union(*(i.inferior for i in inferior))
        assert isinstance(desc, RankDesc)
        self.desc = desc

    def __repr__(self):
        return "Rank(%d, %s)" % (len(self.inferior), self.desc)

class ObjectSpec(object):
    '''Partial specification of a DML object. Basically the AST of an object
    declaration block, but slightly post-processed.'''
    __slots__ = ('site', 'rank', 'templates', 'in_eachs', 'params', 'blocks')
    def __init__(self, site, rank, templates, in_eachs, params, blocks):
        self.site = site
        # Rank object
        self.rank = rank
        # list of (site, Template), for instantiated templates
        self.templates = templates
        # list of (name, ObjectSpec)
        self.in_eachs = in_eachs
        # list of ast.param
        self.params = params
        # list of tuples (preconds, shallow-stmts, composite-stmts),
        # where preconds is a (possibly empty) list of expression
        # ASTs, and shallow-stmts is a list of ast.session, ast.method
        # or ast.error objects, and composite-stmts is a list of
        # tuples (objkind, name, arrayinfo, ObjectSpec list),
        # one for each declaration of a subobject of composite
        # kind. shallow-stmts and composite-stmts are included in the
        # object if all exprs, evaluated in the object's scope, yield
        # true.
        self.blocks = blocks

    def __repr__(self):
        # A full repr is way too verbose; instead print a shorter form that
        # usually can uniquely identify a spec
        return 'ObjectSpec(%r, [%s], [%s])' % (
            self.rank, ','.join(t.name for (_, t) in self.templates),
            ','.join(param.args[0] for param in self.params))

    def subobjspecs(self):
        '''Return all ObjectSpec:s defined in this spec, including recursively
        in subobjects, but excluding instantiated templates'''
        return [subspec
                for (_, _, subobjs) in self.blocks
                for (_, _, _, spec) in subobjs
                for subspec in [spec] + spec.subobjspecs()]

    def defined_symbols(self):
        '''Return a dictionary of all symbols defined, conditionally or
        unconditionally, in this block. The dictionary maps symbol to a pair
        (kind, site)'''
        symbols = {}
        for (_, t) in self.templates:
            for (sym, val) in list(t.spec.defined_symbols().items()):
                symbols[sym] = val
        for p in self.params:
            symbols[p.args[0]] = ('param', p.site)
        for (_, shallow, composite) in self.blocks:
            for sub in shallow:
                if sub.kind in {'method'}:
                    symbols[sub.args[0]] = (sub.kind, sub.site)
                elif sub.kind in {'session', 'saved'}:
                    for decl_ast in sub.args[0]:
                        (name, _) = decl_ast.args
                        symbols[name] = (sub.kind, sub.site)
                elif sub.kind == 'hook':
                    symbols[sub.args[0]] = (sub.kind, sub.site)
                else:
                    assert sub.kind == 'error'
            for (_, name, _, specs) in composite:
                symbols[name] = ('subobj', specs.site)
        return symbols

class InstantiatedTemplateSpec(ObjectSpec):
    '''The return value of `wrap_sites`, represents the object spec of
    a particular instantiation of a template'''
    __slots__ = ('parent_template',)
    def __init__(self, parent_template, site, rank, templates, in_eachs,
                 params, blocks):
        self.parent_template = parent_template
        super().__init__(site, rank, templates, in_eachs, params, blocks)

class Template(object):
    def __init__(self, name, trait, spec):
        self.name = name
        # Trait instance, or None
        self.trait = trait
        # ObjectSpec instance
        self.spec = spec

    def __repr__(self):
        return 'Template(%r)' % (self.name,)
    @property
    def site(self): return self.spec.site
    def traits(self):
        '''Return a set of all traits implemented by this template'''
        if self.trait:
            return Set((self.trait, *self.trait.ancestors))
        else:
            return Set().union(
                *[sup.traits() for (_, sup) in self.spec.templates])

    @functools.cache
    def get_potential_method_impl_details(self, method_name: str) -> tuple[
            bool, tuple['Template', ...]]:
        '''Provides details about which among this template and its ancestors
        may provide the highest-rank implementation of the specified method
        (when not considering any rank higher than that of the template.)

        Returns a tuple `(provides_impl, next_candidates)`:
        * `provides_impl`: Signifies that the current template itself specifies
          an implementation of the method (which may be conditionally provided)
        * `next_candidates`: A tuple of all hierarchically unrelated ancestor
          templates that specify (and thus may provide) a highest-rank
          implementation of the named method when excluding the current
          template. If the current template unconditionally provides an
          implementation, this tuple will be empty.

        Example return values of `t.get_potential_method_impl_details(m)` and
        their meaning:
        * `(True, ())`
          `t` specifies an implementation of `m`, and no ancestor template
          specifies an implementation of `m` that may possibly be used
          instead.
        * `(False, ())``
          Neither `t` nor its ancestor templates have a specification of a
          method `m`.
        * `(True, (next_t1, next_t2))`
          `t` specifies an implementation of a method `m`, which, if provided
          to the object instantiating the template, would override the
          next-highest rank implementations, which are specified by `next_t1`
          and `next_t2`.
        * `(False, (next_t1, next_t2))`
          The highest-rank implementation specifications of the method `m` are
          given by the (unrelated) ancestor templates `next_t1` and `next_t2`.
        '''
        self_is_candidate = False
        for (preconds, shallow, composite) in self.spec.blocks:
            for sub in shallow:
                if sub.args[0] == method_name:
                    if not preconds:
                        return (sub.kind == 'method', ())
                    elif sub.kind == 'method':
                        self_is_candidate = True
        rank_to_candidate = {}

        for (_, tmpl) in self.spec.templates:
            (provides_impl,
             sub_next_candidates) = tmpl.get_potential_method_impl_details(
                 method_name)
            if provides_impl:
                rank_to_candidate[tmpl.spec.rank] = tmpl
            else:
                rank_to_candidate.update((ancestor.spec.rank, ancestor)
                                         for ancestor in sub_next_candidates)

        highest_ranks = dml.traits.get_highest_ranks(Set(rank_to_candidate))

        next_candidates = tuple(rank_to_candidate[r] for r in highest_ranks)

        return (self_is_candidate, next_candidates)

def flatten_ifs(stmts, preconds):
    '''Given a sequence of {if, method, session, object, error, export} nodes,
    recursively flatten all ifs and return a list of (preconditions,
    simple, composite) tuples, where preconditions is a list of
    expression ASTs, simple is a list of method/session/error/export ASTs,
    and composite is a list of object ASTs.'''
    result = []
    simple = []
    composite = []
    for stmt in stmts:
        if stmt.kind == 'hashif':
            (cond, t, f) = stmt.args
            result.extend(flatten_ifs(t, preconds + [cond]))
            if f:
                neg = ast.unop(cond.site, '!', cond)
                result.extend(flatten_ifs(f, preconds + [neg]))
            if logging.show_porting:
                if t:
                    PWUNUSED.positive_conds.add(cond)
                if f:
                    PWUNUSED.negative_conds.add(neg)
        elif stmt.kind == 'object':
            composite.append(stmt)
        else:
            if stmt.kind not in {'method', 'session', 'saved',
                                 'error', 'export', 'hook'}:
                raise ICE(stmt.site, 'unexpected AST kind %s' % (stmt.kind,))
            simple.append(stmt)
    result.append((preconds, simple, composite))
    return result

def object_spec_from_asts(site, stmts, templates, inferior, in_each_structure,
                          desc):
    # Recursively create specs for all 'in each' statements
    # first. This must be done first, because their ranks are inferior.
    in_each_specs = {}
    for (in_each_ast, (sub_inferior, sub_in_eachs)) in (
            in_each_structure.items()):
        (names, subasts) = in_each_ast.args
        in_each_specs[in_each_ast] = object_spec_from_asts(
            in_each_ast.site, subasts, templates, sub_inferior, sub_in_eachs,
            RankDesc(desc.kind, desc.text, (names,) + desc.in_eachs))
    inferior_ranks = {templates[name].spec.rank for name in inferior}
    inferior_ranks.update(spec.rank for spec in list(in_each_specs.values()))
    rank = Rank(inferior_ranks, desc)

    def obj_from_asts(site, stmts):
        # list of parameter statement ASTs
        params = []
        # list of pairs (site, Template)
        is_stmts = []
        in_eachs = []
        rest = []
        for stmt in stmts:
            if stmt.kind == 'param':
                params.append(stmt)
            elif stmt.kind == 'is':
                (template_refs,) = stmt.args
                if logging.show_porting:
                    template_renames = {'unimplemented': 'unimpl',
                                        'silent_unimplemented': 'silent_unimpl',
                                        '_read_unimplemented': 'read_unimpl',
                                        '_write_unimplemented': 'write_unimpl',}
                    for (issite, name) in template_refs:
                        if name in template_renames:
                            report(PRENAME_TEMPLATE(issite, name,
                                                    template_renames[name]))
                is_stmts.extend([(issite, templates[name])
                                 for (issite, name) in template_refs])
            elif stmt.kind == 'in_each':
                (names, stmts) = stmt.args
                in_eachs.append(([templates[name] for name in names],
                                 in_each_specs[stmt]))
            else:
                rest.append(stmt)
        blocks = []
        for (preconds, shallow, composite) in flatten_ifs(rest, []):
            # The 'composite' list returned by flatten_ifs is just a
            # list of object ASTs; recursively transform those into
            # ObjectSpec objects
            block = []
            for decl_ast in composite:
                assert decl_ast.kind == 'object'
                (name, objtype, indices, sub_stmts) = decl_ast.args
                spec = obj_from_asts(
                    decl_ast.site, sub_stmts + [ast.is_(
                        decl_ast.site, [(decl_ast.site, objtype)])])
                block.append((objtype, name, indices, spec))
            blocks.append((preconds, shallow, block))
        return ObjectSpec(site, rank, is_stmts, in_eachs, params, blocks)
    return obj_from_asts(site, stmts)

def rank_structure(asts):
    '''Given an object declaration, given as a list of ast.AST, analyze
    its structure and return a tuple (inferior, unconditional,
    in_each_structure). 'inferior' is a flat dict, mapping name of
    inferior template, to a statement referencing the template.
    The dict includes all references recursively, including in subobjects and
    'in each' statements. 'in_each_structure' is a nested dict, showing the
    hierarchy of template instantiation and 'in each' statements. Keys
    are in_each ASTs, and values are corresponding pairs (inferior,
    in_each_structure) defined recursively on the same form.

    'unconditional' is the set of template references that are not
    conditioned with an #if block. References to such templates are
    permitted as long as that #if block is dead; this allows common code to
    conditionally instantiate a template.
    '''
    inferior = {}
    unconditional = Set()
    in_each_structure = {}
    queue = [(ast, False) for ast in asts]
    while queue:
        (spec, conditional) = queue.pop()
        if spec.kind == 'object':
            (_, objtype, _, stmts) = spec.args
            inferior[objtype] = spec
            queue.extend((stmt, conditional) for stmt in stmts)
        elif spec.kind == 'is':
            (template_refs,) = spec.args
            for (_, name) in template_refs:
                inferior[name] = spec
                if not conditional:
                    unconditional.add(name)
        elif spec.kind == 'in_each':
            (names, stmts) = spec.args
            (sub_inferior, sub_uncond, sub_structure) = rank_structure(stmts)
            for name in names:
                sub_inferior[name] = spec
            in_each_structure[spec] = (sub_inferior, sub_structure)
            inferior.update(sub_inferior)
            if not conditional:
                unconditional.update(names)
                unconditional.update(sub_uncond)
        elif spec.kind == 'hashif':
            (_, t, f) = spec.args
            queue.extend((s, True) for s in t)
            queue.extend((s, True) for s in f)
        else:
            assert spec.kind in {'error', 'method', 'param',
                                 'session', 'saved', 'export', 'hook'}
    return (inferior, unconditional, in_each_structure)

def process_templates(template_decls):
    # Report and filter out any attempts to use nonexisting traits.
    # Also, figure out the inheritance relation between
    # templates. Note that a template P is considered a parent of
    # another template C even if C declares a subobject that instantiates P.

    # name -> list of parent names
    required_templates = {}
    template_rank_structure = {}
    for (name, (_, asts, _)) in list(template_decls.items()):
        (references, uncond_refs, in_each_structure) = rank_structure(asts)
        template_rank_structure[name] = (references, in_each_structure)
        referenced = Set(references)
        required_templates[name] = referenced
        all_missing = referenced.difference(template_decls)
        if all_missing:
            # fallback: add missing templates and retry
            for missing in all_missing:
                site = references[missing].site
                if (missing not in uncond_refs
                    or compat.dml12_misc in dml.globals.enabled_compat):
                    # delay error until template instantiation
                    dml.globals.missing_templates.add(missing)
                else:
                    report(ENTMPL(site, missing))
                template_decls[missing] = (site, [], None)
            return process_templates(template_decls)
    try:
        template_order = dml.topsort.topsort(required_templates)
    except dml.topsort.CycleFound as e:
        is_sites = []
        # find the sites of the 'is' statements that give a cycle
        for (c, p) in zip(e.cycle, e.cycle[1:] + [e.cycle[0]]):
            (_, asts, _) = template_decls[c]
            (ref_asts, _, _) = rank_structure(asts)
            is_sites.append(ref_asts[p].site)
        if any(name.startswith('@') for name in e.cycle):
            report(ECYCLICIMP(is_sites))
        else:
            report(ECYCLICTEMPLATE(is_sites))
        for name in e.cycle:
            # prune the templates that created a cycle
            (site, _, _) = template_decls[name]
            template_decls[name] = (site, [], None)
        return process_templates(template_decls)

    # The generation of struct definitions assumes the dictionary to
    # be topologically ordered on inheritance: if A inherits B, B
    # appears first in the dict.
    traits = {}

    # name -> Template
    templates = {}
    for name in template_order:
        (site, asts, trait_stmts) = template_decls[name]
        (references, in_each_structure) = template_rank_structure[name]
        spec = object_spec_from_asts(
            site, asts, templates, references, in_each_structure,
            RankDesc('file', os.path.basename(name[1:])) if name.startswith('@')
            else RankDesc('template', name))
        if trait_stmts is None:
            trait = None
        else:
            trait = dml.traits.process_trait(
                spec.site, name, trait_stmts,
                Set().union(
                    *[tpl.traits() for (_, tpl) in spec.templates]),
                spec.defined_symbols())
            traits[name] = trait
        templates[name] = Template(name, trait, spec)
    return (templates, traits)
