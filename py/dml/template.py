# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

# Process templates, from ASTs to Template objects

import os
import functools
from . import ast, logging
from . import breaking_changes
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
    __slots__ = ('site', 'rank', 'templates', 'params', 'blocks')
    def __init__(self, site, rank, templates, params, blocks):
        self.site = site
        # Rank object
        self.rank = rank
        # list of (site, Template), for instantiated templates
        self.templates = templates
        # list of ast.param
        self.params = params
        # list of tuples (preconds, shallow-stmts, composite-stmts, in-eachs),
        # where preconds is a (possibly empty) list of expression
        # ASTs, and shallow-stmts is a list of ast.session, ast.method
        # or ast.error objects, composite-stmts is a list of tuples
        # (objkind, name, arrayinfo, ObjectSpec list) each representing a
        # composite subobject declaration, and in-eachs is a list of tuples
        # (target list of Template:s, ObjectSpec of in-each block)
        # representing the `in each` declarations made directly within the
        # block.
        # The declarations that in-eachs, shallow-stmts, and composite-stmts
        # represent are only included in the object if all exprs of preconds,
        # evaluated in the object's scope, yield true.
        self.blocks = blocks

    def __repr__(self):
        # A full repr is way too verbose; instead print a shorter form that
        # usually can uniquely identify a spec
        return 'ObjectSpec(%r, [%s], [%s])' % (
            self.rank, ','.join(t.name for (_, t) in self.templates),
            ','.join(param.args[0] for param in self.params))

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
        for (_, shallow, composite, _) in self.blocks:
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
            for (_, name, _, _, specs) in composite:
                symbols[name] = ('subobj', specs.site)
        return symbols

class InstantiatedTemplateSpec(ObjectSpec):
    '''The return value of `wrap_sites`, represents the object spec of
    a particular instantiation of a template'''
    __slots__ = ('parent_template',)
    def __init__(self, parent_template, site, rank, templates, params, blocks):
        self.parent_template = parent_template
        super().__init__(site, rank, templates, params, blocks)

class Template(object):
    def __init__(self, name, trait, spec, shared_objects, objtype=None):
        self.name = name
        # Trait instance, or None
        self.trait = trait
        # ObjectSpec instance
        self.spec = spec
        # name -> Template, for each `shared <objtype>` declaration
        self.shared_objects = shared_objects
        # the object type, if this is the template of a shared object
        self.objtype = objtype

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
        for (preconds, shallow, composite, in_eachs) in self.spec.blocks:
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

def flatten_ifs(in_each_specs, templates, stmts, preconds):
    '''Given a sequence of {if, method, session, object, error, export, in-each} nodes,
    recursively flatten all ifs and return a list of (preconditions,
    simple, composite, in_each) tuples, where preconditions is a list of
    expression ASTs, simple is a list of method/session/error/export ASTs,
    composite is a list of object ASTs, and in_each is list of two-tuples
    (target list of Template:s, ObjectSpec of in-each block)'''
    result = []
    simple = []
    composite = []
    in_eachs = []
    for stmt in stmts:
        if stmt.kind == 'hashif':
            (cond, t, f) = stmt.args
            result.extend(flatten_ifs(in_each_specs, templates,
                                      t, preconds + [cond]))
            if f:
                neg = ast.unop(cond.site, '!', cond)
                result.extend(flatten_ifs(in_each_specs, templates,
                                          f, preconds + [neg]))
            if logging.show_porting:
                if t:
                    PWUNUSED.positive_conds.add(cond)
                if f:
                    PWUNUSED.negative_conds.add(neg)
        elif stmt.kind in {'object', 'sharedobject'}:
            composite.append(stmt)
        elif stmt.kind == 'in_each':
            (names, _) = stmt.args
            in_eachs.append(([templates[name] for name in names],
                             in_each_specs[stmt]))
        else:
            if stmt.kind not in {'method', 'session', 'saved',
                                 'error', 'export', 'hook'}:
                raise ICE(stmt.site, 'unexpected AST kind %s' % (stmt.kind,))
            simple.append(stmt)
    result.append((preconds, simple, composite, in_eachs))
    return result

def split_template_body(body, creates_trait):
    '''Split a template body into the part that describes objects and the
    part that describes the template's trait.'''
    template_body = []
    trait_body = []
    for tstmt in body:
        if tstmt.kind == 'sharedmethod':
            trait_body.append(tstmt)
        elif tstmt.kind == 'param':
            (_, type_info, _, value) = tstmt.args
            if (type_info is not None
                and type_info.kind == 'paramtype'):
                trait_body.append(tstmt)
                # the form "param x: int = value;" has
                # aspects of both trait and template,
                # and the form "param x: int;" has some effect
                # when explicit_param_decls is enabled
                template_body.append(tstmt)
            else:
                template_body.append(tstmt)
        elif tstmt.kind in {'session', 'saved'}:
            template_body.append(tstmt)
            if creates_trait:
                trait_body.append(tstmt)
        elif tstmt.kind == 'sharedhook':
            template_body.append(tstmt.args[0])
            trait_body.append(tstmt.args[0])
        else:
            template_body.append(tstmt)
    return (template_body, trait_body)

def template_trait(name, spec, trait_stmts, shared_objects,
                   conditional_specs):
    '''`conditional_specs` are specs whose symbols are reserved in the
    trait without being part of `spec`'''
    if trait_stmts is None:
        return None
    symbols = {}
    for cond_spec in conditional_specs:
        symbols.update(cond_spec.defined_symbols())
    symbols.update(spec.defined_symbols())
    return dml.traits.process_trait(
        spec.site, name, trait_stmts,
        {objname: tpl.trait for (objname, tpl) in shared_objects.items()},
        Set().union(*[tpl.traits() for (_, tpl) in spec.templates]),
        symbols)

def ancestors(is_stmts):
    result = set()
    for (_, tpl) in is_stmts:
        result.add(tpl)
        result.update(ancestors(tpl.spec.templates))
    return result

def object_spec_from_asts(site, stmts, templates, inferior, in_each_structure,
                          desc, tname=None):
    '''Return (ObjectSpec, dict name -> Template). If `stmts` is the body
    of template `tname`, then the dict holds the implicit template of each
    `shared` object declaration in it.'''
    # Recursively create specs for all 'in each' statements
    # first. This must be done first, because their ranks are inferior.
    in_each_specs = {}
    for (in_each_ast, (sub_inferior, sub_in_eachs)) in (
            in_each_structure.items()):
        (names, subasts) = in_each_ast.args
        (in_each_specs[in_each_ast], _) = object_spec_from_asts(
            in_each_ast.site, subasts, templates, sub_inferior, sub_in_eachs,
            RankDesc(desc.kind, desc.text, (names,) + desc.in_eachs))
    inferior_ranks = {templates[name].spec.rank for name in inferior}
    inferior_ranks.update(spec.rank for spec in list(in_each_specs.values()))
    rank = Rank(inferior_ranks, desc)

    def shared_template(tname, objname, objtype, site, stmts,
                        conditional_specs, enclosing_is_stmts):
        '''Create the implicit template of a `shared` object declaration.
        It gets the rank of the enclosing template, just like the
        declaration of an ordinary subobject. `conditional_specs` are the
        specs of conditional declarations of the same object.'''
        (body, trait_stmts) = split_template_body(stmts, True)
        # Shared members are resolved through trait inheritance rather
        # than by rank, so the implicit template must inherit those of the
        # same object in inherited templates
        inherited = sorted(
            (a.shared_objects[objname] for a in ancestors(enclosing_is_stmts)
             if objname in a.shared_objects),
            key=lambda tpl: tpl.name)
        (spec, shared_objects) = obj_from_asts(
            site, body, tname,
            [(site, templates[objtype])] + [(site, tpl) for tpl in inherited])
        return Template(
            tname, template_trait(tname, spec, trait_stmts, shared_objects,
                                  conditional_specs),
            spec, shared_objects, objtype)

    def obj_from_asts(site, stmts, tname, instantiated=()):
        '''`instantiated` is a list of (site, Template) for templates
        instantiated in addition to the `is` statements in `stmts`'''
        # list of parameter statement ASTs
        params = []
        # list of pairs (site, Template)
        is_stmts = []
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
            else:
                rest.append(stmt)
        is_stmts.extend(instantiated)
        def decl_spec(site, objtype, sub_stmts, sub_instantiated):
            (spec, _) = obj_from_asts(
                site, sub_stmts + [ast.is_(site, [(site, objtype)])], None,
                sub_instantiated)
            return spec
        # As soon as one declaration of an object is shared, the bodies of
        # all its unconditional declarations in this template body form the
        # implicit template. A condition around a declaration is evaluated
        # in the enclosing scope, so it cannot be moved into the implicit
        # template; a conditional declaration only extends the object, but
        # its names are reserved in the implicit template.
        # The unconditional declarations are therefore set aside until all
        # conditional ones are done.
        #
        # Object declarations are partitioned by name:
        # - conditional_specs: specs of conditional declarations, built at once
        # - unconditional_decls: unconditional declarations, built afterwards
        # - shared_names: names with a shared declaration, which makes their
        #   unconditional declarations the implicit template
        conditional_specs: dict[str, list[ObjectSpec]] = {}
        unconditional_decls: dict[str, list[ast.AST]] = {}
        shared_names: set[str] = set()
        blocks = []
        for (preconds, shallow, composite, in_each) in flatten_ifs(
                in_each_specs, templates, rest, []):
            # The 'composite' list returned by flatten_ifs is just a
            # list of object ASTs; recursively transform those into
            # ObjectSpec objects
            block = []
            if preconds:
                for decl_ast in composite:
                    # shared declarations cannot be conditional
                    assert decl_ast.kind == 'object'
                    (name, objtype, indices, is_extension,
                     sub_stmts) = decl_ast.args
                    spec = decl_spec(decl_ast.site, objtype, sub_stmts, [])
                    conditional_specs.setdefault(name, []).append(spec)
                    block.append((objtype, name, indices, is_extension, spec))
            else:
                unconditional_block = block
                for decl_ast in composite:
                    if decl_ast.kind == 'sharedobject':
                        [decl_ast] = decl_ast.args
                        shared_names.add(decl_ast.args[0])
                    unconditional_decls.setdefault(
                        decl_ast.args[0], []).append(decl_ast)
            blocks.append((preconds, shallow, block, in_each))
        # An inherited shared object without a most specific declaration,
        # e.g. in a diamond, is declared implicitly, as if by
        # `shared <objtype> name;`
        implicit = []
        if tname is not None:
            inherited = dml.traits.inherited_subobjs(
                Set().union(*[tpl.traits() for (_, tpl) in is_stmts]))
            implicit = [name for (name, decls) in inherited.items()
                        if len(decls) > 1 and name not in shared_names]
            shared_names.update(implicit)
        shared_objects: dict[str, Template] = {}
        for (name, decls) in unconditional_decls.items():
            if name in shared_names:
                (first, *rest_decls) = decls
                tpl = shared_template(
                    f'{tname}.{name}', name, first.args[1], first.site,
                    [stmt for d in decls for stmt in d.args[4]],
                    conditional_specs.get(name, []), is_stmts)
                shared_objects[name] = tpl
                specs = ([decl_spec(first.site, first.args[1], [],
                                    [(first.site, tpl)])]
                         + [decl_spec(d.site, d.args[1], [], [])
                            for d in rest_decls])
            else:
                specs = [decl_spec(d.site, d.args[1], d.args[4], [])
                         for d in decls]
            for (d, spec) in zip(decls, specs):
                (_, objtype, indices, is_extension, _) = d.args
                unconditional_block.append(
                    (objtype, name, indices, is_extension, spec))
        for name in implicit:
            if name in unconditional_decls:
                continue
            objtype = next(
                a.shared_objects[name].objtype
                for a in sorted(ancestors(is_stmts), key=lambda t: t.name)
                if name in a.shared_objects)
            tpl = shared_template(f'{tname}.{name}', name, objtype, site, [],
                                  conditional_specs.get(name, []), is_stmts)
            shared_objects[name] = tpl
            unconditional_block.append(
                (objtype, name, [], None,
                 decl_spec(site, objtype, [], [(site, tpl)])))
        return (ObjectSpec(site, rank, is_stmts, params, blocks),
                shared_objects)
    return obj_from_asts(site, stmts, tname)

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
            (_, objtype, _, _, stmts) = spec.args
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
        elif spec.kind == 'sharedobject':
            [obj] = spec.args
            (_, objtype, _, _, stmts) = obj.args
            inferior[objtype] = spec
            (body, _) = split_template_body(stmts, True)
            queue.extend((stmt, conditional) for stmt in body)
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
                    or not breaking_changes.dml12_remove_misc_quirks.enabled):
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

    # name -> Template
    templates = {}
    for name in template_order:
        (site, asts, trait_stmts) = template_decls[name]
        (references, in_each_structure) = template_rank_structure[name]
        (spec, shared_objects) = object_spec_from_asts(
            site, asts, templates, references, in_each_structure,
            RankDesc('file', os.path.basename(name[1:])) if name.startswith('@')
            else RankDesc('template', name),
            name if trait_stmts is not None else None)
        templates[name] = Template(
            name, template_trait(name, spec, trait_stmts, shared_objects, []),
            spec, shared_objects)
    # The generation of struct definitions assumes the dictionary to
    # be topologically ordered on inheritance: if A inherits B, B
    # appears first in the dict. The traits of shared objects are reached
    # through Trait.shared_objects.
    traits = {name: tpl.trait for (name, tpl) in templates.items()
              if tpl.trait is not None}
    def with_shared_objects(tpl):
        yield tpl
        for child in tpl.shared_objects.values():
            yield from with_shared_objects(child)
    templates_by_trait = {
        t.trait: t for tpl in templates.values()
        for t in with_shared_objects(tpl) if t.trait is not None}
    return (templates, traits, templates_by_trait)
