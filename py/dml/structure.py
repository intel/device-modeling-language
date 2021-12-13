# © 2021-2022 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

# Structure stuff

import itertools
import collections
import abc
import re
from . import objects, logging, crep, ast
from . import traits
from . import toplevel
from . import topsort
from . import slotsmeta
from . import ctree
from .logging import *
from .codegen import *
from .symtab import *
from .expr import *
from .ctree import *
from .expr_util import *
from .messages import *
from .types import *
import dml.globals
from . import template
from .template import Rank, RankDesc, ObjectSpec
from .reginfo import explode_registers
from . import dmlparse

__all__ = (
    'mkglobals', 'mkdev'
)

# A list of extern declarations, as tuples (name,site,type)
externs = []

def check_constant_expr(expr):
    '''Check that an expression in a constant declaration is either a
    proper value, or (recursively) a list of proper values; raise an
    error otherwise.'''
    if isinstance(expr, NonValue):
        if isinstance(expr, List):
            for sub in expr.value:
                check_constant_expr(sub)
        else:
            raise expr.exc()

def redundant_externs(stmts):
    '''Return a set of untyped 'extern' declarations that can be ignored
    in DML 1.2'''
    externs = {s for s in stmts if s.kind == 'extern'}
    typed = [s for s in externs if s.args[0][3] is not None]
    untyped = [s for s in externs if s.args[0][3] is None]
    if typed:
        return untyped
    else:
        return untyped[1:]

def mkglobals(stmts):
    "Collect all global constants and add them to the global scope"
    stmts = list(stmts)
    by_name = {}
    assert not global_scope.symbols()
    for stmt in stmts:
        if stmt.kind in ['extern', 'extern_typedef', 'dml_typedef']:
            ((_, _, name, _),) = stmt.args
        else:
            name = stmt.args[0]
        by_name.setdefault(name, []).append(stmt)

    for name in by_name:
        clash = by_name[name]
        if len(clash) > 1 and dml.globals.compat_dml12:
            # DML 1.2 permits multiple redundant 'extern foo;'
            # declarations; drop these
            for stmt in redundant_externs(clash):
                clash.remove(stmt)
                stmts.remove(stmt)
        if len(clash) <= 1:
            continue
        types = [s for s in clash if s.kind in ['extern_typedef', 'dml_typedef',
                                                'template', 'template_dml12']]
        if len(types) == 1:
            # types may clash with values
            clash.remove(types[0])
        if len(clash) > 1:
            report(ENAMECOLL(clash[1].site, clash[0].site, name))
            stmts.remove(clash[1])
            # retry with duplicate removed
            mkglobals(stmts)
            return

    # Evaluate constants first, this makes it possible to use them in
    # struct definitions (bug 6030).
    for stmt in stmts:
        try:
            if stmt[0] == 'constant':
                _, site, name, expr = stmt
                expr = codegen_expression_maybe_nonvalue(
                    expr, None, global_scope)
                check_constant_expr(expr)
                if expr.constant:
                    global_scope.add(ExpressionSymbol(name, expr, site))
                else:
                    raise ENCONST(expr, expr)
        except DMLError as e:
            report(e)

    # map name -> pair (ObjectSpec, list of trait statements or None)
    templates = {}
    new_symbols = []
    # all non-extern anonymous struct types, str -> type
    anonymous_structs = {}
    # names of non-extern typedefs
    new_typedefs = set()

    for stmt in stmts:
        try:
            if stmt.kind in ['template', 'template_dml12']:
                (name, body) = stmt.args
                template_body = []
                trait_body = []
                for tstmt in body:
                    if tstmt.kind in ('sharedmethod', 'typedparam'):
                        trait_body.append(tstmt)
                    elif tstmt.kind in ('session', 'saved'):
                        template_body.append(tstmt)
                        if stmt.kind == 'template':
                            trait_body.append(tstmt)
                    else:
                        template_body.append(tstmt)
                if stmt.kind == 'template_dml12':
                    # guaranteed by grammar.
                    assert trait_body == []
                    # means that no type is created
                    trait_body = None
                templates[name] = (stmt.site, template_body, trait_body)
            elif stmt[0] == 'extern':
                (_, esite, (_, site, name, typ)) = stmt
                if typ is None:
                    # guaranteed by grammar
                    assert dml.globals.dml_version == (1, 2)
                    if (not dml.globals.compat_dml12
                        and not site.filename().endswith('simics-api.dml')):
                        report(EEXTERN(stmt.site))
                    typ = TUnknown()
                else:
                    (struct_defs, typ) = eval_type(
                        typ, site, None, global_scope, extern=True,
                        allow_void=site.dml_version() == (1, 2))
                    # any substructs are converted to anonymous extern structs
                    assert not struct_defs
                new_symbols.append(LiteralSymbol(name, typ, site))
                externs.append((name, site, typ))
            elif stmt[0] == 'extern_typedef':
                (_, site, (_, _, name, typ)) = stmt
                assert not typedefs.get(name, None)
                (struct_defs, typ) = eval_type(
                    typ, site, None, global_scope, typename=name, extern=True,
                    allow_void=True)
                # any substructs are converted to anonymous extern structs
                assert not struct_defs
                typedefs[name] = typ
            elif stmt[0] == 'dml_typedef':
                (_, site, (_, _, name, typ)) = stmt
                assert not typedefs.get(name, None)
                (struct_defs, typ) = eval_type(typ, site, None, global_scope,
                                               typename=name, allow_void=True)
                for (_, t) in struct_defs:
                    if t is not typ:
                        anonymous_structs[t.label] = t
                typedefs[name] = typ
                assert name not in new_typedefs
                new_typedefs.add(name)
            elif stmt[0] == 'loggroup':
                _, site, name = stmt
                dml.globals.log_groups.append(name)
                new_symbols.append(LiteralSymbol(
                    name, TNamed('int', const=True), site,
                    crep.cloggroup(name)))
            elif stmt.kind != 'constant': # handled above
                raise ICE(stmt.site, 'bad AST kind: %s' % (stmt.kind,))
        except DMLError as e:
            report(e)

    for sym in new_symbols:
        global_scope.add(sym)

    (dml.globals.templates, dml.globals.traits) = template.process_templates(
        templates)

    for (tname, tpl) in list(dml.globals.templates.items()):
        if tpl.trait:
            assert tname not in typedefs
            trait_type = tpl.trait.type()
            # any name collisions were caught earlier with ENAMECOLL
            typedefs[tname] = trait_type

    type_declaration_order = sort_type_declarations(new_typedefs,
                                                    anonymous_structs)
    global_type_declaration_order[:] = type_declaration_order
    global_anonymous_structs.clear()
    global_anonymous_structs.update(anonymous_structs)

    if dml.globals.dml_version != (1, 2):
        for t in typedefs.values():
            try:
                safe_realtype(t)
            except ETYPE as e:
                report(e)
        for sym in new_symbols:
            try:
                safe_realtype(sym.type)
            except ETYPE as e:
                report(e)

def check_named_types(t):
    '''Checks that a type does not reference a non-existing type'''
    if isinstance(t, TNamed):
        if t.c not in typedefs:
            raise ETYPE(t.declaration_site, t)
    elif isinstance(t, TStruct):
        t.resolve()
        for (mn, mt) in t.members.items():
            check_named_types(mt)
    elif isinstance(t, (TPtr, TVector, TArray)):
        check_named_types(t.base)
    elif isinstance(t, TFunction):
        for pt in t.input_types:
            check_named_types(pt)
        check_named_types(t.output_type)
    elif isinstance(t, TTraitList):
        if t.traitname not in dml.globals.traits:
            raise ETYPE(t.declaration_site, t)
    elif isinstance(t, (TVoid, IntegerType, TBool, TFloat, TTrait, TObjIdentity)):
        pass
    else:
        raise ICE(t.declaration_site, "unknown type %r" % t)

def type_deps(t, include_structs, expanded_typedefs):
    '''Given that t appears inside a DML typedef, return the set of DML
    typedefs that need to appear before this typedef in generated C
    code. The location of a named struct ("typedef struct { ... } t;"
    in DML) in the dependency graph refers to the struct definition in
    C code ("struct t { ... }"); the type definition/struct
    declaration ("typedef struct t t;") appears
    before all other typedefs in generated C code.

    include_structs controls whether named structs count as
    dependencies. They do count for struct members and array types, since
    we otherwise would get an incomplete type.

    expanded_typedefs is used to avoid infinite recursion.
    '''
    if isinstance(t, TNamed):
        if t.c not in typedefs:
            raise ETYPE(t.declaration_site, t)
        if isinstance(typedefs[t.c], TStruct):
            if include_structs:
                if t.c in expanded_typedefs:
                    return [t.c]
                # Need to recurse explicitly to ensure we get all
                # dependencies, because of this example:
                #  typedef struct { } A;
                #  typedef A B;
                #  typedef struct { B b; } C;
                # Here, B does not depend on A, but C
                # depends on A via B.
                return [t.c] + type_deps(typedefs[t.c], True,
                                         expanded_typedefs + [t.c])
            else:
                return []
        else:
            return [t.c]
    elif isinstance(t, TStruct):
        t.resolve()
        deps = []
        if include_structs:
            deps.append(t.label)
        for (mn, mt) in t.members.items():
            deps.extend(type_deps(mt, True, expanded_typedefs))
        return deps
    elif isinstance(t, TArray):
        return type_deps(t.base, True, expanded_typedefs)
    elif isinstance(t, (TPtr, TVector)):
        return type_deps(t.base, False, expanded_typedefs)
    elif isinstance(t, TFunction):
        return ([dep for pt in t.input_types
                 for dep in type_deps(pt, False, expanded_typedefs)]
                + type_deps(t.output_type, False, expanded_typedefs))
    elif isinstance(t, (IntegerType, TVoid, TBool, TFloat, TTrait)):
        return []
    elif isinstance(t, TExternStruct):
        # extern structs are assumed to be self-contained
        return []
    else:
        raise ICE(t.declaration_site, "unknown type %r" % t)

def remove_type(name):
    del typedefs[name]
    if name in new_global_types:
        new_global_types.remove(name)

def sort_type_declarations(new_typedefs, anonymous_structs):
    deps = {}
    for name in typedefs:
        if name in new_typedefs:
            try:
                deplist = type_deps(typedefs[name], False, [])
            except DMLError as e:
                report(e)
                del typedefs[name]
                return sort_type_declarations(new_typedefs - {name},
                                              anonymous_structs)
            else:
                deps[name] = deplist
        else:
            # extern typedef
            deps[name] = []
    for tn in anonymous_structs:
        assert tn not in deps
        try:
            deps[tn] = type_deps(anonymous_structs[tn], False, [])
        except DMLError as e:
            report(e)
    try:
        type_order = topsort.topsort(deps)
    except topsort.CycleFound as e:
        report(ETREC([typedefs[n].declaration_site for n in e.cycle
                      if n in typedefs],
                     typedefs[e.cycle[0]]))
        for n in e.cycle:
            del typedefs[n]
        # retry with this cycle removed
        return sort_type_declarations(
            new_typedefs.difference(e.cycle), anonymous_structs)
    # TODO: we could check that no extern typedef depends on a
    # non-extern typedef, bug 24617
    return [n for n in type_order
            # exclude extern typedefs
            if n in new_typedefs or n in anonymous_structs]

unused_field_methods = {'after_read',
                        'before_read',
                        'after_write',
                        'before_write',
                        'after_set',
                        'before_set',
                        'after_get',
                        'before_get',
                        'after_hard_reset',
                        'after_soft_reset'}

def is_unused_default(node):
    parent = node.parent
    if not parent:
        # The device object
        return False

    # Warn for methods that are present in a field but only has effect
    # in a register
    if node.objtype == 'method' and parent.objtype == 'field':
        return node.name in unused_field_methods

    # Warn if the read or write method is overridden in a register
    # with fields
    if node.objtype == 'method' and parent.objtype == 'register':
        return (node.name in ['read', 'write']
                and not parent.wholefield
                # Presence of non-None default_impl means that the
                # method does have a non-default implementation
                and node.default_method.node)

    return False

dml12_methods = {
    'attribute': {'before_set', 'after_set'},
    'bank': {
        'access', 'get_write_value', 'set_read_value',
        'read_access_memop', 'finalize_read_access',
        'read_access', 'miss_read_access', 'write_access_memop',
        'finalize_write_access', 'write_access',
        'miss_write_access',
        '_unmapped_read_access', '_unmapped_write_access',
        'miss_access'},
    'register': {
        'after_hard_reset', 'after_soft_reset', 'read_access',
        'read_access_main', 'write_access', 'write_access_main',
        'before_read', 'after_read', 'before_write', 'after_write'},
    'field': {'read_access', 'write_access'},
    'connect': {'before_set', 'after_set', 'validate_port'},
}

def is_dml12_method(node):
    if node.objtype != 'method':
        return False
    objtype = node.parent.objtype
    if node.name in dml12_methods.get(objtype, ()):
        return True

def check_unused_and_warn(node):
    """Check for unused object in the object tree and warn if they are
    not used. This usually applies to methods and parameters."""

    if node.refcount == 0:
        if not warning_is_ignored('WUNUSED'):
            report(WUNUSED(node))
        elif is_unused_default(node):
            report(WUNUSEDDEFAULT(node))
        elif dml.globals.dml_version != (1, 2) and is_dml12_method(node):
            report(WUNUSED_DML12(node))

    for n in node.get_components():
        check_unused_and_warn(n)

def wrap_sites(obj_spec, issite, tname):
    '''Instantiate a template, by traversing it and replacing sites'''
    templates = [(TemplateSite(site, issite, tname), t)
                 for (site, t) in obj_spec.templates]
    params = [ast.parameter(TemplateSite(site, issite, tname), name, value)
              for (_, site, name, value) in obj_spec.params]

    blocks = []
    for (preconds, shallow, composite) in obj_spec.blocks:
        shallow_wrapped = []
        for stmt in shallow:
            asttype = stmt.kind
            if asttype == 'method':
                (_, site, name, value, overridable, export, rsite) = stmt
                shallow_wrapped.append(ast.method(
                    TemplateSite(site, issite, tname), name, value,
                    overridable, export, rsite))
            elif asttype == 'error':
                (_, site, msg) = stmt
                shallow_wrapped.append(
                    ast.error(TemplateSite(site, issite, tname), msg))
            elif asttype == 'session':
                (_, site, decls, inits) = stmt
                shallow_wrapped.append(
                    ast.session(TemplateSite(site, issite, tname),
                                decls, inits))
            elif asttype == 'saved':
                (_, site, decls, inits) = stmt
                shallow_wrapped.append(
                    ast.saved(TemplateSite(site, issite, tname),
                              decls, inits))
            else:
                raise ICE(issite, 'unknown node type %r %r' % (asttype, stmt))
        composite_wrapped = [
            (objtype, name, arrayinfo, wrap_sites(spec, issite, tname))
            for (objtype, name, arrayinfo, spec) in composite]
        blocks.append((preconds, shallow_wrapped, composite_wrapped))

    return ObjectSpec(TemplateSite(obj_spec.site, issite, tname),
                      obj_spec.rank, templates, obj_spec.in_eachs,
                      params, blocks)

def setparam(node, name, mkexpr):
    """
    Set a parameter from an expression.
    """
    assert isinstance(mkexpr, objects.ParamExpr)

    pnode = node.get_component(name, 'parameter')
    if not pnode:
        raise ICE(mkexpr.site, "Unknown parameter '%s' in setparam" % (name,))
    pnode.set_expr(mkexpr)

def add_templates(obj_specs, each_stmts):
    used_templates = set()
    queue = sum((obj_spec.templates for obj_spec in obj_specs), [])

    i = 0
    while i < len(queue):
        (site, tpl) = queue[i]
        i += 1
        if (dml.globals.compat_dml12
            and tpl.name in dml.globals.missing_templates):
            report(ENTMPL(site, tpl.name))
            continue
        if tpl in used_templates:
            continue
        used_templates.add(tpl)
        for (tpls, spec) in each_stmts.get(tpl, []):
            for t in tpls:
                if t not in used_templates:
                    # One template did not match, at least not yet. It
                    # can happen that this template is instantiated
                    # but just not yet processed; add an entry to
                    # each_stmts so we will re-consider this in-each
                    # statement when the missing template is instantiated.
                    each_stmts = each_stmts.copy()
                    each_stmts[t] = each_stmts.get(t, []) + [(tpls, spec)]
                    break
            else:
                # All templates match; expand the each statement.
                obj_specs.append(wrap_sites(spec, spec.site, tpl.name))
                queue.extend(spec.templates)

        if tpl.name.startswith('@'):
            obj_spec = tpl.spec
        else:
            obj_spec = wrap_sites(tpl.spec, site, tpl.name)
        obj_specs.append(obj_spec)
        queue.extend(obj_spec.templates)

    if logging.show_porting:
        PWUNUSED.used_templates.update(used_templates)

    return obj_specs

def eval_arraylen(size_ast, parent_scope):
    size = codegen_expression(size_ast, parent_scope, global_scope)
    if not size.constant:
        raise EASZVAR(size.site, size)
    if not isinstance(size.value, int):
        report(EBTYPE(size.site, size, "integer"))
        return 2  # arbitrary nonzero integer
    if size.value < 1:
        raise EASZR(size.site)
    return size.value

def merge_parameters(defs, obj_specs):
    '''Given a list of parameters along with the templates they are
    defined in, as (rank, ast.parameter) pairs, return the
    declaration to use for the parameter, or raise a DMLError.'''

    # Handle declarations ("parameter x;") separately
    decls = [(rank, p) for (rank, p) in defs if p.args[1] == (None, None, None)]
    defs = [(rank, p) for (rank, p) in defs if p.args[1] != (None, None, None)]

    if not defs:
        (_, decl) = decls[0]
        (name, _) = decl.args
        raise ENPARAM(decl.site, name)

    # Handle 'auto' declarations separately. It's easy; an 'auto'
    # declaration must be alone.
    for (_, param) in defs:
        (name, (_, _, auto)) = param.args
        if auto:
            if len(defs) == 1:
                return param
            else:
                raise EAUTOPARAM(param.site, name)

    all_inferior = set().union(*(rank.inferior for (rank, _) in defs))
    superior = [(rank, p) for (rank, p) in defs if rank not in all_inferior]

    if dml.globals.dml_version == (1, 2):
        # Backward compatibility: If there is exactly one
        # non-default definition, then disregard template
        # instantiation relations and give the non-default definition
        # precedence
        nondefault = [(r, p) for (r, p) in defs
                      if p.args[1][0]]
        if len(nondefault) == 1:
            [(nd_rank, nd_param)] = nondefault
            # Add 'is' declaration in 1.4. In the odd case where
            # t1 overrides t2 but the non-default declaration is
            # in t2, do nothing (the right thing is likely to
            # remove the definition in t2)
            if (logging.show_porting and len(superior) > 1
                and nd_rank not in all_inferior):
                for (r, p) in superior:
                    if r != nd_rank:
                        report_poverride(nd_rank, r, obj_specs)
            return nd_param

    if len(superior) > 1:
        def is_default(decl):
            (_, p) = decl
            (_, (_, default, _)) = p.args
            return default is not None

        # If two declarations are non-default, then blame them (bug 24322)
        [(rank1, p1), (rank2, p2)] = sorted(
            superior, key=is_default)[:2]
        (name, _) = p1.args
        if rank1 is rank2:
            # If the two conflicting default definitions are in the
            # same template/file, then the message in EAMBINH is
            # inaccurate and it's easier to understand it as a name
            # collision.
            raise ENAMECOLL(p1.site, p2.site, name)
        else:
            raise EAMBINH(
                p1.site, p2.site, name, rank1.desc, rank2.desc,
                is_default((rank1, p1)))

    [(rank0, param0)] = superior

    for (rank, param) in defs:
        (name, (final, _, _)) = param.args
        if final and rank is not rank0:
            # Attempt to override non-default parameter
            report(EINVOVER(param0.site, param.site, name))
            return param

    if dml.globals.dml_version == (1, 2):
        # New inheritance rules are kept internal for now
        if len(defs) == 2:
            (_, (_, default, _)) = param0.args
            if default:
                report(WEXPERIMENTAL(
                    param0.site, "parameter with two default declarations"))
        elif len(defs) > 2:
            report(WEXPERIMENTAL(
                param0.site, "more than one level of parameter overrides"))

    return param0

def typecheck_method_override(m1, m2):
    '''check that m1 can override m2'''
    assert m1.kind == m2.kind == 'method'
    (_, (inp1, outp1, throws1, _), _, _, _) = m1.args
    (_, (inp2, outp2, throws2, _), _, _, _) = m2.args

    # We should also check parameter types here (bug 20686)
    if len(inp1) != len(inp2):
        raise EMETH(m1.site, m2.site, "different number of input parameters")
    if len(outp1) != len(outp2):
        raise EMETH(m1.site, m2.site, "different number of output parameters")
    for (a1, a2) in zip(inp1, inp2):
        ((n1, t1), (n2, t2)) = (a1.args, a2.args)
        if (t1 is None) != (t2 is None):
            if dml.globals.dml_version == (1, 2):
                if t1 is None and logging.show_porting:
                    try:
                        (struct_defs, type2) = eval_type(
                            t2, a2.site, None, global_scope)
                    except DMLError:
                        # uncommon but can happen, e.g. an argument
                        # with static array length that depends on a
                        # parameter
                        pass
                    else:
                        report(PINARGTYPE(a1.site, type2.declaration(n1)))
            else:
                raise EMETH(m1.site, m2.site, "different inline args")
        if (t1 and t2
            and a1.site.dml_version() != (1, 2)
            and a2.site.dml_version() != (1, 2)):
            # TODO move to caller
            (_, type1) = eval_type(t1, a1.site, None, global_scope)
            (_, type2) = eval_type(t2, a2.site, None, global_scope)
            if safe_realtype(type1).cmp(safe_realtype(type2)) != 0:
                raise EMETH(a1.site, a2.site,
                            f"mismatching types in input argument {n1}")

    for (i, (a1, a2)) in enumerate(zip(outp1, outp2)):
        if a1.site.dml_version() != (1, 2) and a2.site.dml_version() != (1, 2):
            ((n1, t1), (n2, t2)) = (a1.args, a2.args)
            (_, type1) = eval_type(t1, a1.site, None, global_scope)
            (_, type2) = eval_type(t2, a2.site, None, global_scope)
            if safe_realtype(type1).cmp(safe_realtype(type2)) != 0:
                msg = "mismatching types in return value"
                if len(outp1) > 1:
                    msg += f" {i + 1}"
                raise EMETH(a1.site, a2.site, msg)

    if (logging.show_porting
        and all(a2.args[1] for a2 in inp2)
        and any(not a1.args[1] for a1 in inp1)):
        # hack to change 'inline method' -> 'method' if needed
        report(PINARGTYPE(m1.site, 'method'))

    # 'throws' annotations are verified later

def report_poverride(sup, inf, obj_specs):
    if inf in sup.inferior:
        # rank already established
        return
    sup_objs = [o for o in obj_specs if o.rank is sup]
    assert sup_objs
    for sup_obj in sup_objs:
        if inf.desc.kind == 'template':
            report(POVERRIDE(sup_obj.site, inf.desc.text))
        else:
            report(POVERRIDE_IMPORT(sup_obj.site, inf.desc.text))

def sort_method_implementations(implementations, obj_specs):
    if dml.globals.dml_version == (1, 2) and len(implementations) == 2:
        # Backward compatibility: If there is exactly one default and
        # one non-default implementation, then disregard template
        # instantiation relations.
        [m1, m2] = implementations
        # create fake ranks to make sure methods end up in the right order
        if m1.overridable and not m2.overridable:
            if logging.show_porting:
                report_poverride(m2.rank, m1.rank, obj_specs)
            m2.rank = Rank({m1.rank}, m2.rank.desc)
        elif not m1.overridable and m2.overridable:
            if logging.show_porting:
                report_poverride(m1.rank, m2.rank, obj_specs)
            m1.rank = Rank({m2.rank}, m1.rank.desc)
    return traits.sort_method_implementations(implementations)

def merge_subobj_defs(def1, def2, parent):
    (objtype, name, arrayinfo, obj_specs1) = def1
    (objtype2, name2, arrayinfo2, obj_specs2) = def2
    assert name == name2

    site1 = obj_specs1[0].site
    site2 = obj_specs2[0].site

    if objtype != objtype2:
        report(ENAMECOLL(site1, site2, name))
        return def1

    if len(arrayinfo) != len(arrayinfo2):
        raise EAINCOMP(site1, site2, name,
                       "mixing declarations with different number "
                       "of array dimensions")

    merged_arrayinfo = []
    if arrayinfo:
        parent_scope = Location(parent, static_indices(parent))

        for ((idxvar1, len1), (idxvar2, len2)) in zip(arrayinfo, arrayinfo2):
            if idxvar1 != idxvar2:
                raise EAINCOMP(site1, site2, name,
                               "mismatching index variables")

            if len1 is None:
                merged_arrayinfo.append((idxvar1, len2))
            elif len2 is not None and (eval_arraylen(len1, parent_scope)
                                       != eval_arraylen(len2, parent_scope)):
                raise EAINCOMP(site1, site2, name, "mismatching array sizes")
            else:
                merged_arrayinfo.append((idxvar1, len1))


    return (objtype, name, merged_arrayinfo, obj_specs1 + obj_specs2)

def method_is_std(node, methname):
    """
    Return true if the method implementation is taken from dml-builtins.dml.
    """
    meth = node.get_component(methname)
    if not meth:
        # can happen after a compile error
        return True
    return meth.site.filename().endswith("dml-builtins.dml")

def mkdata(spec, parent):
    site, name, typ, astinit = spec

    parent_scope = Location(parent, static_indices(parent))
    (struct_defs, dtype) = eval_type(
        typ, site, parent_scope, global_scope)
    dtype = dtype.resolve()
    TStruct.late_global_struct_defs.extend(struct_defs)
    obj = objects.Session(name, dtype, astinit, site, parent)
    if astinit:
        dml.globals.device.add_init_data(obj)
    try:
        realtype(crep.node_storage_type(obj))
    except DMLUnknownType as e:
        raise ETYPE(obj, e.type)
    return obj

def mksaved(spec, parent):
    site, name, typ, astinit = spec

    parent_scope = Location(parent, static_indices(parent))
    (struct_defs, dtype) = eval_type(typ, site, parent_scope, global_scope)
    TStruct.late_global_struct_defs.extend(struct_defs)
    dtype.resolve()
    if deep_const(dtype):
        raise ESAVEDCONST(site, dtype)
    obj = objects.Saved(name, dtype, astinit, site, parent)
    if astinit:
        dml.globals.device.add_init_data(obj)
    try:
        realtype(crep.node_storage_type(obj))
    except DMLUnknownType as e:
        raise ETYPE(obj, e.type)
    return obj

def register_fields(reg):
    '''Return a generator returning all field instances of a
    register. Also reports errors if the msb, lsb or bitsize parameter
    is not constant across register indices.'''
    assert dml.globals.dml_version == (1, 2)
    for g in reg.get_components('group'):
        # If we would allow a hierarchy reg -> group -> field, then we
        # would run into trouble because the implicit field is added
        # before we can know that the group actually contains a field.
        # For this reason, in DML 1.2 we disallow groups in regs
        # altogether.
        report(ENALLOW(g.site, reg))
    unexpanded = reg.get_components('field')
    for field in unexpanded:

        bitsizes = []
        for fieldi in itertools.product(
                *(list(range(sz)) for sz in field.dimsizes[reg.dimensions:])):
            indices = static_indices(reg) + tuple(
                mkIntegerLiteral(field.site, i) for i in fieldi)
            lsb = param_int(field, 'lsb', indices=indices)
            msb = param_int(field, 'msb', indices=indices)
            if msb < lsb:
                raise EBITRN(field, msb, lsb)
            yield (field, fieldi, lsb, msb)
            # bitsize is defined in terms of lsb and msb, so we
            # don't need to evaluate it across register indices
            bitsize = param_expr(field, 'bitsize',
                tuple(mkIntegerLiteral(field.site, i)
                      for i in (0,) * reg.dimensions + fieldi))
            if not bitsize.constant:
                # guaranteed by dml-builtins.dml
                raise ICE(reg.site, 'non-constant bitsize')
            bitsizes.append(bitsize.value)

        if any(sz != bitsizes[0] for sz in bitsizes[1:]):
            # bitsize is defined in dml-builtins, so the 'msb'
            # site is probably more relevant
            report(EFARRSZ(param_expr_site(field, 'msb')))

def report_unused_templates():
    for tpl in PWUNUSED.used_templates:
        if not tpl.name.startswith('@'):
            report(PNO_WUNUSED(tpl.site, 'template', tpl.name))
    for tpl in set(dml.globals.templates.values()) - PWUNUSED.used_templates:
        if not tpl.name.startswith('@'):
            report(PWUNUSED(tpl.site, 'template', tpl.name))

def report_unused_conds():
    for cond in PWUNUSED.positive_conds:
        if cond in PWUNUSED.satisfied_conds:
            report(PNO_WUNUSED(cond.site, 'if', None))
        else:
            report(PWUNUSED(cond.site, 'if', None))
    for cond in PWUNUSED.negative_conds:
        if cond in PWUNUSED.satisfied_conds:
            report(PNO_WUNUSED(cond.site, 'else', None))
        else:
            report(PWUNUSED(cond.site, 'else', None))

def mkdev(devname, obj_specs):
    dev = mkobj(devname, 'device', (), obj_specs, None, {})
    if logging.show_porting:
        report_unused_conds()
        report_unused_templates()
    return dev

# Map frozenset of Trait objects, to implicit trait inheriting those
# traits
implicit_traits = {}

def mkobj(ident, objtype, arrayinfo, obj_specs, parent, each_stmts):
    # Sometimes we need to pick a single site to point at for a given
    # object, even if the declaration is dispersed over multiple
    # locations. The first element of obj_specs is usually has high
    # rank, so that's a relevant site to pick.
    site = obj_specs[0].site

    (index_vars, arraylen_asts) = list(zip(*arrayinfo)) or ((), ())

    obj = create_object(site, ident, objtype, parent,
                        arraylen_asts, index_vars)

    with ErrorContext(obj):
        obj_specs = add_templates(obj_specs, each_stmts)
        index_sites = [ast.site for ast in arraylen_asts]
        obj_params = create_parameters(obj, obj_specs, index_vars, index_sites)
        return mkobj2(obj, obj_specs, obj_params, each_stmts)

def create_object(site, ident, objtype, parent,
                  arraylen_asts, index_vars):
    array_lens = tuple(
        eval_arraylen(len_ast, Location(parent, static_indices(parent)))
        for len_ast in arraylen_asts)

    if objtype == 'device':
        assert not arraylen_asts
        return objects.Device(ident, site)
    elif objtype == 'bank':
        if ident is None and not dml.globals.compat_dml12:
            report(ESYNTAX(site, 'bank', 'anonymous banks are not allowed'))
        return objects.Bank(ident, site, parent, array_lens, index_vars)
    elif objtype == 'group':
        return objects.Group(ident, site, parent, array_lens, index_vars)
    elif objtype == 'register':
        return objects.Register(ident, site, parent,
                                array_lens, index_vars)
    elif objtype == 'field':
        return objects.Field(ident, site, parent, array_lens, index_vars)
    elif objtype == 'connect':
        return objects.Connection(ident, site, parent,
                                  array_lens, index_vars)
    elif objtype == 'interface':
        assert not arraylen_asts
        return objects.Interface(ident, site, parent)
    elif objtype == 'attribute':
        return objects.Attribute(ident, site, parent, array_lens, index_vars)
    elif objtype == 'event':
        return objects.Event(ident, site, parent, array_lens, index_vars)
    elif objtype == 'port':
        return objects.Port(ident, site, parent, array_lens, index_vars)
    elif objtype == 'subdevice':
        return objects.Subdevice(ident, site, parent, array_lens, index_vars)
    elif objtype == 'implement':
        assert not arraylen_asts
        return objects.Implement(ident, site, parent)

    raise ICE(site, "unknown object type %s" % (objtype,))

def make_autoparams(obj, index_vars, index_var_sites):
    site = obj.site

    autoparams = {}

    if dml.globals.dml_version == (1, 2):
        autoparams['name'] = SimpleParamExpr(
            mkStringConstant(site, obj.ident) if obj.ident
            else mkUndefined(site))
    else:
        autoparams['_ident'] = SimpleParamExpr(
            mkStringConstant(site, obj.ident))

    index_params = ()
    # Handle array information
    for (dim, (index_var, var_site)) in enumerate(
            zip(index_vars, index_var_sites)):
        idx_param = IndexParamExpr(var_site, obj.parent.dimensions + dim,
                                   index_var)
        index_params += (idx_param,)
        # This will refer to the index coupled with the idxvar,
        # innermost overrides
        autoparams[index_var] = idx_param

    # Assign auto parameters related to array info
    # In 1.4; The 'indices' auto-param is a list containing local indices
    # In 1.2; The 'index' auto-param is a list containing local indices
    #         if in a multi-dimensional array, the local index if in a simple
    #         array, and undefined otherwise
    #         The 'indexvar' auto-param is the name of index variable that the
    #         local index is stored in if in a simple array, undefined otherwise
    if dml.globals.dml_version == (1, 2):
        if len(index_vars) == 1:
            [index_var] = index_vars
            # TODO: Add this documentation to dml.docu
            # If in a multi-dimensional array, this will be set to undefined
            # So in 1.2 you can verify if you are in a multi-dimensional
            # array by checking if this is defined
            autoparams['indexvar'] = SimpleParamExpr(
                mkStringConstant(site, index_var))
            autoparams['index'] = autoparams[index_var]
        elif index_vars:
            autoparams['index'] = IndexListParamExpr(site, index_params)
        else:
            autoparams['index'] = SimpleParamExpr(mkUndefined(site))
    else:
        autoparams['indices'] = IndexListParamExpr(site, index_params)
        autoparams['_be_bitorder'] = SimpleParamExpr(
            mkBoolConstant(site, site.bitorder() == 'be'))

    if obj.objtype == 'device':
        autoparams['obj'] = SimpleParamExpr(mkDeviceObject(site))
        if dml.globals.dml_version == (1, 2):
            autoparams['banks'] = UninitializedParamExpr(site, 'banks')
        autoparams['simics_api_version'] = SimpleParamExpr(
            mkStringConstant(site, dml.globals.api_version))

        dml.globals.device = obj

    elif obj.objtype == 'bank':
        if dml.globals.dml_version == (1, 2):
            for pname in ['mapped_registers', 'unmapped_registers',
                          'numbered_registers']:
                autoparams[pname] = UninitializedParamExpr(site, pname)

    elif obj.objtype == 'register':
        if dml.globals.dml_version == (1, 2):
            autoparams['fields'] = UninitializedParamExpr(site, 'fields')
            autoparams['_regname'] = QNameParamExpr(obj, 'bank')

    elif obj.objtype == 'field':
        if dml.globals.dml_version == (1, 2):
            # If this is an automatic field, create msb/lsb parameter from
            # the size of the register
            if obj.ident:
                autoparams['explicit'] = SimpleParamExpr(
                    mkBoolConstant(site, True))
            else:
                autoparams['name'] = SimpleParamExpr(
                    param_expr(obj.parent, 'name'))
                autoparams['explicit'] = SimpleParamExpr(
                    mkBoolConstant(site, False))

    elif obj.objtype == 'connect':
        if dml.globals.dml_version == (1, 2):
            autoparams['interfaces'] = UninitializedParamExpr(
                site, 'interfaces')

    elif obj.objtype == 'event':
        autoparams['evclass'] = EventClassParamExpr(obj)

    # Add common automatic parameters
    autoparams['qname'] = QNameParamExpr(obj, 'device')

    if obj.parent:
        autoparams['parent'] = ParentParamExpr(obj)
    else:
        autoparams['parent'] = SimpleParamExpr(mkUndefined(obj.site))

    return autoparams

def implicit_params(obj, index_vars):
    # Find index_vars collisions here
    sorted_ivars = sorted(index_vars)
    for v1, v2 in zip(sorted_ivars, sorted_ivars[1:]):
        if v1 == v2:
            report(ENAMECOLL(obj.site, obj.site, v1))
    params = [(var, (None, None, 'auto')) for var in index_vars]

    if (dml.globals.dml_version == (1, 2)
        and obj.objtype == 'field'
        and obj.ident is None):
        site = obj.site
        regsize = param_int(obj.parent, 'size')
        params.extend([('msb', (ast.int(site, regsize * 8 - 1), None, None)),
                       ('lsb', (ast.int(site, 0), None, None))])
    return params

def create_parameters(obj, obj_specs, index_vars, index_sites):
    '''Merge parameter ASTs and convert to Parameter objects'''

    # "automatic" parameters are declared 'parameter xyz auto;' in
    # dml-builtins, where the 'auto' is substituted by a magic value
    # by the compiler.
    # "implicit" parameters are instead parameters where the entire
    # declaration is magically added by the compiler
    implicit_rank = Rank(set(), RankDesc('verbatim',
                                         '<implicit parameter block>'))
    # map parameter name -> list of (Rank, ast.parameter object)
    parameters = {name: [(implicit_rank, ast.parameter(obj.site, name, v))]
                  for (name, v) in implicit_params(obj, index_vars)}
    for obj_spec in obj_specs:
        for s in obj_spec.params:
            assert s.kind == 'parameter'
            (name, _) = s.args
            parameters.setdefault(name, []).append((obj_spec.rank, s))

    autoparams = make_autoparams(obj, index_vars, index_sites)

    return [mkparam(obj, autoparams,
                    merge_parameters(parameters[name], obj_specs))
            for name in sorted(parameters)]

def eval_precond(cond_ast, obj, global_scope):
    try:
        with ErrorContext(obj, cond_ast.site):
            cond = codegen_expression(
                cond_ast, Location(obj, static_indices(obj, cond_ast.site)),
                global_scope)
    except DMLError as e:
        report(e)
        return False # whatever
    else:
        cond = as_bool(cond)
        if cond.constant:
            # guaranteed by as_bool()
            assert isinstance(cond.ctype(), TBool)
            return cond.value
        else:
            report(ENCONST(cond, cond))
            return False # whatever

class InvalidDefault(objects.MethodDefault):
    '''Method default value that cannot be called'''
    def __init__(self, sym):
        self.sym = sym
    node = None
    def default_sym(self, indices):
        return self.sym

class DefaultMethodObj(objects.MethodDefault):
    __slots__ = ('node',)
    def __init__(self, node):
        self.node = node
    def default_sym(self, indices):
        return ExpressionSymbol(
            'default',
            mkNodeRef(self.node.site, self.node, indices),
            self.node.site)

class DefaultTraitMethod(objects.MethodDefault):
    '''The 'default' symbol of a non-trait method that overrides a trait
    method'''
    def __init__(self, trait_method, trait_node):
        # TraitMethod instance
        self.trait_method = trait_method
        # object whose trait the method belongs to, i.e., the method's
        # parent node.
        self.trait_node = trait_node
    node = None
    def default_sym(self, indices):
        site = self.trait_method.site
        default_def_trait = self.trait_method.trait

        ancestry_path = self.trait_node.traits.ancestry_paths[
            default_def_trait][0]
        if default_def_trait is not self.trait_method.vtable_trait:
            ancestry_path += default_def_trait.ancestry_paths[
                self.trait_method.vtable_trait][0]
        return ExpressionSymbol(
            'default',
            TraitMethodDirect(
                site, ObjTraitRef(
                    site, self.trait_node, self.trait_method.vtable_trait,
                    indices,
                    ancestry_path=ancestry_path),
                self.trait_method),
            site)

def report_pbefaft(obj, method_asts):
    '''before/after methods, e.g. before_write + after_write, are merged
    into one method, e.g. write(uint64 value), which calls default() between
    callbacks.
    '''
    # Standard before/after methods that are to be converted, together
    # with the method to replace them with, and the call to default().
    # Some special handling is needed for read and write.
    before_after_pairs = {
        'register': [
             ('before_set', 'after_set',
              'set(uint64 value)', 'default(value)'),
            ('before_write', 'after_write',
             # value arg is renamed if before_write is present;
             # handled with special case below
             'write_register(uint64 value, uint64 enabled_bytes, void *aux)',
             'default(value, enabled_bytes, aux)'),
            ('before_read', 'after_read',
             # default statement must be adjusted if after_read
             # is present; handled with special case below
             'read_register(uint64 enabled_bytes, void *aux) -> (uint64)',
             'return default(enabled_bytes, aux)'),
            (None, 'after_hard_reset', 'hard_reset()', 'default()'),
            (None, 'after_soft_reset', 'soft_reset()', 'default()')],
        'attribute': [('before_set', 'after_set',
                       'set(attr_value_t value) throws', 'default(value)')],
        'connect': [('before_set', 'after_set',
                     'set(conf_object_t *obj)', 'default(obj)')],
    }
    if obj.objtype in before_after_pairs:
        for (before_name, after_name, method_decl, default_call) in (
                before_after_pairs[obj.objtype]):
            # map rank -> AST
            before_asts = dict(method_asts.get(before_name, []))
            after_asts = dict(method_asts.get(after_name, []))
            # map rank -> (before, after)
            combined = {r: (before_asts.get(r), after_asts[r])
                        for r in after_asts}
            for r in set(before_asts) - set(after_asts):
                combined[r] = (before_asts[r], None)
            # don't update standard lib
            lib_tpl = ('conf_attribute' if before_name == 'before_set'
                       else obj.objtype)
            del combined[dml.globals.templates[lib_tpl].spec.rank]
            for (bef, aft) in combined.values():
                if aft:
                    (_, _, _, aft_body) = aft.args[1]
                    aft_args = [aft.site, dmlparse.start_site(aft_body.site),
                                dmlparse.end_site(aft_body.site)]
                else:
                    aft_args = [None, None, None]
                if after_name == 'after_read':
                    if aft:
                        default_call = ('local uint64 value'
                                        + ' = default(enabled_bytes, aux)')
                        return_stmt = 'return value;'
                    else:
                        return_stmt = None
                else:
                    return_stmt = None
                if bef:
                    (bef_inp, _, _, bef_body) = bef.args[1]
                    if before_name == 'before_write':
                        # find name of 'value' arg
                        (_, _, _, value_cdecl) = bef_inp
                        (value_arg, _) = value_cdecl.args
                        method_decl = method_decl.replace('value', value_arg)
                        default_call = default_call.replace('value', value_arg)
                    report(PBEFAFT(bef.site, dmlparse.start_site(bef_body.site),
                                   method_decl, default_call, return_stmt,
                                   bef.site, dmlparse.start_site(bef_body.site),
                                   dmlparse.end_site(bef_body.site),
                                   *aft_args))
                else:
                    report(PBEFAFT(aft.site, dmlparse.start_site(aft_body.site),
                                   method_decl, default_call, return_stmt,
                                   None, None, None,
                                   *aft_args))
                if obj.objtype == 'register':
                    tramp_site = bef.site if bef else aft.site
                    if before_name == 'before_read':
                        report(PTRAMPOLINE(tramp_site, None,
                                           'is dml12_compat_read_register;'))
                    elif before_name == 'before_write':
                        report(PTRAMPOLINE(tramp_site, None,
                                           'is dml12_compat_write_register;'))


def wrap_method_body_in_try(site, overridden_site, obj, name, body):
    if (obj.objtype != 'implement'
        and not site.filename().endswith('dml-builtins.dml')):
        report(WTHROWS_DML12(site, overridden_site))
    return ast.compound(site, [
        ast.try_(site, body, ast.log(
            site, 'error', ast.int(site, 1), None, ast.int(site, 0),
            'Unexpected exception in %s.%s, ignoring'
            % (obj.logname_anonymized(), name), []))])

class ObjMethodHandle(traits.MethodHandle):
    shared = False
    def __init__(self, method_ast, rank):
        (name, (_, _, throws, _), overridable, _, _) = method_ast.args
        super(ObjMethodHandle, self).__init__(
            method_ast.site, name, rank, overridable)
        self.method_ast = method_ast
        self.throws = throws

class TraitMethodHandle(traits.MethodHandle):
    shared = True
    def __init__(self, trait_method, rank):
        super(TraitMethodHandle, self).__init__(
            trait_method.site, trait_method.name,
            rank, trait_method.overridable)
        self.trait_method = trait_method
        self.throws = trait_method.throws

def process_method_implementations(obj, name, implementations,
                                   shared_impl_traits, obj_specs,
                                   vtable_nothrow_dml14):
    # A method can have both shared and non-shared implementations.
    # We will create a MethodHandle object for either, which
    # sort_method_implementations() uses to resolve override order.
    unshared_methods = [
        ObjMethodHandle(method_ast, rank)
        for (rank, method_ast) in implementations]
    shared_methods = [
        TraitMethodHandle(
            impl_trait.method_impls[name],
            dml.globals.templates[impl_trait.name].spec.rank)
        for impl_trait in shared_impl_traits]

    (default_map, method_order) = sort_method_implementations(
        unshared_methods + shared_methods, obj_specs)

    impl_to_method = {}
    for (default_level, impl) in reversed(list(enumerate(
            method_order))):
        if impl.shared:
            if default_map[impl]:
                # shared method overrides a non-shared method
                report(ETMETH(
                    default_map[impl][0].site, impl.site, name))
            # handled separately
            continue
        defaults = default_map[impl]
        if len(defaults) == 0:
            default = InvalidDefault(traits.NoDefaultSymbol())
        elif len(defaults) == 1:
            if defaults[0].shared:
                [trait] = shared_impl_traits
                default = DefaultTraitMethod(
                    trait.method_impls[name], obj)
            else:
                default = DefaultMethodObj(
                    impl_to_method[defaults[0]])
        else:
            default = InvalidDefault(traits.AmbiguousDefaultSymbol(
                [m.site for m in defaults]))
        (name, (inp_ast, outp_ast, throws, body), _, _,
         rbrace_site) = impl.method_ast.args
        if (impl.method_ast.site.dml_version() == (1, 2) and throws
            and vtable_nothrow_dml14):
            body = wrap_method_body_in_try(
                impl.method_ast.site, vtable_nothrow_dml14,
                obj, name, body)
            throws = False
        for overridden in default_map[impl]:
            if not overridden.overridable:
                raise EDMETH(impl.site, overridden.site, name)
            # the override of trait ASTs is checked later, by
            # traits.typecheck_method_override
            if not overridden.shared:
                # captured with ETMETH above
                assert not impl.shared
                typecheck_method_override(impl.method_ast,
                                          overridden.method_ast)
            if (impl.method_ast.site.dml_version() == (1, 2) and throws
                and not overridden.throws
                and overridden.method_ast.site.dml_version() == (1, 4)):
                # If a 1.4 library file defines an overrideable
                # non-throwing method, and a 1.2 device overrides
                # this, then assume that the method was never
                # intended to throw, and that the throws ->
                # nothrow change is part of the migration to 1.4.
                # We modify the override to nothrow, and patch it
                # to catch any exceptions.
                body = wrap_method_body_in_try(
                    impl.method_ast.site, overridden.method_ast.site,
                    obj, name, body)
                throws = False
            elif throws != overridden.throws:
                if (dml.globals.dml_version == (1, 2)
                      and not throws):
                    # Permit a nonthrowing method to override
                    # a throwing 1.2 method, with no warning.
                    # This is needed for some common standard
                    # library overrides, e.g. read, write,
                    # get, set, when sharing code between DML
                    # 1.2 and 1.4,
                    pass
                else:
                    raise EMETH(impl.site, overridden.site,
                                "different nothrow annotations")

        method = mkmethod(impl.site, rbrace_site,
                          Location(obj, static_indices(obj)),
                          obj, name, inp_ast,
                          outp_ast, throws, body, default,
                          default_level)
        impl_to_method[impl] = method

    if dml.globals.dml_version == (1, 2):
        for (_, method_ast) in implementations:
            (_, msite, _, _, _, exported, _) = method_ast
            if exported:
                if not method.fully_typed:
                    raise EEXTERN(method.site)
                func = method_instance(method)
                mark_method_referenced(func)
                mark_method_exported(func, crep.cref(method), msite)
                break
        # Export hard_reset and soft_reset from device objects in 1.2
        # automatically
        if (obj.objtype == 'device' and
            name in ('hard_reset', 'soft_reset')):
            func = method_instance(method)
            mark_method_referenced(func)
            mark_method_exported(func, crep.cref(method), obj.site)

    return method

ident_re = re.compile(r'[A-Za-z_][\w_]*')
# Second stage of object creation, protected by error context
# TODO: we should probably split up mkobj into several methods with
# clearer responsibilities
def mkobj2(obj, obj_specs, params, each_stmts):
    for param in params:
        obj.add_component(param)

    # For each symbol defined in this object's scope, map symbol name
    # to site. For name collision detection.
    symbols = {param.name: param.site for param in params}
    if dml.globals.dml_version == (1, 4):
        objname = param_str(obj, 'name')
        if not ident_re.match(objname):
            raise ENAMEID(param_expr_site(obj, 'name'), objname)
        obj.name = objname
    else:
        obj.name = obj.ident
    if obj.objtype == 'device':
        pnode = obj.get_component('_build_confidentiality')
        if pnode and pnode.objtype == 'parameter':
            val = pnode.get_expr(())
            if isinstance(val, IntegerConstant) and val.value > 0:
                dml.globals.build_confidentiality = val.value

    if obj.objtype == 'register':
        if not param_defined(obj, 'size'):
            raise EREGNSZ(obj)

        if param_int(obj, 'size', 4) > 8:
            raise EREGISZ(obj)

    obj_traits = []
    for obj_spec in obj_specs:
        for (issite, tpl) in obj_spec.templates:
            if tpl.trait:
                obj_traits.append((issite, tpl.trait))

    for obj_spec in obj_specs:
        for (templates, spec) in obj_spec.in_eachs:
            each_stmts = each_stmts.copy()
            each_stmts[templates[0]] = each_stmts.get(
                templates[0], []) + [(templates[1:], spec)]

    (shallow_subobjs, composite_subobjs) = ([], [])
    for obj_spec in obj_specs:
        for (preconds, shallow, composite) in obj_spec.blocks:
            if all(eval_precond(cond, obj, global_scope)
                   for cond in preconds):
                if logging.show_porting:
                    PWUNUSED.satisfied_conds.update(preconds)
                shallow_subobjs.append((shallow, obj_spec.rank))
                composite_subobjs.append((composite, obj_spec.rank))

    # name -> list of (Rank, ast.method)
    method_asts = {}
    sessions = {}
    saved = {}
    # list of export stmnts
    exports = []
    for (stmts, rank) in shallow_subobjs:
        for s in stmts:
            if s.kind == 'error':
                _, esite, msg = s
                raise EERRSTMT(esite, msg or "explicit error")
            elif s.kind == 'method':
                (name, _, _, _, _) = s.args
                if name not in method_asts:
                    if name in symbols:
                        report(ENAMECOLL(s.site, symbols[name], name))
                    else:
                        symbols[name] = s.site
                        method_asts[name] = [(rank, s)]
                else:
                    method_asts[name].append((rank, s))
            elif s.kind == 'session':
                (decls, inits) = s.args
                if inits is not None and len(decls) != len(inits):
                    report(ESYNTAX(s.site, None,
                                   'wrong number of initializers:\n'
                                   + f'{len(decls)} variables declared\n'
                                   + f'{len(inits)} initializers specified'))
                else:
                    if inits is None:
                        inits = [None]*len(decls)
                    for (decl_ast, init_ast) in zip(decls, inits):
                        (name, typ_ast) = decl_ast.args
                        if name in symbols:
                            report(ENAMECOLL(s.site, symbols[name], name))
                        else:
                            symbols[name] = s.site
                            sessions[name] = (s.site, name, typ_ast, init_ast)

            elif s.kind == 'saved':
                (decls, inits) = s.args
                if inits is not None and len(decls) != len(inits):
                    report(ESYNTAX(s.site, None,
                                   'wrong number of initializers:\n'
                                   + f'{len(decls)} variables declared\n'
                                   + f'{len(inits)} initializers specified'))
                else:
                    if inits is None:
                        inits = [None]*len(decls)
                    for (decl_ast, init_ast) in zip(decls, inits):
                        (name, typ_ast) = decl_ast.args
                        if name in symbols:
                            report(ENAMECOLL(s.site, symbols[name], name))
                        else:
                            symbols[name] = s.site
                            saved[name] = (s.site, name, typ_ast, init_ast)
            elif s.kind == 'export':
                exports.append(s)
            else:
                raise ICE(s.site, 'UNKNOWN %r' % (s,))

    subobj_defs = {}

    for (stmts, rank) in composite_subobjs:
        for s in stmts:
            (objtype, ident, arrayinfo, subobj_spec) = s

            if ident is None:
                assert (dml.globals.dml_version == (1, 2)
                        and objtype in ['bank', 'field'])

            subobj_def = (objtype, ident, arrayinfo, [subobj_spec])
            if ident in subobj_defs:
                subobj_defs[ident] = merge_subobj_defs(subobj_defs[ident],
                                                       subobj_def, obj)
            elif ident in symbols:
                report(ENAMECOLL(subobj_spec.site, symbols[ident], ident))
            else:
                symbols[ident] = subobj_spec.site
                subobj_defs[ident] = subobj_def

    for (_, _, arrayinfo, specs) in subobj_defs.values():
        for (i, (idx, dimsize_ast)) in enumerate(arrayinfo):
            if dimsize_ast is None:
                report(EAUNKDIMSIZE(specs[0].site, i, idx))
                arrayinfo[i] = (idx, ast.int(specs[0].site, 1))

    explicit_traits = {t for (_, t) in obj_traits}
    ancestors = explicit_traits.union(
        *(t.ancestors for t in explicit_traits))

    for traitset in traits.required_implicit_traits(ancestors):
        if traitset not in implicit_traits:
            direct_parents = traitset.difference(*(
                t.ancestors for t in traitset))
            new_trait = traits.mktrait(
                obj.site, "__implicit_" + "__".join(sorted(
                    t.name for t in direct_parents)),
                traitset, {}, {}, {}, {})
            implicit_traits[traitset] = new_trait
            if new_trait.name in dml.globals.traits:
                raise ICE(
                    obj.site,
                    'implicit trait %s already exists' % (new_trait.name,))
            # We add a trait, but no typedef, to avoid exposing the
            # type to DML code
            dml.globals.traits[new_trait.name] = new_trait
        ancestors.add(implicit_traits[traitset])
    direct_parents = ancestors.difference(*(t.ancestors for t in ancestors))

    trait_method_impls = traits.merge_method_impl_maps(
        obj.site, explicit_traits)

    obj_scope = Location(obj, static_indices(obj))
    # Create all method nodes
    for name in sorted(method_asts):
        if dml.globals.dml_version == (1, 2):
            for trait in ancestors:
                if name in trait.vtable_methods:
                    (tsite, _, _, throws_in_vtable) = trait.vtable_methods[name]
                    # Whether a 1.4 file declares the method as
                    # non-throwing in a vtable
                    vtable_nothrow_dml14 = (
                        tsite if (trait.site.dml_version() == (1, 4)
                                  and not throws_in_vtable)
                        else None)
                    break
            else:
                vtable_nothrow_dml14 = False
        implementations = method_asts[name]
        if (dml.globals.dml_version != (1, 2)
            and name in {
                'register': {'read', 'write', 'read_field', 'write_field'},
                'field': {'read', 'write', 'read_field', 'write_field',
                          'get', 'set'},
            }.get(obj.objtype, set())):
            if dml.globals.traits[name] not in ancestors:
                (_, mast) = implementations[0]
                report(WNOIS(mast.site, name))

        try:
            method = process_method_implementations(
                obj, name, implementations,
                [trait_method_impls[name]]
                if name in trait_method_impls else [],
                obj_specs,
                dml.globals.dml_version == (1, 2) and vtable_nothrow_dml14)
        except DMLError as e:
            report(e)
        else:
            obj.add_component(method)

    if logging.show_porting:
        report_pbefaft(obj, method_asts)

    subobjs = []

    if dml.globals.dml_version == (1, 2):
        # If there are no fields defined, create a field that covers
        # the whole register.
        if obj.objtype == 'register' and not any(
                objtype == 'field'
                for (objtype, _, _, _) in list(subobj_defs.values())):
            # The implicit field instantiates the built-in field
            # template and does nothing else.
            subobjs.append(mkobj(
                None, 'field', (),
                [ObjectSpec(
                    obj.site,
                    # technically this rank is incorrect, but doesn't
                    # matter since it's not applied to any parameters
                    # or methods
                    Rank(set(), RankDesc('verbatim', '<implicit field>')),
                    [(obj.site, dml.globals.templates['field'])], [], [], [])],
                obj, each_stmts))

    for name in sorted(sessions):
        session_spec = sessions[name]
        try:
            subobj = mkdata(session_spec, obj)
        except DMLError as e:
            report(e)
        else:
            subobjs.append(subobj)
    for name in sorted(saved):
        saved_spec = saved[name]
        try:
            subobj = mksaved(saved_spec, obj)
        except DMLError as e:
            report(e)
        else:
            subobjs.append(subobj)

    # map logical subobj name to whatever set the name (ident or name param)
    subobj_name_defs = {}

    for name in sorted(subobj_defs, key=lambda name: name or ''):
        (objtype, ident, arrayinfo, subobj_specs) = subobj_defs[name]
        if (not obj.accepts_child_type(objtype)
            # HACK: disallow non-toplevel banks in DML 1.2, see SIMICS-19009
            or (dml.globals.dml_version == (1, 2)
                and obj.objtype != 'device'
                and objtype == 'bank')):
            report(ENALLOW(subobj_specs[0].site, obj))
            continue
        try:
            subobj = mkobj(ident, objtype, arrayinfo, subobj_specs, obj,
                           each_stmts)
        except DMLError as e:
            report(e)
        else:
            if subobj.name:
                if subobj.name in subobj_name_defs:
                    report(ENAMECOLL(subobj.name_site,
                                     subobj_name_defs[subobj.name],
                                     subobj.name))
                    continue
                subobj_name_defs[subobj.name] = (
                    subobj.name_site if
                    isinstance(subobj, objects.CompositeObject) else
                    subobj.site)
            subobjs.append(subobj)

    for o in subobjs:
        obj.add_component(o)

    # Map name to MethodFunc.
    trait_method_overrides = {}
    # Map name to objects.Parameter.
    trait_param_nodes = {}
    for trait in direct_parents:
        for member in trait.members():
            match = trait.member_declaration(member)
            if match is None:
                # template member invisible to shared methods
                continue
            (decl_site, decl_trait) = match
            member_kind = decl_trait.member_kind(member)
            override = obj.get_component(member)
            if member_kind == 'session':
                # an implicit data object has been added earlier on
                assert override and override.objtype in ('session', 'saved')
                # handled as a special case in vtable initialization
                continue
            if not override:
                if member not in trait.method_impl_traits:
                    # report error: override required by parameter or
                    # abstract method
                    assert member_kind in ['method', 'parameter']
                    for (tsite, t) in obj_traits:
                        if t.implements(decl_trait):
                            raise EABSTEMPLATE(
                                tsite, decl_site, decl_trait.name,
                                member_kind, member)
                    raise ICE(decl_trait.site,
                              'no site found for EABSTEMPLATE(%s)' % (member,))
                continue

            if override.objtype != decl_trait.member_kind(member):
                # e.g. an attempt to override a parameter with a method
                report(ENAMECOLL(override.site, decl_site, member))
                continue

            vtable_trait = trait.vtable_trait(member)
            if member_kind == 'method':
                (tsite, tinp, toutp,
                 tthrows) = vtable_trait.vtable_methods[member]
                if not override.fully_typed:
                    for (n, t) in override.inp:
                        if not t:
                            raise EMETH(
                                override.site, tsite,
                                'input argument declared without a type')
                    if dml.globals.dml_version == (1, 2):
                        # untyped outputs only exist in 1.2
                        for (n, t) in override.outp:
                            if not t:
                                raise EMETH(
                                    override.site, tsite,
                                    'output argument declared without a type')
                    raise ICE(override.site, 'no untyped args')

                traits.typecheck_method_override(
                    (override.site, override.inp, override.outp,
                     override.throws),
                    (tsite, tinp, toutp, tthrows))
                trait_method_overrides[member] = override
            else:
                assert member_kind == 'parameter'
                trait_param_nodes[member] = override

    obj.set_traits(
        traits.ObjTraits(
            obj, ancestors, traits.merge_ancestor_vtables(ancestors, obj.site),
            trait_method_overrides, trait_param_nodes))

    # Fixup some parameters and stuff

    if obj.objtype == 'device':
        banks = obj.get_components('bank')
        if dml.globals.dml_version == (1, 2):
            # Check for duplicate bank functions
            used = {}
            setparam(obj, 'banks', ObjectListParamExpr(
                obj.site, [(b, (), b.dimsizes) for b in banks]))

            for b in banks:
                for indices in all_index_exprs(b):
                    funexpr = param_expr(b, 'function', indices)
                    if defined(funexpr):
                        funnum = expr_intval(funexpr)
                        if funnum in used:
                            report(EDBFUNC(param_expr(b, 'function'),
                                           param_expr(used[funnum], 'function'),
                                           funnum))
                        else:
                            used[funnum] = b
        for b in banks:
            sort_registers(b)
        set_confidential_object(obj)

        if dml.globals.dml_version == (1, 2):
            # top-level parameters defined in 1.4 files can be accessed
            # without $. This allows 'constant' declarations in 1.2 files
            # to be converted compatibly to 'param' in unified 1.2+1.4 files.
            for p in params:
                if (p.site.dml_version() == (1, 4)
                    and global_scope.lookup(p.name) is None):
                    try:
                        global_scope.add(
                            ExpressionSymbol(p.name, p.get_expr(()), p.site))
                    except DMLError:
                        # handled later
                        pass

        # Evaluate all parameters once, to early smoke out non-existing
        # identifiers. TODO: perhaps this should not be done?
        zero_index = (mkIntegerLiteral(obj.site, 0),)
        for param in obj.get_recursive_components('parameter'):
            with ErrorContext(param, None):
                try:
                    try:
                        # Evaluate statically, because it triggers caching
                        # in ASTParamExpr
                        param.get_expr(static_indices(param.parent))
                    except EIDXVAR as e:
                        # Dependency on index variable, re-evaluate at one
                        # index to capture errors early
                        param.get_expr(zero_index * param.dimensions)
                except DMLError as e:
                    if (dml.globals.dml_version == (1, 2)
                        and dml.globals.api_version <= '5'
                        and isinstance(e, EREF)):
                        # We forgive some errors in unused parameters, to
                        # avoid the annoyance caused by hard errors from code
                        # that used to compile fine.
                        #
                        # Note that this error suppression is a hack
                        # which introduces irregularities: The EREF
                        # exception is raised on the first encountered
                        # EREF error; if any other subexpressions
                        # contain other kinds of errors, then those
                        # errors will be suppressed as well.
                        #
                        # In future versions, we should not forgive
                        # broken dead parameters. This is bug 24234.
                        WREF.instances.append(WREF(
                            param.site, param.logname(), e))
                    else:
                        report(e)
        if (dml.globals.dml_version != (1, 2)
            or not dml.globals.compat_dml12):
            # TODO: this should be handled cleaner in the case of pure
            # 1.4 code
            for p in obj.get_components():
                sym = global_scope.lookup(p.name)
                if sym:
                    report(ENAMECOLL(p.site, sym.site, p.name))

        # At this point, methods and subobjs are created and we can
        # try to evaluate their exporting
        for export in exports:
            method_ref_ast, name_ast = (export.args)
            method_ref = codegen_expression_maybe_nonvalue(
                method_ref_ast, obj_scope, global_scope)
            name_expr = codegen_expression(name_ast, obj_scope, global_scope)
            # By continuing we can discover more error, since exports
            # have no effect if invalid
            if not isinstance(method_ref, NodeRef):
                report(ENOBJ(method_ref_ast.site, method_ref))
                continue
            method, indices = method_ref.get_ref()
            if not isinstance(method, objects.Method):
                report(ENMETH(method_ref_ast.site, method))
                continue
            if indices:
                report(EEXPORT(method.site, method))
                continue
            if not method.fully_typed:
                report(EEXPORT(method.site, export.site))
                continue
            name = expr_strval(name_expr)
            if not re.match(r"[A-Za-z_][\w_]*", name):
                report(ENAMEID(name_ast.site, name))
                continue
            if method.throws or len(method.outp) > 1:
                report(EEXPORT(method.site, export.site))
                continue
            func = method_instance(method)
            mark_method_referenced(func)
            mark_method_exported(func, name, export.site)

    elif obj.objtype == 'bank':
        set_confidential_object(obj)
        if logging.show_porting:
            if param_defined(obj, 'function'):
                report(PABSTRACT_TEMPLATE(symbols['function'],
                                          'function_mapped_bank'))

    elif obj.objtype == 'register':
        set_confidential_object(obj)
        if dml.globals.dml_version == (1, 2):
            def field_msb(field):
                (_0, _1, _2, msb1) = field
                return msb1

            fields = sorted(
                register_fields(obj),
                key = field_msb)
            if obj.wholefield:
                if not param_bool(obj, 'allocate', fallback=True):
                    if method_is_std(obj, 'set'):
                        obj.writable = False
                    if method_is_std(obj, 'get'):
                        obj.readable = False
            else:
                if not any(param_bool(f, 'allocate', fallback=True)
                           for (f, _, _, _) in fields):
                    if all(method_is_std(f, 'set') for (f, _, _, _) in fields):
                        obj.writable = False
                    if all(method_is_std(f, 'get') for (f, _, _, _) in fields):
                        obj.readable = False

            # 'fields' might be an empty list, if each field caused
            # DMLError to be raised.
            if fields:
                # Check that the fields don't overlap
                (lsf, _, lsb, _) = fields[0]
                if lsb < 0:
                    raise EBITRR(lsf)
                for ((lsf, li, lsb, _), (msf, mi, _, msb)) in zip(fields[1:],
                                                                  fields[:-1]):
                    if lsb <= msb:
                        reg_indices = (0,) * (lsf.dimensions - len(li))
                        report(EBITRO(lsf, tuple(mkIntegerLiteral(None, i)
                                                 for i in reg_indices + li),
                                      msf, tuple(mkIntegerLiteral(None, i)
                                                 for i in reg_indices + mi)))
                (msf, _, _, msb) = fields[-1]
                if msb >= param_int(obj, 'bitsize'):
                    raise EBITRR(msf)

            # Set the 'fields' parameter
            obj.fields = [field for (field, _, _, _) in fields]
            setparam(obj, 'fields', ObjectListParamExpr(
                obj.site,
                # Always expand all indices: the list order is
                # significant, and two field arrays may be interleaved
                [(f, tuple(mkIntegerLiteral(f.site, i) for i in indices), ())
                 for (f, indices, _, _) in fields]))
        else: # DML version >= 1.4
            if not param_bool(obj, 'writable'):
                obj.writable = False
            if not param_bool(obj, 'readable'):
                obj.readable = False
        if not obj.writable or not obj.readable:
            confparam = param_str(obj, 'configuration')
            if not obj.writable and not obj.readable and confparam != "none":
                raise EANULL(obj.site)
            if confparam in ('required', 'optional'):
                raise EACHK(obj.site)

    elif obj.objtype == 'field':
        if dml.globals.dml_version == (1, 2) and obj.ident == None:
            obj.parent.wholefield = obj
        set_confidential_object(obj)

    elif obj.objtype == 'attribute':
        if not param_defined(obj,
                             'attr_type' if dml.globals.dml_version == (1, 2)
                             else '_attr_type'):
            raise EATYPE(obj)

        if ((dml.globals.dml_version == (1, 2)
             and method_is_std(obj, 'set_attribute')
             and not param_defined(obj, 'allocate_type')
             and method_is_std(obj, 'set'))
            or (dml.globals.dml_version != (1, 2)
                and not param_bool(obj, 'writable'))):
            obj.writable = False
        if ((dml.globals.dml_version == (1, 2)
             and method_is_std(obj, 'get_attribute')
             and not param_defined(obj, 'allocate_type')
             and method_is_std(obj, 'get'))
            or (dml.globals.dml_version != (1, 2)
                and not param_bool(obj, 'readable'))):
            obj.readable = False

        if not obj.writable or not obj.readable:
            confparam = param_str(obj, 'configuration')
            if not obj.writable and not obj.readable and confparam != "none":
                raise EANULL(obj.site)
            if confparam in ('required', 'optional'):
                raise EACHK(obj.site)

        if logging.show_porting and dml.globals.dml_version == (1, 2):
            param = obj.get_component('allocate_type')
            atype_expr = param.get_expr(static_indices(obj))
            if defined(atype_expr):
                atype = expr_strval(atype_expr)
                type_param = obj.get_component('type')
                # if 'parameter type = "i"' appears together with allocate_type,
                # then remove that as well
                if type_param.site.filename() == param.site.filename():
                    type_site = type_param.site
                else:
                    type_site = None
                # integer attributes of all sizes are converted to
                # the corresponding 64-bit type
                if atype.startswith('uint'):
                    report(PATTRIBUTE(obj.site, 'uint64_attr', param.site,
                                      type_site))
                elif atype.startswith('int'):
                    report(PATTRIBUTE(obj.site, 'int64_attr', param.site,
                                      type_site))
                elif atype in ('double', 'bool'):
                    report(PATTRIBUTE(obj.site, atype + '_attr', param.site,
                                      type_site))

    elif obj.objtype == 'connect':
        if dml.globals.dml_version == (1, 2):
            interfaces = obj.get_components('interface')
            assert all(i.dimensions == obj.dimensions for i in interfaces)
            setparam(obj, 'interfaces', ObjectListParamExpr(
                obj.site, [(i, (), ()) for i in interfaces]))

    elif obj.objtype == 'interface':
        typename = param_str(obj, 'c_type' if dml.globals.dml_version == (1, 2)
                             else '_c_type')
        t = TPtr(TNamed(typename, const=True))
        t.declaration_site = obj.site
        try:
            realtype(t)
        except DMLUnknownType:
            raise EIFTYPE(obj, t)

    elif obj.objtype == 'event':
        if (dml.globals.dml_version == (1, 2)
            and param_str(obj, 'timebase') == 'stacked'):
            report(WDEPRECATED(obj.get_component('timebase').site,
                               "stacked events are deprecated"))
        if logging.show_porting and dml.globals.dml_version == (1, 2):
            timebase = param_str(obj, 'timebase')
            param_site = obj.get_component('timebase').site
            if param_site.filename().endswith('dml-builtins.dml'):
                param_site = None
            tpl = ('custom_time_event' if timebase == 'seconds'
                   else 'custom_cycle_event')
            report(PEVENT(obj.site, tpl, param_site, None))

    return obj

def set_confidential_object(obj):
    expr = param_expr(obj, '_confidentiality')
    val = expr_intval(expr) if defined(expr) else 0
    if (val - dml.globals.build_confidentiality) > 0:
        obj._confidential = True
        if obj.objtype in ('register', 'field') and obj.name:
            setparam(obj, 'name', SimpleParamExpr(mkHiddenName(
                obj.site, obj.name, obj)))
            setparam(obj, 'qname', HiddenQNameParamExpr(obj))

def get_register_registers(reg, indexvars):
    # Get a flat list of all individual registers in a potential
    # register array. Return it as list of NodeRef objects
    # Obtain all combinations of indices into a potentially
    # multi-dimensional array
    index_crossproduct = itertools.product(*(
        (mkIntegerLiteral(reg.site, i) for i in range(array_len))
        for array_len in reg.arraylens()))
    return [mkNodeRef(reg.site, reg,
                      indexvars +
                      indices) for indices in index_crossproduct]

def get_group_registers(group, indexvars):
    # Get a flat list of all individual registers in a bank/group.
    # Return it as list of NodeRef objects
    subnodes = group.get_components('group', 'register')

    regs = []

    if group.isindexed():
        # Obtain all combinations of indices into a potentially
        # multi-dimensional array
        index_crossproduct = itertools.product(*(
            (mkIntegerLiteral(None, i) for i in range(array_len))
            for array_len in group.arraylens()))
        for indices in index_crossproduct:
            subindexvars = indexvars + indices
            for node in subnodes:
                if node.objtype == 'group':
                    regs.extend(get_group_registers(node, subindexvars))
                else:
                    regs.extend(get_register_registers(node, subindexvars))
    else:
        for node in subnodes:
            if node.objtype == 'group':
                regs.extend(get_group_registers(node, indexvars))
            else:
                regs.extend(get_register_registers(node, indexvars))

    return regs

def param_linear_int(param):
    '''If possible, return tuple of coefficients (i0, i1, .., iN, k) such
    that param's value can be equivalently expressed as
    (i0*v0 + i1*v1 + ... + iN*vN + k), in DML's 64-bit arithmetic,
    where vN are index variables. Return None if no such
    representation can be found.

    If a tuple is returned, then the parameter value either has type
    uint64 or int64.
    '''
    class IndexVar(Expression):
        type = TInt(64, True)
        @slotsmeta.auto_init
        def __init__(self, site, variables): pass

    indices = tuple(IndexVar(param.site,
                             tuple(1 if x == i else 0
                                   for x in range(param.dimensions + 1)))
                    for i in range(param.dimensions))
    class NotLinear(Exception): pass
    def expr_linear_int(expr):
        if isinstance(expr, ctree.Add):
            return tuple(l + r
                         for (l, r) in zip(expr_linear_int(expr.lh),
                                           expr_linear_int(expr.rh)))
        elif isinstance(expr, ctree.Mult):
            l = expr_linear_int(expr.lh)
            r = expr_linear_int(expr.rh)
            if any(r[:-1]):
                if any(l[:-1]):
                    # bilinear
                    raise NotLinear()
                # normalize: all variables in 'l'
                (l, r) = (r, l)
            return tuple(x * r[-1] for x in l)
        elif isinstance(expr, ctree.Subtract):
            return tuple(l - r
                         for (l, r) in zip(expr_linear_int(expr.lh),
                                           expr_linear_int(expr.rh)))
        elif isinstance(expr, ctree.IntegerConstant):
            return (0,) * param.dimensions + (expr.value,)
        elif isinstance(expr, IndexVar):
            return expr.variables
        elif (isinstance(expr, ctree.Cast) and expr.type.is_int
              and expr.type.bits == 64):
            return expr_linear_int(expr.expr)
        raise NotLinear()

    try:
        expr = param.get_expr(indices)
    except DMLError:
        return None
    try:
        return tuple(c & ((1 << 64) - 1) for c in expr_linear_int(expr))
    except NotLinear:
        return None

def sort_registers(bank):
    # TODO: partition_registers is expensive, and all it gives in 1.4
    # is to check for errors such as EREGOL. It would be better with a
    # function that only checks for those problems, but which yields a
    # warning instead. That way, we can naturally allow the
    # (expensive) check to be skipped by disabling the warning. See US1610.
    if dml.globals.dml_version == (1, 2):
        (bank.mapped_registers, bank.unmapped_registers,
         bank.numbered_registers) = explode_registers(bank)
        setparam(bank, 'mapped_registers', ObjectListParamExpr(
            bank.site,
            [instance
             for reg in bank.mapped_registers
             for instance in reg.node_instances()]))

        setparam(bank, 'unmapped_registers', ObjectListParamExpr(
            bank.site,
            [instance
             for reg in bank.unmapped_registers
             for instance in reg.node_instances()]))

        setparam(bank, 'numbered_registers', ObjectListParamExpr(
            bank.site,
            [instance
             for reg in bank.numbered_registers
             for instance in reg.node_instances()]))
    else:
        def reg_coords(reg):
            return itertools.product(
                *(range(i) for i in reg.dimsizes[bank.dimensions:]))
        def explode_offsets(reg):
            param = reg.get_component('offset')
            assert param.objtype == 'parameter'
            coeffs = param_linear_int(param)
            if coeffs and any(coeffs[:bank.dimensions]):
                # offset depends linearly on bank index. Delegate to
                # the code for the non-linear case to report this as an error.
                coeffs = None
            if coeffs is None:
                bank_indices = tuple(StaticIndex(reg.site, var)
                                     for var in bank.idxvars())
                return [(param_int(reg, 'offset',
                                   indices = bank_indices + reg_indices), coord)
                        for (reg_indices, coord) in zip(
                                itertools.product(
                                    *([mkIntegerLiteral(reg.site, i)
                                       for i in range(dimsize)]
                                      for dimsize in reg.dimsizes[
                                              bank.dimensions:])),
                                reg_coords(reg))]
            else:
                return [
                    (coeffs[-1] + sum(a * b for (a, b)
                                      in zip(coeffs[bank.dimensions:], coord)),
                     coord)
                    for coord in reg_coords(reg)]

        # list of quadruples (start, end, DMLObject, coord)
        # where coord is a tuple integer indices of this instance,
        # and [start, end] is a half-open offset interval.
        all_offsets = []
        mask = (1 << 64) - 1
        for reg in bank.get_recursive_components('register'):
            size = param_int(reg, 'size')
            all_offsets.extend((offset & mask, (offset + size) & mask,
                                reg, coord)
                               for (offset, coord) in explode_offsets(reg))
        all_offsets.sort(key=lambda t: t[0])
        while all_offsets and all_offsets[-1][0] == mask:
            del all_offsets[-1]
        for (a, b) in zip(all_offsets, all_offsets[1:]):
            if a[1] > b[0]:
                report(EREGOL(a[2], b[2], a[3], b[3]))

class ParentParamExpr(objects.ParamExpr):
    def __init__(self, obj):
        self.obj = obj
    @property
    def site(self): return self.obj.site
    def mkexpr(self, indices):
        assert len(indices) == self.obj.dimensions
        if self.obj.isindexed():
            indices = indices[:-self.obj.local_dimensions()]
        return mkNodeRef(self.obj.site, self.obj.parent, indices)

class QNameParamExpr(objects.ParamExpr):
    def __init__(self, node, relative):
        assert node
        self.node = node
        self.relative = relative
    @property
    def site(self): return self.node.site
    def mkexpr(self, indices):
        return QName(self.node.site, self.node, self.relative, indices)

class HiddenQNameParamExpr(objects.ParamExpr):
    def __init__(self, node):
        self.node = node
    @property
    def site(self): return self.node.site
    def mkexpr(self, indices):
        return HiddenQName(self.node.site, self.node, indices)

class ObjectListParamExpr(objects.ParamExpr):
    '''List of DML objects, using a common parent'''
    __slots__ = ('site', 'instances')
    def __init__(self, site, instances):
        self.site = site
        self.instances = instances
    def mkexpr(self, indices):
        return mkObjectList(
            self.site,
            [(node, indices + parent_relative_indices, dimsizes)
             for (node, parent_relative_indices, dimsizes) in self.instances])

class ASTParamExpr(objects.ParamExpr):
    '''Expression AST, codegen:ed on lookup'''
    __slots__ = ('ast', 'parent', 'cached')
    params_on_stack = []
    def __init__(self, ast, parent):
        self.ast = ast
        self.parent = parent
        self.cached = None
    @property
    def site(self): return self.ast.site
    def mkexpr(self, indices):
        if self.cached:
            return self.cached
        if self in self.params_on_stack:
            raise ERECPARAM([pe.ast.site for pe in self.params_on_stack[
                self.params_on_stack.index(self):]])
        self.params_on_stack.append(self)
        try:
            expr = codegen_expression_maybe_nonvalue(
                self.ast, Location(self.parent, indices), global_scope)
        finally:
            popped = self.params_on_stack.pop()
            assert popped is self
        if all(isinstance(i, StaticIndex) for i in indices) and expr.constant:
            # cannot possibly evaluate to a different value
            self.cached = expr
        return expr

class UninitializedParamExpr(objects.ParamExpr):
    __slots__ = ('site', 'name')
    def __init__(self, site, name):
        self.site = site
        self.name = name
    def mkexpr(self, indices):
        raise EUNINITIALIZED(self.site, self.name)

class SimpleParamExpr(objects.ParamExpr):
    '''A parameter expression, using a simple index-free Expression as
    value'''
    __slots__ = ('expr',)
    def __init__(self, expr):
        self.expr = expr
    @property
    def site(self): return self.expr.site
    def mkexpr(self, indices):
        return self.expr

class EventClassParamExpr(objects.ParamExpr):
    '''The evclass parameter of an event object'''
    __slots__ = ('node',)

    def __init__(self, node):
        assert node.objtype == 'event'
        self.node = node

    @property
    def site(self): return self.node.site

    def mkexpr(self, indices):
        for i in indices:
            if isinstance(i, NonValue):
                # implicit reference to index variable
                raise i.exc()
        expr = crep.get_evclass(self.node)
        # add an extra *& prefix to make DML and C agree that this is an lvalue
        return mkDereference(self.node.site, mkLit(self.node.site,
                     '&%s%s' % (
                         expr, ''.join('[' + i.read() + ']' for i in indices)),
                     TPtr(TPtr(TNamed('event_class_t')))))

class IndexParamExpr(objects.ParamExpr):
    '''Parameter containing a simple index'''
    __slots__ = ('dimension', 'varname', 'site')
    def __init__(self, site, dimension, varname):
        self.dimension = dimension
        self.varname = varname
        self.site = site
    def mkexpr(self, indices):
        if self.dimension > len(indices) - 1:
            raise ICE(self.site, "Too few indices given: %s" % (indices,))
        idx = indices[self.dimension]
        return idx

class IndexListParamExpr(objects.ParamExpr):
    '''Parameter containing a list of IndexParamExpr'''
    __slots__ = ('site', 'params')
    def __init__(self, site, params):
        for p in params:
            assert(isinstance(p, IndexParamExpr))
        self.params = params
        self.site = site
    def mkexpr(self, indices):
        return mkList(self.site,
                      [param.mkexpr(indices) for param in self.params])

def mkparam(obj, autoparams, param):
    _, site, name, (value, default, auto) = param

    # auto-parameters are like normal "hard" assignments
    if name in autoparams:
        if not auto and not default:
            # seems that dml-builtins lacks a declaration
            # 'parameter ... auto;'
            raise ICE(site, 'missing auto declaration: %s' % (name,))
        return objects.DMLParameter(
            name, site, obj, autoparams[name])
    elif auto:
        # user-supplied 'auto' declaration
        raise EAUTOPARAM(site, name)

    if value is None:
        value = default

    # caught earlier, ENPARAM
    assert value is not None

    return objects.DMLParameter(name, site, obj, ASTParamExpr(value, obj))

def port_builtin_method_overrides(name, site, inp_ast, parent_obj):
    if ((name in ['read', 'write']
         and parent_obj.objtype in ['register', 'field'])
        or (name in ['get', 'set', 'read_access', 'write_access']
            and parent_obj.objtype == 'field')
        or (name in ['hard_reset', 'soft_reset']
            and parent_obj.objtype in ['bank', 'device'])):
        tpl_name = {'read_access': 'read_field',
                    'write_access': 'write_field'}.get(name, name)
        report(PABSTRACT_TEMPLATE(site, tpl_name))

    known_signatures = {
        'bank': {
            # remove difficult args to trigger errors, and rely on PTRAMPOLINE
            'access': ('old_access', [(0, 'generic_transaction_t *'),
                                      (1, 'uint64 '),
                                      (2, 'uint64 ')], True),
            'read_access_memop': ('old_read_access_memop',
                                  [(0, 'generic_transaction_t *'),
                                   (1, 'uint64 '),
                                   (2, 'uint64 ')], True),
            'write_access_memop': ('old_write_access_memop',
                                   [(0, 'generic_transaction_t *'),
                                    (1, 'uint64 '),
                                    (2, 'uint64 '),
                                    (3, 'uint64 ')], True),
            'read_access': ('old_read_access', [(1, 'uint64 ')], False),
            'write_access': ('old_write_access', [(1, 'uint64 '),
                                                  (3, 'uint64 ')], False),
            'miss_access': ('old_miss_access', [(1, 'uint64 ')], True),
            'miss_read_access': ('old_miss_read_access',
                                 [(0, 'uint64 ')], False),
            'miss_write_access': ('old_miss_write_access',
                                  [(0, 'uint64 '), (2, 'uint64 ')], False),
        },
        'connect': {
            'validate_port': ('validate', [(0, 'conf_object_t *')], False)
        },
        'attribute': {
            'set': ('set', [(0, 'attr_value_t ')], True)
        },
        'register': {
            'set': ('set', [(0, 'uint64 ')], False),
            'write': ('write', [(0, 'uint64 ')], False),
            'read_access': ('read_register', [('enabled_bytes', 'uint64 '),
                                              ('aux', 'void *')], False),
            'write_access': ('write_register', [(3, 'uint64 '),
                                                ('enabled_bytes', 'uint64 '),
                                                ('aux', 'void *')], False),
        },
        'field': {
            'set': ('set', [(0, 'uint64 ')], False),
            'write': ('write', [(0, 'uint64 ')], False),

            'read_access': ('read_field', [('enabled_bits', 'uint64 '),
                                           ('aux', 'void *')], False),
            'write_access': ('write_field', [(0, 'uint64 '),
                                             ('enabled_bits', 'uint64 '),
                                             ('aux', 'void *')], False),
        },
    }
    methods = known_signatures.get(parent_obj.objtype, {})
    if name in methods:
        (new_name, args, throws) = methods[name]
        new_inp = []
        for (old_idx, new_type) in args:
            if isinstance(old_idx, int):
                old_arg = inp_ast[old_idx]
                (n, _) = old_arg.args
            else:
                n = old_idx
            new_inp.append(new_type + n)
        report(PCHANGE_INARGS(site, 'method %s(%s)%s' % (
            new_name, ', '.join(new_inp), ' throws' if throws else '')))
    # Mapping objkind -> (1.4 trampoline, 1.2 trampoline)
    # Two cases:
    #
    # 1. Neither trampoline is None: The 1.2 method communicates
    #    errors with exceptions, while the 1.4 method uses return
    #    value. In this case, known_signatures renames the original
    #    implementation to old_* and removes any 1.4-incompatible
    #    args, and we create a 1.4 trampoline that translates the exception
    #    into a return value. We also create a 1.2 trampoline that calls the
    #    1.4 method.
    #
    #    The user can then move old_* into the 1.4 method and change it
    #    to use a return value.
    #
    # 2. 1.4 trampoline is None: The 1.2 method signature can be translated
    #    to work in 1.4; we only need to create a trampoline to call this from
    #    DML 1.2.
    trampolines = {
        'bank': {
            'access': ('''\
method io_memory_access(generic_transaction_t *memop, uint64 offset, void *aux) -> (bool) {
    // TODO: further conversion likely needed; probably move
    // old_access implementation into this method
    try {
        old_access(memop, offset, SIM_get_mem_op_size(memop));
    } catch {
        return false;
    }
    return true;
}''', 'is dml12_compat_io_memory_access;'),
            'miss_read_access': ('''\
method unmapped_read(uint64 offset, uint64 bits, void *aux)
    -> (uint64) throws {
    local bool ok;
    local uint64 value;
    (ok, value) = old_miss_read_access(offset);
    if (!ok)
        throw;
    return value;
}''', '''\
method miss_read_access(uint64 offset, uint64 size) -> (bool, uint64) {
    try {
        local uint64 value;
        value = unmapped_read(offset, (~0)[size * 8 - 1 : 0], NULL);
        return (true, value);
    } catch {
        return (false, 0);
    }
}'''),
            'miss_write_access': ('''\
method unmapped_write(uint64 offset, uint64 bits, uint64 value, void *aux) throws {
    if (!old_miss_write_access(offset, value))
        throw;
}''', '''\
method miss_write_access(uint64 offset, uint64 size, uint64 value) -> (bool) {
    try {
        unmapped_write(offset, value, (~0)[size * 8 - 1 : 0], NULL);
        return true;
    } catch {
        return false;
    }
}'''),
            'miss_access': ('''\
method unmapped_read(uint64 offset, uint64 bits, void *aux)
    -> (uint64) throws {
    // TODO: manual conversion likely needed
    old_miss_access(offset);
    return 0;
}

method unmapped_write(uint64 offset, uint64 bits, uint64 value, void *aux) throws {
    // TODO: manual conversion likely needed
    old_miss_access(offset);
}''', '''\
inline method miss_access(inline memop, inline offset, inline size) throws {
    if (SIM_mem_op_is_read(memop)) {
        local uint64 value;
        value = unmapped_read(offset, (~0)[size * 8 - 1 : 0], NULL);
    } else {
        local uint64 value;
        value = get_write_value(memop);
        unmapped_write(offset, value, (~0)[size * 8 - 1 : 0], NULL);
    }
}'''),
            'read_access': ('''\
method read(uint64 offset, uint64 enabled_bytes, void *aux) -> (uint64) throws {
    local bool ok;
    local uint64 value;
    // TODO: further conversion likely needed; probably move
    // old_read_access implementation into this method
    (ok, value) = old_read_access(offset);
    if (!ok) {
        throw;
    }
    return value;
}''', '''\
method read_access(generic_transaction_t *memop, physical_address_t offset,
                   physical_address_t size)
    -> (bool, uint64) {
    try {
        local uint64 value;
        value = read(offset, (1 << (size * 8)) - 1, NULL);
        return (true, value);
    } catch {
        return (false, 0);
    }
}'''),
            'read_access_memop': ('''\
method io_memory_access(generic_transaction_t *memop, uint64 offset, void *aux) -> (bool) {
    // TODO: further conversion likely needed; probably move
    // old_read_access_memop implementation into this method
    try {
        old_read_access_memop(memop, offset, SIM_get_mem_op_size(memop));
    } catch {
        return false;
    }
    return true;
}''', 'is dml12_compat_io_memory_access;'),
            'write_access': ('''\
method write(uint64 offset, uint64 enabled_bytes, uint64 value, void *aux) throws {
    // TODO: further conversion likely needed; probably move
    // old_write_access implementation into this method
    if (!old_write_access(offset, value)) {
        throw;
    }
}''', '''\
method write_access(generic_transaction_t *memop, physical_address_t offset,
                    physical_address_t size, uint64 writevalue) -> (bool) {
    try {
        write(offset, (1 << (size * 8)) - 1, writevalue, NULL);
        return true;
    } catch {
        return false;
    }
}'''),
            'write_access_memop': ('''\
method io_memory_access(generic_transaction_t *memop, uint64 offset, void *aux) -> (bool) {
    // TODO: further conversion likely needed; probably move
    // old_write_access_memop implementation into this method
    try {
        old_write_access_memop(memop, offset, SIM_get_mem_op_size(memop),
                               this.byte_order == "little-endian"
                               #? SIM_get_mem_op_value_le(memop)
                               #: SIM_get_mem_op_value_be(memop));
    } catch {
        return false;
    }
    return true;
}''', 'is dml12_compat_io_memory_access;'),
        },
        'field': {
            'read_access': (None, 'is dml12_compat_read_field;'),
            'write_access': (None, 'is dml12_compat_write_field;'),
        },
        'register': {
            'read_access': (None, 'is dml12_compat_read_register;'),
            'write_access': (None, 'is dml12_compat_write_register;'),
        },
    }
    trampoline_methods = trampolines.get(parent_obj.objtype, {})
    if name in trampoline_methods:
        (tramp14, tramp12) = trampoline_methods[name]
        if 'is dml12_compat' in tramp12:
            report(PIMPORT_DML12COMPAT(site))
        else:
            tramp12 = ('#if (dml_1_2) {\n    %s\n}'
                       % (tramp12.replace('\n', '\n    ')))
        report(PTRAMPOLINE(site, tramp14, tramp12))

def mkmethod(site, rbrace_site, location, parent_obj, name, inp_ast,
             outp_ast, throws, body, default, default_level):
    # check for duplicate parameter names
    named_args = inp_ast
    if body.site.dml_version() == (1, 2):
        named_args = named_args + outp_ast
    argnames = set()
    for (_, tsite, n, t) in named_args:
        if n in argnames:
            raise EARGD(tsite, n)
        argnames.add(n)

    if logging.show_porting and body.site.dml_version() == (1, 2):
        # Untyped output args are forbidden in 1.4; make sure to
        # insert a type. For now, we guess uint64, because that will
        # work most of the time. We could make a more qualified guess,
        # e.g. based on parameter name or actual uses, but untyped
        # non-integer outputs seem to be rather uncommon.
        for (_, psite, pname, ptype) in outp_ast:
            if not ptype:
                report(PTYPEDOUTPARAM(psite, 'uint64'))
        if not isinstance(default, InvalidDefault):
            port_builtin_method_overrides(name, site, inp_ast, parent_obj)
            if (default.node and default.node.throws
                and default.node.site.dml_version() == (1, 4)):
                report(PTHROWS(body.site))

    inp = eval_method_inp(inp_ast, location, global_scope)
    outp = eval_method_outp(outp_ast, location, global_scope)

    for (n, t) in inp + outp:
        if t:
            check_named_types(t)
            t = realtype(t)
            if t.is_int and t.is_endian:
                raise EEARG(site)

    for (n, t) in outp:
        # See SIMICS-19028
        if t and deep_const(t):
            raise ICE(site,
                      'Methods with (partially) const output/return '
                      + 'values are not yet supported.')
    if default_level:
        name += "___default%d" % (default_level,)
    method = objects.Method(name, site, parent_obj,
                            inp, outp, throws, body, default,
                            rbrace_site)
    if logging.show_porting:
        if method.fully_typed:
            PWUNUSED.typed_methods.add(method)
        else:
            PWUNUSED.inline_methods[method.site] = method
    return method
