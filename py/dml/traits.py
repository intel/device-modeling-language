# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

# Infrastructure for traits

import itertools
import functools
import contextlib
import abc
import os
from . import objects, logging, crep, codegen, toplevel, topsort, compat
from .logging import *
from .codegen import *
from .symtab import *
from .ctree import *
from .expr import *
from .expr_util import *
from .messages import *
from .slotsmeta import auto_init
from .types import *
from .set import Set
import dml.globals

__all__ = (
    'merge_method_impl_maps',
    'merge_ancestor_vtables',
    'typecheck_method_override',
    'ObjTraits',
    'TraitObjMethod',
    'mktrait',
    'NoDefaultSymbol',
    'AmbiguousDefaultSymbol',
)

def process_trait(site, name, subasts, ancestors, template_symbols):
    # Methods declared in this trait
    methods = {}
    params = {}
    sessions = {}
    hooks = {}
    def check_namecoll(name, site):
        if name in methods:
            (othersite, _, _, _, _, _, _, _, _, _) = methods[name]
            raise ENAMECOLL(site, othersite, name)
        if name in params:
            (othersite, _) = params[name]
            raise ENAMECOLL(site, othersite, name)
        if name in sessions:
            (othersite, _) = sessions[name]
            raise ENAMECOLL(site, othersite, name)
        if name in hooks:
            (othersite, _, _) = hooks[name]
            raise ENAMECOLL(site, othersite, name)

    for ast in subasts:
        try:
            if ast.kind == 'sharedmethod':
                (mname, inp_asts, outp_asts, throws, qualifiers,
                 overridable, body, rbrace_site) = ast.args
                independent = 'independent' in qualifiers
                startup = 'startup' in qualifiers
                memoized = 'memoized' in qualifiers
                if (startup and not independent) or (memoized and not startup):
                    raise ICE(impl.site,
                              'Invalid qualifier combination: '
                              + ' '.join(['independent']*independent
                                         + ['startup']*startup
                                         + ['memoized']*memoized))
                inp = eval_method_inp(inp_asts, None, global_scope)
                outp = eval_method_outp(outp_asts, None, global_scope)
                check_namecoll(mname, ast.site)
                methods[mname] = (ast.site, inp, outp, throws, independent,
                                  startup, memoized, overridable, body,
                                  rbrace_site)
            elif ast.kind in {'session', 'saved'}:
                (decls, _) = ast.args
                for decl_ast in decls:
                    (sname, type_ast) = decl_ast.args
                    (struct_defs, stype) = eval_type(
                        type_ast, ast.site, None, global_scope)
                    add_late_global_struct_defs(struct_defs)
                    check_namecoll(sname, ast.site)
                    sessions[sname] = (ast.site, stype)
            elif ast.kind == 'param':
                (pname, type_info, _, _) = ast.args
                assert type_info.kind == 'paramtype'
                (type_ast,) = type_info.args
                (struct_defs, ptype) = eval_type(type_ast, type_info.site, None,
                                                 global_scope)
                # this would be trivial to support, but completely meaningless
                for (err_site, _) in struct_defs:
                    report(EANONSTRUCT(err_site, "parameter type"))
                check_namecoll(pname, ast.site)
                params[pname] = (ast.site, ptype)
            elif ast.kind == 'hook':
                (hname, arraylen_asts, type_asts) = ast.args

                msg_types = []
                for type_ast in type_asts:
                    (struct_defs, dtype) = eval_type(type_ast, site, None,
                                                     global_scope)
                    add_late_global_struct_defs(struct_defs)
                    # TODO maybe realtype?
                    msg_types.append(dtype)
                array_lens = tuple(eval_arraylen(len_ast, global_scope)
                                   for len_ast in arraylen_asts)
                check_namecoll(hname, ast.site)
                hooks[hname] = (ast.site, array_lens, msg_types)
            else:
                raise ICE(ast.site, 'unknown ast')
        except DMLError as e:
            report(e)
    return mktrait(site, name, ancestors, methods, params, sessions, hooks,
                   template_symbols)

class NoDefaultSymbol(Symbol):
    """A broken reference to 'default' inside a method that has no default
    method. This is represented as an explicit symbol in order to
    report ENDEFAULT instead of the slightly less informative
    EIDENT."""
    def __init__(self, site):
        super(NoDefaultSymbol, self).__init__('default', site=site)
    def expr(self, site):
        raise ENDEFAULT(site)

class AmbiguousDefaultSymbol(Symbol):
    """A broken reference to 'default' inside a method that has two
    possible parent methods. This is represented as an explicit
    symbol in order to report EAMBDEFAULT instead of the slightly
    less informative EIDENT."""
    def __init__(self, default_method_sites):
        super(AmbiguousDefaultSymbol, self).__init__(
            'default', site=default_method_sites[0])
        self.default_method_sites = default_method_sites
    def expr(self, site):
        raise EAMBDEFAULT(site, self.default_method_sites)

class TraitVTableItem(metaclass=abc.ABCMeta):
    '''A value for a struct field in a vtable instance'''
    __slots__ = ()
    @abc.abstractproperty
    def vtable_trait(self): '''The trait to whose vtable the item belongs'''
    @abc.abstractproperty
    def name(self): '''Field name in vtable struct'''

class TraitMethod(TraitVTableItem):
    __slots__ = ('site', 'inp', 'outp', 'throws', 'independent', 'startup',
                 'memoized', 'astbody', 'name', 'trait', 'default_traits',
                 'overridable', 'rbrace_site', '_memo_outs_struct', 'id')

    def __init__(self, site, inp, outp, throws, independent, startup, memoized,
                 overridable, astbody, trait, name, default_traits,
                 rbrace_site):
        self.site = site
        self.inp = inp
        self.outp = outp
        self.throws = throws
        self.independent = independent
        self.startup = startup
        self.memoized = memoized
        self.overridable = overridable
        self.astbody = astbody
        self.trait = trait
        self.name = name
        # the traits whose implementation we override
        self.default_traits = default_traits
        self.rbrace_site = rbrace_site
        self._memo_outs_struct = None

    def __str__(self):
        return '%s.%s' % (self.trait.name, self.name)

    def cname(self):
        '''Name of the C identifier for this method'''
        return '_DML_TM_%s__%s' % (self.trait.name, self.name)

    @property
    def memo_outs_struct(self):
        assert self.memoized
        if self._memo_outs_struct is None:
            memo_dict = {'p_' + name: typ for (name, typ) in self.outp}
            memo_dict['ran'] = TInt(8, True)
            if self.throws:
                memo_dict['threw'] = TBool()
            self._memo_outs_struct = TStruct(
                memo_dict, label=f'_memo_{self.trait.name}__{self.name}')
        return self._memo_outs_struct

    @property
    def vtable_trait(self):
        '''The type of the implicit trait argument. The method will end up
        in the vtable of this trait (except if it's a method that doesn't end
        up in a vtable)'''
        if self.name in self.trait.vtable_methods:
            return self.trait
        if self.name in self.trait.ancestor_vtables:
            return self.trait.ancestor_vtables[self.name]
        else:
            # not present in vtables
            assert not self.overridable
            return self.trait

    def downcast_path(self):
        vtable_trait = self.vtable_trait
        if vtable_trait is self.trait:
            return ()
        else:
            return self.trait.ancestry_paths[vtable_trait][0]

    def declaration(self):
        implicit_inargs = self.vtable_trait.implicit_args()
        args = ", ".join(t.declaration(n)
                         for (n, t) in c_inargs(
                                 crep.maybe_dev_arg(self.independent)
                                 + implicit_inargs + list(self.inp),
                                 self.outp, self.throws))
        return c_rettype(self.outp, self.throws).declaration(
            '%s(%s)' % (self.cname(), args))

    def codegen_body(self):
        with (crep.DeviceInstanceContext()
              if not self.independent else contextlib.nullcontext()):
            scope = MethodParamScope(self.trait.scope(global_scope))
            implicit_inargs = self.vtable_trait.implicit_args()
            site = SimpleSite(self.site.loc())
            if len(self.default_traits) > 1:
                default = AmbiguousDefaultSymbol(
                    [trait.method_impls[self.name].site
                     for trait in self.default_traits])
            elif self.default_traits:
                [default_trait] = self.default_traits
                default_method = default_trait.method_impls[self.name]
                [(name, typ)] = implicit_inargs
                default = ExpressionSymbol(
                    'default',
                    TraitMethodDirect(
                        default_method.site,
                        mkLit(site, cident(name), typ),
                        default_method),
                    site)
            else:
                default = NoDefaultSymbol(self.site)

            for (n, t) in self.outp:
                # See SIMICS-19028
                if deep_const(t):
                    raise ICE(self.site,
                              'Methods with (partially) const output/return '
                              + 'values are not yet supported.')

            if self.memoized:
                assert self.independent and self.startup
                memoization = codegen.SharedIndependentMemoized(self)
            else:
                memoization = None
            body = codegen_method(
                self.astbody.site, self.inp, self.outp, self.throws, self.independent,
                memoization, self.astbody, default,
                Location(dml.globals.device, ()), scope, self.rbrace_site)

            downcast_path = self.downcast_path()
            if downcast_path:
                trait_decl = mkInline(
                    site,
                    '%s UNUSED = DOWNCAST(%s, %s, %s);' % (
                        self.trait.type().declaration('_' + cident(self.trait.name)),
                        '_' + cident(self.vtable_trait.name),
                        cident(self.trait.name),
                        '.'.join(cident(t.name) for t in downcast_path)))
                body = mkCompound(site, [trait_decl, body])
            return body

def merge_ancestor_vtables(ancestors, site):
    ancestor_vtables = {}
    for ancestor in ancestors:
        for name in itertools.chain(
                ancestor.vtable_methods, ancestor.vtable_params,
                ancestor.vtable_sessions, ancestor.vtable_hooks,
                ancestor.vtable_memoized_outs):
            if name in ancestor_vtables:
                # This may mean that an abstract method or parameter is
                # defined in two traits. We could allow this, as long
                # as types match, and it's overridden in an unambiguous way.
                report(EAMBINH(site, None, name,
                               ancestor.name, ancestor_vtables[name].name))
            else:
                ancestor_vtables[name] = ancestor
    return ancestor_vtables

def mktrait(site, tname, ancestors, methods, params, sessions, hooks,
            template_symbols):
    '''Produce a trait, possibly reporting errors'''
    direct_parents = [a for a in ancestors
                      if not any(a in p.ancestors for p in ancestors)]
    ancestor_method_impls = merge_method_impl_maps(site, direct_parents)

    ancestor_vtables = merge_ancestor_vtables(ancestors, site)

    # a parameter declaration cannot override anything
    bad_params = []
    for name in params:
        for ancestor in direct_parents:
            coll = ancestor.member_declaration(name)
            if coll:
                (orig_site, _) = coll
                (param_site, _) = params[name]
                report(ENAMECOLL(param_site, orig_site, name))
                bad_params.append(name)
    for name in bad_params:
        del params[name]

    bad_sessions = []
    for name in sessions:
        for ancestor in direct_parents:
            coll = ancestor.member_declaration(name)
            if coll:
                (orig_site, _) = coll
                (session_site, _) = sessions[name]
                report(ENAMECOLL(session_site, orig_site, name))
                bad_sessions.append(name)
    for name in bad_sessions:
        del sessions[name]

    bad_methods = set()
    for (name, (msite, inp, outp, throws, independent, startup, memoized, overridable,
                body, rbrace_site)) in list(methods.items()):
        for ancestor in direct_parents:
            coll = ancestor.member_declaration(name)
            if coll:
                (orig_site, orig_trait) = coll
                if orig_trait.member_kind(name) != 'method':
                    # cannot override non-method with method
                    report(ENAMECOLL(msite, orig_site, name))
                    bad_methods.add(name)

                elif body is None and name in ancestor_vtables:
                    # A method cannot be defined as abstract if it's
                    # already defined in an ancestor. Supporting such
                    # declarations would make no sense, because the
                    # only sensible interpretation would be to ignore
                    # the declaration.
                    report(EAMETH(msite, orig_site, name))
                    bad_methods.add(name)

                elif (name in orig_trait.method_impls
                      and not orig_trait.method_impls[name].overridable):
                    report(EDMETH(msite, orig_trait.method_impls[name].site,
                                  name))
                    bad_methods.add(name)
                elif name not in ancestor_vtables:
                    raise ICE(msite,
                              'ancestor is overridable but not in vtable')

                # Type-checking of overrides is done later, after typedefs
                # have been populated with all template types.
                # See Trait.typecheck_methods()

    for name in bad_methods:
        del methods[name]

    bad_hooks =  []
    for name in hooks:
        for ancestor in direct_parents:
            coll = ancestor.member_declaration(name)
            if coll:
                (orig_site, _) = coll
                (session_site, _, _) = hooks[name]
                report(ENAMECOLL(session_site, orig_site, name))
                bad_hooks.append(name)
    for name in bad_hooks:
        del hooks[name]

    reserved_symbols = {sym: template_symbols[sym]
                         for sym in template_symbols
                         if (sym not in methods
                             and sym not in params
                             and sym not in sessions
                             and sym not in hooks
                             and sym not in ancestor_vtables
                             and sym not in ancestor_method_impls)}
    # referencing 'dev.xyz' from a shared method is always OK, even if
    # it's technically an untyped object parameter
    reserved_symbols.pop('dev', None)
    return Trait(site, tname, ancestors, methods, params, sessions, hooks,
                 ancestor_vtables, ancestor_method_impls, reserved_symbols)

def typecheck_method_override(left, right):
    (site0, inp0, outp0, throws0, independent0, startup0, memoized0) = left
    (site1, inp1, outp1, throws1, independent1, startup1, memoized1) = right
    if len(inp0) != len(inp1):
        raise EMETH(site0, site1, "different number of input arguments")
    if len(outp0) != len(outp1):
        raise EMETH(site0, site1, "different number of output arguments")
    if throws0 != throws1:
        raise EMETH(site0, site1, "different nothrow annotations")
    for ((n, t0), (_, t1)) in zip(inp0, inp1):
        t0 = safe_realtype_unconst(t0)
        t1 = safe_realtype_unconst(t1)
        ok = (t0.eq_fuzzy(t1)
              if compat.lenient_typechecking in dml.globals.enabled_compat
              else t0.eq(t1))
        if not ok:
            raise EMETH(site0, site1,
                        "mismatching types in input argument %s" % (n,))
    for (i, ((_, t0), (_, t1))) in enumerate(zip(outp0, outp1)):
        t0 = safe_realtype_unconst(t0)
        t1 = safe_realtype_unconst(t1)
        ok = (t0.eq_fuzzy(t1)
              if compat.lenient_typechecking in dml.globals.enabled_compat
              else t0.eq(t1))
        if not ok:
            raise EMETH(site0, site1,
                        "mismatching types in output argument %d" % (i + 1,))

    def qualifier_check(qualifier_name, qualifier0, qualifier1):
        if qualifier0 > qualifier1:
            raise EMETH(site0, site1,
                        (f"overriding method is declared {qualifier_name}, "
                         + "but the overridden method is not"))
        elif qualifier0 < qualifier1:
            raise EMETH(site0, site1,
                        (f"overridden method is declared {qualifier_name}, "
                         + "but the overriding method is not"))

    qualifier_check('independent', independent0, independent1)
    qualifier_check('startup', startup0, startup1)
    qualifier_check('memoized', memoized0, memoized1)

def merge_method_impl_maps(site, parents):
    '''Return a dictionary mapping method name to the most specific traits
    in which the method is defined. Multiple traits represent hierarchally
    unrelated traits each providing an implementation, indicating ambiguity
    as to what implementation should be used.'''

    # DML does not have a method resolution order. Instead we require
    # that the resolution order of overridden members are uniquely
    # determined by the partial order defined by trait inheritance. In
    # other words, if a member is inherited from two different traits,
    # then one of the traits must be a subtrait of the other. If that is not
    # true, then the conflict must be resolved though any usage of both traits
    # together must also (eventually) provide an overriding definition of both
    # implementations.

    # TODO: we should merge this method resolution mechanism into
    # structure.sort_method_implementations
    merged_impls = {}
    for parent in parents:
        for (mname, unmerged_impls) in parent.method_impl_traits.items():
            if mname not in merged_impls:
                merged_impls[mname] = unmerged_impls
            else:
                # The method is multiply inherited. For each unmerged
                # implementation of the parent, if it gets overridden by any
                # previous implementation, skip it; otherwise,
                # implementations it overrides are removed, and mark it as
                # a new implementation.
                # If marked as a new implementation, and there are previous
                # implementations it doesn't override, then report an error if
                # the new or old implementations are not overridable.
                # This naive algorithm will result in roughly O(n³ * log(n))
                # behaviour (when assuming all variable lengths are equal)
                existing_impls = merged_impls[mname]
                new_impls = []
                for unmerged_impl in unmerged_impls:
                    filtered_existing_impls = []
                    for existing_impl in existing_impls:
                        if existing_impl.implements(unmerged_impl):
                            break
                        elif not unmerged_impl.implements(existing_impl):
                            filtered_existing_impls.append(existing_impl)
                    else:
                        # Novel implementation. Error if there are existing
                        # unrelated implementations, and the new or the
                        # previous implementations are unoverridable.
                        # Need only check if existing is unoverridable if there
                        # were only exactly one of them (otherwise they are all
                        # already guaranteed to be overridable)
                        if (filtered_existing_impls
                            and not (
                                unmerged_impl.method_impls[mname].overridable
                                and (len(existing_impls) != 1
                                     or existing_impls[0].method_impls[
                                         mname].overridable))):
                            report(EAMBINH(
                                site, None, mname,
                                unmerged_impl.name,
                                existing_impls[0].name))
                        else:
                            new_impls.append(unmerged_impl)
                            existing_impls = filtered_existing_impls
                existing_impls.extend(new_impls)
                merged_impls[mname] = existing_impls
    return merged_impls

class MethodHandle(object):
    def __init__(self, site, name, obj_spec, overridable):
        self.site = site
        self.name = name
        self.obj_spec = obj_spec
        self.overridable = overridable
        self.rank = obj_spec.rank

def get_highest_ranks(ranks):
    '''Given a set of ranks, return the subset of highest unrelated ranks'''
    if len(ranks) <= 1:
        return ranks.union()
    # .intersection(ranks) is redundant, but may serve to avoid creating large
    # intermediate sets
    return ranks.difference(*(r.inferior.intersection(ranks) for r in ranks))

@functools.cache
def calc_minimal_ancestry(ranks: frozenset["Rank"]):
    '''Given a set of ranks, return a dictionary Rank -> set(Rank)
    mapping each rank to the set of highest unrelated ranks it subsumes.
    In addition, minimal_ancestry[None] is the set of highest unrelated ranks
    among all ranks given.
    '''
    # The subset of the ancestry graph where implementations of this
    # method can be found
    ancestry = {rank: rank.inferior.intersection(ranks)
                for rank in ranks}

    # None represents a fictional declaration that overrides all real
    # implementations
    ancestry[None] = ranks

    # Transitive reduction, naive O(n^3) algorithm.
    minimal_ancestry = {
        rank: parents.difference(*(
            ancestry[p] for p in parents))
        for (rank, parents) in ancestry.items()}

    return minimal_ancestry

def sort_method_implementations(implementations):
    '''Given a list of (Rank, ast.method) pairs, return a pair
    (default_map, method_order), where default_map is a dict mapping
    ast.method to list of ast.method it overrides, and method_order is
    a topological ordering of methods based on this graph.'''

    rank_to_method = {}
    for impl in implementations:
        if impl.rank in rank_to_method:
            # two conflicting method definitions in the same block
            raise ENAMECOLL(impl.site, rank_to_method[impl.rank].site,
                            impl.name)
        rank_to_method[impl.rank] = impl

    minimal_ancestry = calc_minimal_ancestry(frozenset(rank_to_method))

    if len(minimal_ancestry[None]) > 1:
        # There is no single method implementation that overrides all
        # other implementations
        def is_default(r):
            return rank_to_method[r].overridable

        [r1, r2] = sorted(minimal_ancestry[None], key=is_default)[:2]
        raise EAMBINH(rank_to_method[r1].site,
                      rank_to_method[r2].site,
                      rank_to_method[r1].name,
                      r1.desc, r2.desc,
                      is_default(r1))

    # Ancestry graph translated back to method ASTs. Maps method to
    # list of default methods.
    method_map = {
        m: [rank_to_method[x] for x in minimal_ancestry[r]]
        for (r, m) in rank_to_method.items()}
    method_order = list(reversed(topsort.topsort(method_map)))

    m = method_order[0]
    if (dml.globals.dml_version == (1, 2)
        and os.path.basename(m.site.filename()) != 'dml12-compatibility.dml'):
        if len(implementations) > 2:
            report(WEXPERIMENTAL(
                m.site, "more than one level of method overrides"))
        if len(implementations) == 2 and m.overridable:
            report(WEXPERIMENTAL(
                m.site, "method with two default declarations"))

    return (method_map, method_order)

class SubTrait:
    '''Logic shared between nodes and traits, which both can inherit
    other traits.'''
    def __init__(self, ancestors, ancestor_vtables):
        # cheap, grossly non-exhaustive, check that 'ancestors' contains all
        # ancestors recursively.
        if ancestors:
            one_ancestor = next(iter(ancestors))
            if one_ancestor.ancestors:
                assert next(iter(one_ancestor.ancestors)) in ancestors

        # Minimal set of parents from which all other ancestors are
        # reachable. The iteration order is significant for code
        # generation, so we sort the parents (by name, quite
        # arbitrarily) to generate code in a predictable manner.
        self.direct_parents = sorted(
            ancestors.difference(*(p.ancestors for p in ancestors)))

        # Map ancestor trait to ordered lists of paths that self inherits
        # that trait E.g., {A: [(B, A), (C, D, A)]} means that this
        # trait inherits A in two ways: By inheriting B which in turn
        # inherits A, and by inheriting C which inherits D which inherits
        # A. This means that we end up with two copies of A's vtable,
        # which requires adjustor thunks to be created if any method
        # overrides a method from A's vtable.  All ancestry of a trait
        # are semantically equivalent, but the first path in the list
        # is considered canonical: When producing a run-time trait
        # reference, the compiler prefers to use the first path, and
        # adjustor thunks (which lead to an extra indirection) are
        # avoided on the canonical path while generating vtables, if possible.
        self.ancestry_paths = {
            p: ([(p,)] if p in self.direct_parents
                else [(dp,) + path
                      for dp in self.direct_parents
                      if p in dp.ancestry_paths
                      for path in dp.ancestry_paths[p]])
            for p in ancestors}

        # Map method or parameter name to the ancestor trait in whose
        # vtable the method or parameter appears
        self.ancestor_vtables = ancestor_vtables

    @property
    def ancestors(self): return self.ancestry_paths.keys()

class ObjTraits(SubTrait):
    '''Keep track of the traits implemented by a DML object'''

    def __init__(self, node, traits, ancestor_vtables, method_overrides,
                 param_nodes):
        super(ObjTraits, self).__init__(traits, ancestor_vtables)

        self.node = node
        self.referenced = Set()
        # Dictionary, name -> objects.Parameter
        self.param_nodes = param_nodes
        # Dictionary, name -> objects.Method
        self.method_overrides = method_overrides

    def mark_referenced(self, trait):
        '''Make sure code is generated that allows a literal reference to
        the given trait in this node'''
        # Also produce all subtraits of the given trait.
        sub = self.ancestry_paths[trait][0][0]
        if not sub in self.referenced:
            self.referenced.add(sub)
            for m in list(self.method_overrides.values()):
                if sub.member_declaration(m.name) is not None:
                    codegen.mark_method_referenced(codegen.method_instance(m))
            sub.mark_referenced()

    def vtable_cname(self, trait):
        '''The C name of a trait's vtable'''
        return "_tr_%s__%s" % (self.node.attrname() or '_dev', trait.name)

    def lookup_shared_method_impl(self, site, name, indices):
        '''Return implementation of shared method provided by trait'''
        assert isinstance(indices, tuple)
        for trait in self.direct_parents:
            if name in trait.method_impl_traits:
                ref = ObjTraitRef(site, self.node, trait, indices)
                method = trait.lookup(name, ref, site)
                assert method
                return method
        return None

class ReservedSymbol(NonValue):
    @auto_init
    def __init__(self, site, name, kind, template, decl_site): pass
    def __str__(self):
        return self.name
    def exc(self):
        fmt = {'param': 'untyped parameter %s',
               'method': 'non-shared method %s',
               # session and saved variables are normally part of the
               # template type, but not if declared inside #if
               'session': 'conditional session variable %s',
               'saved': 'conditional saved variable %s',
               # hooks are only part of the template type if declared
               # shared
               'hook': 'hook %s',
               # template type, but not if declared inside #if
               'subobj': 'subobject %s',
               }[self.kind] % (self.name,)
        return ENSHARED(self.site, fmt, self.template, self.decl_site)

class Trait(SubTrait):
    '''A trait, as defined by a top-level 'trait' statement'''

    # set of traits that are actually used. Each trait appears before
    # all its subtraits.
    referenced = Set()

    def __init__(self, site, name, ancestors, methods, params, sessions, hooks,
                 ancestor_vtables, ancestor_method_impls, reserved_symbols):
        method_impls = {
            name: TraitMethod(
                msite, inp, outp, throws, independent, startup, memoized,
                overridable, body, self, name,
                ancestor_method_impls.get(name, []), rbrace_site)
            for (name, (msite, inp, outp, throws, independent, startup,
                        memoized, overridable, body, rbrace_site))
            in list(methods.items())
            if body is not None}

        super(Trait, self).__init__(ancestors, ancestor_vtables)
        self.name = name
        self.site = site

        # Method implementations provided by this trait. Dictionary,
        # name -> TraitMethod
        self.method_impls = method_impls

        method_impl_traits = dict(ancestor_method_impls)
        for name in method_impls:
            method_impl_traits[name] = [self]
        # Maps method name to the list of hierarchically unrelated Trait
        # objects that provide an implementation of the method
        self.method_impl_traits = method_impl_traits

        # methods and parameters that are direct members of this trait's vtable
        self.vtable_methods = {
            name: (msite, inp, outp, throws, independent, startup, memoized)
            for (name, (msite, inp, outp, throws, independent, startup, memoized,
                        overridable, _, _))
            in list(methods.items())
            if overridable and name not in ancestor_vtables}
        self.vtable_params = params
        self.vtable_sessions = sessions
        self.vtable_hooks = {name: (hooks[name], THook(hooks[name][2]))
                             for name in hooks}
        self.vtable_memoized_outs = {
            '_memo_outs_' + name: method.memo_outs_struct
            for (name, method) in method_impls.items()
            if method.independent and method.memoized}
        self.reserved_symbols = reserved_symbols

    def __repr__(self):
        return 'Trait(%r, %r)' % (
            self.name, [parent.name for parent in self.direct_parents])

    def __str__(self):
        return self.name

    def __lt__(self, other):
        if not isinstance(other, Trait):
            return NotImplemented
        return self.name < other.name

    def type(self):
        return TTrait(self)

    def typecheck_methods(self):
        for (_, inp, outp, _, _, _, _) in self.vtable_methods.values():
            for (_, t) in inp + outp:
                try:
                    check_named_types(t)
                except DMLError as e:
                    report(e)

        for sm in self.method_impls.values():
            # To avoid duplicating error messages
            bad = False
            if sm.name not in self.vtable_methods:
                for (_, t) in sm.inp + sm.outp:
                    try:
                        check_named_types(t)
                    except DMLError as e:
                        bad = True
                        report(e)

            if not bad and sm.name in self.ancestor_vtables:
                try:
                    typecheck_method_override(
                        (sm.site, sm.inp, sm.outp, sm.throws, sm.independent,
                         sm.startup, sm.memoized),
                        self.ancestor_vtables[sm.name].vtable_methods[sm.name])
                except DMLError as e:
                    report(e)

        # We don't attempt to purge the bad methods. Subtemplates could
        # be referencing them, and there is no good way to purge those
        # references. This doesn't seem to be too big of a deal; DMLC stops
        # early enough that the bad overrides don't cause ICE:s to happen
        # (though their presence may lead to other strange behaviour.)

    def scope(self, global_scope):
        '''Return a scope for looking up sibling objects in this trait'''
        s = Symtab(global_scope)
        selfref = mkLit(self.site, '_' + cident(self.name), self.type())
        for name in self.members():
            # This is very hacky, but works well
            try:
                expr = mkSubRef(self.site, selfref, name, '.')
            except EINDEPENDENTVIOL:
                expr = InvalidSymbol(self.site, name, EINDEPENDENTVIOL)
            s.add(ExpressionSymbol(name, expr, self.site))
        # grammar prohibits name collision on 'this'
        s.add(ExpressionSymbol('this', selfref, self.site))
        return s

    def empty(self):
        return not (self.direct_parents or self.vtable_params
                    or self.vtable_sessions or self.vtable_hooks
                    or self.vtable_methods or self.vtable_memoized_outs)

    def members(self):
        '''Return a generator yielding the names of all members of this
        trait'''
        for name in self.vtable_params:
            yield name
        for name in self.vtable_sessions:
            yield name
        for name in self.vtable_hooks:
            yield name
        for name in self.vtable_methods:
            assert name not in self.ancestor_vtables
            yield name
        for name in (name for name in self.ancestor_vtables
                     if self.member_kind(name) != 'memoized_outs'):
            yield name
        for name in self.method_impl_traits:
            # avoid yielding entry twice
            if (name not in self.vtable_methods
                and name not in self.ancestor_vtables):
                yield name
        for name in self.reserved_symbols:
            yield name

    def member_declaration(self, name):
        '''In 'name' is defined by this trait, then return a pair (site, t),
        where t is the trait (this or a supertrait) where the name is
        defined.  If 'name' is not defined in this trait, return
        None.
        '''

        if name in self.method_impl_traits:
            return (self.method_impl_traits[name][0].method_impls[name].site,
                    self)
        elif name in self.vtable_methods:
            (site, _, _, _, _, _, _) = self.vtable_methods[name]
            return (site, self)
        elif name in self.vtable_params:
            (site, _) = self.vtable_params[name]
            return (site, self)
        elif name in self.vtable_sessions:
            (site, _) = self.vtable_sessions[name]
            return (site, self)
        elif name in self.vtable_hooks:
            ((site, _, _), _) = self.vtable_hooks[name]
            return (site, self)
        elif name in self.ancestor_vtables:
            return self.ancestor_vtables[name].member_declaration(name)
        else:
            return None

    def lookup(self, name, expr, site):
        '''Look up a member of this trait; return a referencing expression or
        None. expr is an expression referencing this trait.'''
        if name == 'templates':
            return mkTraitTemplatesRef(site, self, expr)
        if name in self.method_impl_traits:
            impl_traits = self.method_impl_traits[name]
            if not all(impl_trait.method_impls[name].overridable
                       for impl_trait in impl_traits):
                # For a non-overridable method, it is safe to bypass the
                # vtable lookup indirection, and use a direct reference to
                # a specific method implementation. This way, the code
                # gets slightly smaller and faster.
                if len(impl_traits) != 1:
                    raise ICE(site, ("Conflict between multiple shared "
                                     + "method implementations"))
                [impl_trait] = impl_traits
                impl = impl_trait.method_impls[name]
                if self is not impl_trait:
                    expr = TraitUpcast(site, expr, impl_trait)
                if impl_trait is not impl.vtable_trait:
                    expr = TraitUpcast(site, expr, impl.vtable_trait)
                return TraitMethodDirect(site, expr, impl)
        if name in self.vtable_methods:
            (_, inp, outp, throws, independent, _, _) = \
                self.vtable_methods[name]
            return TraitMethodIndirect(site, expr, name, inp, outp, throws,
                                       independent)
        if name in self.vtable_params:
            (_, ptype) = self.vtable_params[name]
            return TraitParameter(site, expr, name, ptype)
        if name in self.vtable_sessions:
            (_, ptype) = self.vtable_sessions[name]
            return mkDereference(site, TraitSessionRef(site, expr, name, ptype))
        if name in self.vtable_hooks:
            ((_, dimsizes, _), hooktyp) = self.vtable_hooks[name]
            if dimsizes:
                return TraitHookArrayRef(site, dimsizes, hooktyp, expr, name,
                                         ())
            else:
                return TraitHookRef(site, (), hooktyp, expr, name, ())
        vtable_trait = self.ancestor_vtables.get(name, None)
        if vtable_trait:
            return vtable_trait.lookup(
                name, TraitUpcast(site, expr, vtable_trait), site)
        if name in self.reserved_symbols:
            (kind, decl_site) = self.reserved_symbols[name]
            return ReservedSymbol(site, name, kind, self.name, decl_site)
        return None

    def implicit_args(self):
        return [("_" + cident(self.name), self.type())]

    def vtable_method_type(self, inp, outp, throws, independent):
        return TPtr(TFunction(
            [t for (n, t) in c_inargs(
                crep.maybe_dev_arg(independent) + self.implicit_args() + inp,
                outp, throws)],
            c_rettype(outp, throws)))

    def mark_referenced(self):
        if not self in Trait.referenced:
            for p in self.direct_parents:
                p.mark_referenced()
            Trait.referenced.add(self)

    def implements(self, trait):
        return trait is self or trait in self.ancestors

    def member_kind(self, name):
        '''For a given member of this trait, return 'method' if it's a
        method, and 'parameter' if it's a parameter'''
        if name in self.vtable_methods or name in self.method_impl_traits:
            return 'method'
        elif name in self.vtable_params:
            return 'parameter'
        elif name in self.vtable_sessions:
            return 'session'
        elif name in self.vtable_hooks:
            return 'hook'
        elif name in self.vtable_memoized_outs:
            return 'memoized_outs'
        elif name in self.ancestor_vtables:
            return self.ancestor_vtables[name].member_kind(name)
        else:
            raise ICE(self.site, f'unknown member: {name}')

    def vtable_trait(self, name):
        '''Return the trait that has the vtable entry for the named trait
        member'''
        if (name in self.vtable_methods or name in self.vtable_params
            or name in self.vtable_sessions or name in self.vtable_hooks
            or name in self.vtable_memoized_outs):
            return self
        return self.ancestor_vtables[name]

def required_implicit_traits(traits):
    '''Given a set of traits implemented by an object, describe a set of
    additional implicit traits that has to be created to make sure
    that adjustor thunks can be created correctly for the involved
    traits. The return value is a list of frozensets, each one showing
    the set of ancestors of one implicit trait.

    An implicit trait is needed when an object has two traits, say A
    and B, which both are subtraits of a third trait C, which has an
    abstract method M, and furthermore, if M is kept abstract in A,
    but overridden by B. Now, the C vtable in A must contain an
    adjustor thunk to B, which requires that A and B have a common
    subtrait implemented by the object. This is the implicit trait.

    This is exercised by t126/1.2/structure/T_trait_diamond, port
    q. It is also required by the standard library.

    In order to simplify this function, we don't analyze methods
    overrides; instead we defensively require an implicit trait
    whenever the trait inheritance graph indicates there might be a
    need for it, i.e., if a trait is multiple inherited and there is
    no single trait that inherits all its subclasses.

    Technically, this is done by splitting the set of traits into
    disjoint partitions, grouped by the equivalence relation yielded
    by the transitive closure of the subclass relation. In other
    words, two traits end up in the same group if they share a common
    subclass, and also (recursively) if they share groups with a
    common third base class.

    Each of these partitions yields one implicit trait, except that
    implicit trait is created for a partition where there already is a
    trait in the partition which is a subtrait of all other traits in
    the partition.
    '''
    # Disjoint-set forest: Maps each base trait (i.e., each trait with
    # no explicit parents) to a list of base traits which it is
    # related to. The same list instance is shared by all
    # instances. An algorithm geek may remark that this is not an
    # asymptotically optimal algorithm for disjoint-set operations.
    # However, it's fast enough, and easy to comprehend.
    partitions = {t: [t] for t in traits if not t.ancestors}
    for t in traits:
        bases = [base for base in t.ancestors if not base.ancestors]
        if not bases:
            continue
        partition = partitions[bases[0]]
        # Merge the partitions of all base traits of t
        for b in bases[1:]:
            other_partition = partitions[b]
            if other_partition[0] != partition[0]:
                # disjoint, merge!
                partition.extend(other_partition)
                for other_trait in other_partition:
                    partitions[other_trait] = partition

    # 'partitions' now contains a partition of all base traits. Assign
    # each non-base traits to a partition.
    for t in traits:
        if t not in partitions:
            for base in t.ancestors:
                if base in partitions:
                    if len(partitions[base][-1].ancestors) > len(t.ancestors):
                        # Preserve the property that the last element of
                        # each partition is the one with the largest
                        # number of ancestors. This way, we can guarantee
                        # that if one trait is a subtrait of all others,
                        # then that trait is the last one (and can be
                        # easily found).
                        partitions[base][-1:-1] = [t]
                    else:
                        partitions[base].append(t)
                    break
            else:
                raise ICE(t.site, 'no base trait for %s' % (t.name))

    # Eliminate duplicates, by picking one representative from each partition
    representatives = Set(partitions[t][0] for t in partitions)
    # Represent each implicit trait as the set of traits it must inherit
    return [frozenset(partitions[t]) for t in representatives
            # An implicit trait is not needed if one trait is subtrait
            # of all other traits in the same partition.
            # .ancestors does not include the trait partitions[t][-1],
            # while partitions[t] does.
            if len(partitions[t][-1].ancestors) + 1 != len(partitions[t])]
