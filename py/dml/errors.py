# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

from .logging import DMLError, ICE, truncate, binary_dump

class AFTER(DMLError):
    """
    An illegal `after` statement was specified. The method callback specified
    may not have any output parameters/return values. If the after is with a
    time delay or bound to a hook, every input parameter must be of serializable
    type (unless that input parameter receives a message component of a hook).
    """
    fmt = "illegal 'after' statement%s with callback method '%s': %s"
    def __init__(self, site, hookexpr, method, unserializable):
        if unserializable:
            msg = (('every method input parameter %smust be of serializable '
                    + 'type')
                   % ('not receiving a message component '
                      * (hookexpr is not None),))
        else:
            msg = ('method must not have any %s'
                   % ('output parameters' if site.dml_version() == (1, 2)
                      else 'return values'))
        on_hook = (f" bound to hook '{hookexpr}'"
                   if hookexpr is not None else '')
        DMLError.__init__(self, site, on_hook, method.name, msg)
        self.method = method
        self.unserializable = unserializable
    def log(self):
        DMLError.log(self)
        self.print_site_message(
            self.method.site,
            "method declaration"
            + ''.join(
                f"\nmethod parameter {p.logref} is of unserializable type: "
                + f"{p.typ}"
                for p in self.unserializable or []))

class AFTERSENDNOW(DMLError):
    """
    An illegal `after` statement was specified where the callback is `send_now`
    of a hook. Every message component type of the hook must be serializable
    (unless that component is provided through a message component parameter
    of the `after` statement, if the `after` statement is attaching the
    callback to another hook.)
    """
    version = "1.4"
    fmt = ("illegal 'after' statement%s with callback '%s.send_now': "
           + "every message component of '%s' %smust be of serializable type%s"
           )
    def __init__(self, site, target_hook, callback_hook, unserializable):
        # Two hooks involved makes this messy, clarify as well as possible
        clarification = ("not provided through a message component parameter "
                         "of the 'after' " * (target_hook is not None))
        unserializable_msg = (''.join(
                f"\nmessage component {idx + 1} is of unserializable type: "
                + f"{ptype}"
                for (idx, ptype) in unserializable))

        on_hook = (f"bound to hook '{target_hook}'"
                   if target_hook is not None else '')
        DMLError.__init__(self, site, on_hook, callback_hook, callback_hook,
                          clarification, unserializable_msg)

class AFTERHOOK(DMLError):
    """
    An illegal hook-bound `after` statement was specified.
    The number of message component parameters must be equal to the number of
    message components of the hook.
    """
    version = "1.4"
    fmt = ("illegal 'after' statement bound to hook '%s': "
           + 'hook has %d message components, but %d message component '
           + 'parameters are given')

class AFTERMSGCOMPPARAM(DMLError):
    """Message component parameters bound by a hook-bound after statement can
    only be used as direct arguments to the specified callback method, and
    cannot be used in arbitrary expressions.
    """
    version = "1.4"
    fmt = ("'%s' is a message component parameter, and can only be used as a "
           "direct argument to the callback method of the after statement")

class HOOKTYPE(DMLError):
    """There are some minor restrictions to a hook's message component
    types. Anonymous structs and arrays of variable/unknown size are not
    supported.
    """
    version = "1.4"
    fmt = ("'%s' is a not a valid message component type for a hook, as it "
           "is or contains some %s")

class CYCLICIMP(DMLError):
    """
    A DML file imports itself, either directly or indirectly.
    """
    fmt = "cyclic import"
    def __init__(self, sites):
        DMLError.__init__(self, sites[0])
        self.other_sites = sites[1:]
    def log(self):
        DMLError.log(self)
        for site in self.other_sites:
            self.print_site_message(site, "via here")

class CYCLICTEMPLATE(DMLError):
    """
    A template inherits from itself, either directly or indirectly.
    """
    fmt = "cyclic template inheritance"
    def __init__(self, sites):
        DMLError.__init__(self, sites[0])
        self.other_sites = sites[1:]
    def log(self):
        DMLError.log(self)
        for site in self.other_sites:
            self.print_site_message(site, "via here")

class AMBINH(DMLError):
    """<a id="EAMBINH"/>
    If a method or parameter has multiple definitions, then there must
    be a unique definition that overrides all other definitions.
    See [Resolution of overrides](language.html#resolution-of-overrides).
    """
    # 'Resolution of overrides' does not exist in the 1.2 reference manual
    version = "1.4"
    fmt = "conflicting definitions of %s when instantiating %s and %s"
    def __init__(self, site, other_site, method, rank_desc1, rank_desc2,
                 overridable=True):
        DMLError.__init__(self, site, method, rank_desc1, rank_desc2)
        extra_lines = []
        if other_site:
            extra_lines.append((other_site, "conflicting definition"))
            def how_to_instantiate(rank_desc):
                # we ignore 'in each' blocks here. In order to get a
                # rank superior to an 'in each' block, one needs to
                # instantiate the parent template or file.
                if rank_desc.kind == 'file':
                    return "add 'import \"%s\"' in this file" % (
                        rank_desc.text,)
                elif rank_desc.kind == 'template':
                    return "add 'is %s' near this line" % (rank_desc.text,)
                else:
                    assert rank_desc.kind == 'verbatim'
                    # should not happen
                    return "instantiate %s" % rank_desc.text
            if overridable:
                # conflicting default methods
                extra_lines.append((
                    site, "to resolve, either %s..."
                    % (how_to_instantiate(rank_desc2),)))
                extra_lines.append((
                    other_site, "... or %s" % (how_to_instantiate(
                        rank_desc1),)))
            else:
                # one non-default method
                extra_lines.append((
                    site, "probable resolution: " + how_to_instantiate(
                        rank_desc2)))
        self.extra_lines = extra_lines
    def log(self):
        DMLError.log(self)
        for (site, msg) in self.extra_lines:
            self.print_site_message(site, msg)

class AMBDEFAULT(DMLError):
    """A method may not invoke its default implementation if multiple
    methods are overridden, and the template inheritance graph is
    insufficient to infer that one default implementation overrides
    the others. See section [x](language.html#calling-methods) for details.
    """
    fmt = "Ambiguous invocation of default implementation"
    def __init__(self, site, default_sites):
        DMLError.__init__(self, site)
        self.default_sites = default_sites
    def log(self):
        DMLError.log(self)
        for site in self.default_sites:
            self.print_site_message(site, "default method candidate")

class AMETH(DMLError):
    """
    A shared abstract method cannot override another method.
    """
    fmt = "shared abstract method %s overrides existing method"

    def __init__(self, site, prev_site, name):
        DMLError.__init__(self, site, name)
        self.prev_site = prev_site

    def log(self):
        DMLError.log(self)
        self.print_site_message(
            self.prev_site, "previous declaration")

class TMETH(DMLError):
    """
    A shared method cannot override a non-shared method
    """
    version = "1.4"
    fmt = "attempt to override non-shared method %s with shared method"
    def __init__(self, site, trait_method_site, name):
        DMLError.__init__(self, site, name)
        self.trait_method_site = trait_method_site
    def log(self):
        DMLError.log(self)
        self.print_site_message(
            self.trait_method_site, "shared method definition")

class TEMPLATEUPCAST(DMLError):
    """When casting to a template type, the source expression must be either
    an object implementing the template, or an expression whose type is a
    subtemplate of the target type."""
    version = "1.4"
    fmt = "invalid upcast, %s not a subtemplate of %s"

class ABSTEMPLATE(DMLError):
    """
    If a template has any abstract methods or parameters, they must all be
    implemented when instantiating the template.
    """
    version = "1.4"
    fmt = "Instantiating template %s requires abstract %s %s to be implemented"
    def __init__(self, is_site, decl_site, decl_trait, kind, name):
        DMLError.__init__(self, is_site, decl_trait, kind, name)
        self.decl_site = decl_site
    def log(self):
        DMLError.log(self)
        self.print_site_message(
            self.decl_site, "abstract declaration")

class ABSMETH(DMLError):
    """
    An (abstractly) declared method never has any definition made for it.
    """
    version = "1.4"
    fmt = "declared method %s is never implemented"

class IMPORT(DMLError):
    """
    The file to imported could not be found. Use the `-I`
    option to specify additional directories to search for imported
    files.
    """
    fmt = "cannot find file to import: %s"
    def __init__(self, site, filename):
        DMLError.__init__(self, site, filename)

class SIMAPI(DMLError):
    """
    The DML file is written in a too old version of DML. Use the
    `--simics-api` option to use a sufficiently old Simics API.
    """
    fmt = "DML version %s does not support API version %s"
    def __init__(self, site, dml_ver, api_ver):
        DMLError.__init__(self, site, dml_ver, api_ver)

class TYPE(DMLError):
    """
    The data type is not defined in the DML code.
    """
    fmt = "unknown type: '%s'"

class VARTYPE(DMLError):
    """A variable has been declared with a given type but the type is
    not acceptable.
    """
    fmt = "variable or field declared %s"

class TREC(DMLError):
    """
    The definition of a structure type can not have itself as direct or
    indirect member.
    """
    fmt = "recursive type definition of %s"
    def __init__(self, sites, type):
        DMLError.__init__(self, sites[0], type)
        self.other_sites = sites[1:]
    def log(self):
        DMLError.log(self)
        for site in self.other_sites:
            self.print_site_message(site, "via here")

class ANONSTRUCT(DMLError):
    """
    Declarations of new structs are not permitted in certain contexts,
    such as method arguments, `new` expressions,
    `sizeoftype` expressions and `cast` expressions.
    """
    fmt = "struct declaration not allowed in a %s"

class EMPTYSTRUCT(DMLError):
    """
    A struct or layout type must have at least one field.
    This restriction does not apply to structs declared in a
    `extern typedef`.
    """
    fmt = "struct or layout with no fields"

class CAST(DMLError):
    """
    The cast operation was not allowed.  It is illegal to cast to void.
    """
    fmt = "illegal cast to '%s'"
    def __init__(self, site, expr, type):
        DMLError.__init__(self, site, type)

class VOID(DMLError):
    """The type `void` is not a value, and thus cannot be used as
    the type of e.g. a variable or struct member"""
    fmt = "illegal use of void type"

class NBOOL(DMLError):
    """
    Conditions must be properly boolean expressions; e.g., "`if (i ==
    0)`" is allowed, but "`if (i)`" is not, if `i` is an
    integer.
    """
    fmt = "non-boolean condition: '%s' of type '%s'"
    def __init__(self, expr):
        DMLError.__init__(self, expr, expr, expr.ctype())

class ASSIGN(DMLError):
    """
    The target of the assignment is not an l-value, and thus cannot be
    assigned to.
    """
    fmt = "cannot assign to this expression: '%s'"
    def __init__(self, site, target):
        DMLError.__init__(self, site, target)

class ASTYPE(DMLError):
    """
    The target of an initializer is incompatible with the type of the
    initializer.
    """
    fmt = ("wrong type for initializer\n"
           "got:      %s\n"
           "expected: %s")
    def __init__(self, site, target_type, source):
        self.source = source
        self.target_type = target_type
        DMLError.__init__(self,
                          site, source.ctype().describe(),
                          target_type.describe_assign_types())

class INCTYPE(DMLError):
    """
    The prefix and postfix increment/decrement operators can only be
    used on integer and pointer expressions.
    """
    fmt = ("wrong type for '%s' operator")

class BTYPE(DMLError):
    """
    An expression had the wrong type.
    """
    fmt = ("wrong type\n"
           "got:      %s\n"
           "expected: %s")

class CSADD(DMLError):
    """
    Non-constant strings cannot be concatenated using `+`.
    """
    fmt = ("non-constant strings cannot be concatenated using '+'")

class EARG(DMLError):
    """
    Function and method arguments in declarations cannot be of
    endian integer type.
    """
    fmt = ("cannot use endian integer as argument type in declaration")

class ASSINL(DMLError):
    """
    The target of the assignment is a method parameter that has been
    given a constant or undefined value when inlining the method.
    """
    fmt = "cannot assign to inlined parameter: '%s'"
    def __init__(self, site, name):
        DMLError.__init__(self, site, name)

class ERRSTMT(DMLError):
    """
    The source code contained a statement "`error;`", which
    forces a compilation error with the given message, or the standard message
    "forced compilation error in source code".
    """
    fmt = "%s"
    def __init__(self, site, msg):
        DMLError.__init__(self, site, msg)

class EXTERN(DMLError):
    """An extern declared method must be fully typed and may not throw
    exceptions."""
    fmt = "illegal declaration of extern method"
    version = "1.2"

class EXPORT(DMLError):
    """Can only export non-inline, non-shared, non-throwing methods declared
    outside object arrays."""
    fmt = "cannot export this method"
    version = "1.4"

    def __init__(self, method_site, export_site):
        DMLError.__init__(self, method_site)
        self.export_site = export_site

    def log(self):
        DMLError.log(self)
        self.print_site_message(self.export_site, "exported here")

class STATICEXPORT(DMLError):
    """A method reference can only be converted to a function pointer if the
    method is non-inline, non-shared, non-throwing, and declared outside an
    object array."""
    fmt = "cannot convert this method reference to a function pointer"
    version = "1.4"

    def __init__(self, method_site, addressof_site):
        DMLError.__init__(self, method_site)
        self.addressof_site = addressof_site

    def log(self):
        DMLError.log(self)
        self.print_site_message(self.addressof_site,
                                "attempted conversion here")

class INVALID(DMLError):
    """
    The expression does not produce a proper value.
    """
    fmt = "invalid expression: '%s'"
    def __init__(self, expr):
        DMLError.__init__(self, expr, expr)

class UNDEF(DMLError):
    """
    Caused by an attempt to generate code for an expression that
    contains the `undefined` value.
    """
    fmt = "undefined value: '%s'"
    def __init__(self, site, expr = None):
        if expr is None:
            expr = site
        DMLError.__init__(self, site, expr)

class SHNEG(DMLError):
    """
    The right-hand side operand to a shift operator must not be negative.
    """
    fmt = "shift with negative shift count: '%s"

class DIVZ(DMLError):
    """
    The right-hand side of the given / or % operator is always zero.
    """
    fmt = "right-hand side operand of '%s' is zero"

# TODO: also check bitwise or/xor for type errors.

class BINOP(DMLError):
    """
    One or both of the operands have the wrong type for the given binary
    operator.
    """
    fmt = "illegal operands to binary '%s' \n%s"
    def __init__(self, site, op, lh, rh):
        DMLError.__init__(self, site, op, binary_dump(lh, rh))

class BSLICE(DMLError):
    """
    A bitslice operation was attempted on an expression that is not an
    integer.
    """
    fmt = "illegal bitslice operation"

class BSSIZE(DMLError):
    """
    Bit slices cannot be larger than 64 bits.
    """
    fmt = "bitslice size of %s bits is not between 1 and 64"

class BSBE(DMLError):
    """A big-endian bit slice can only be done on an expression whose type
    is explicitly defined, such as a local variable or a register field."""
    fmt = "bitslice with big-endian bit order and uncertain bit width"
    def __init__(self, site):
        DMLError.__init__(self, site)

class ZRANGE(DMLError):
    """
    An array index range must start at zero.
    """
    fmt = "array range must start at 0"
    def __init__(self, site):
        DMLError.__init__(self, site)

class NARRAY(DMLError):
    """
    Indexing can only be applied to arrays, integers (bit-slicing),
    and lists.
    """
    fmt = "trying to index something that isn't an array: '%s'"
    def __init__(self, expr):
        DMLError.__init__(self, expr, expr)

class OOB(DMLError):
    """
    The used index is outside the defined range.
    """
    fmt = "array index out of bounds"
    def __init__(self, expr):
        DMLError.__init__(self, expr)

class AVAR(DMLError):
    """
    Indexing into constant lists can only be done with constant indexes.
    """
    fmt = "cannot use variable index in a constant list"

class NLST(DMLError):
    """
    A list was expected.
    """
    fmt = "not a list: %s"

class NVAL(DMLError):
    """
    Only some objects can be used as values directly. An attribute can
    only be accessed directly as a value if it has been declared using the
    `allocate_type` parameter.
    """
    fmt = "not a value: %s"

class NORET(DMLError):
    """
    If a method has output arguments, then control flow may not reach
    the end of the method. Either an explicit value must be returned
    in a return statement, or the execution must be aborted by an
    exception or assertion failure. Note that DMLC's control flow
    analysis is rather rudimentary, and can issue this error on code
    that provably will return. In this case, the error message can be
    silenced by adding `assert false;` to the end of the
    method body.
    """
    version = "1.4"
    fmt = "missing return statement in method with output argument"

class ATYPE(DMLError):
    """
    Either the `attr_type` or the `type` parameter of the
    attribute must be specified.
    """
    fmt = "attribute type undefined: %s"
    def __init__(self, attr):
        DMLError.__init__(self, attr, attr.identity())

class ANAME(DMLError):
    """
    This name is not available as the name of an attribute, since it is
    used for an automatically added attribute.
    """
    fmt = "illegal attribute name: %s"

class ACHK(DMLError):
    """
    An attribute must have set and get methods to be
    checkpointable. This attribute has neither, and the
    'configuration' parameter is either "required" or "optional".
    """
    fmt = "checkpointable attribute missing set or get method"

class ANULL(DMLError):
    """
    An attribute must have a set or a get method to be useful.
    """
    fmt = "attribute has no get or set method"

class REGVAL(DMLError):
    """
    When a register has been specified with explicit fields, you have to
    use the `get` and `set` methods to access the register as
    a single value.
    """
    fmt = "cannot use a register with fields as a value: %s"
    def __init__(self, site, reg):
        DMLError.__init__(self, site, reg.identity())

class NOPTR(DMLError):
    """
    A pointer value was expected.
    """
    fmt = "not a pointer: %s (%s)"
    def __init__(self, site, expr):
        DMLError.__init__(self, site, expr, expr.ctype().describe())

class NOSTRUCT(DMLError):
    """
    The left-hand side operand of the `.` operator is not of struct
    type.
    """
    fmt = "trying to get a member of a non-struct: '%s' of type '%s'"
    def __init__(self, site, expr, ctype = None):
        DMLError.__init__(self, site, expr, ctype or expr.ctype())

class BADFAIL(DMLError):
    """
    An exception is thrown in a context where it will not be caught.
    """
    fmt = "uncaught exception"

class BADFAIL_dml12(DMLError):
    """If a DML 1.2 method lacks a `nothrow` annotation, and a
    non-throwing DML 1.4 method calls it, then DMLC will analyze
    whether the method call can actually cause an exception. If it
    can, this error is reported; if not, the call is permitted.

    For this error, a 1.2 method counts as throwing if it throws an
    exception, or calls a `throws` marked 1.4 method, or
    (recursively) if it invokes a method that counts as throwing. A
    call or throw statement inside a `try` block does not cause
    the method to count as throwing. The methods
    `attribute.set`, `bank.read_access` and
    `bank.write_access` count as throwing even if they don't
    throw.

    This error is normally reported while porting common DML 1.2 code
    to DML 1.4: most 1.2 methods are not meant to throw exceptions,
    and when converted to DML 1.4 this becomes a strict requirement
    unless the method is annotated with the `throws` keyword.
    The remedy for this error message is normally to insert a
    `try` block around some call along the throwing call chain,
    with a `catch` block that handles the exception
    gracefully. The `try` block should usually be as close as
    possible to the `throw` in the call chain.

    """
    fmt = "uncaught exception in call to DML 1.2 method '%s'"
    # DML 1.2 methods that explicitly throw.
    # caller -> site of throwing statement
    throwing_methods = {}
    # Calls from DML 1.2 methods to other DML 1.2 methods, which may cause
    # the caller to throw.
    # callee -> [(call-site, caller), ...]
    uncaught_method_calls = {}
    # Calls from DML 1.4 methods into potentially throwing DML 1.2 methods.
    # callee -> [(call-site, caller), ...]
    protected_calls = {}

    def __init__(self, site, call_chain, other_callers):
        (_, bad_method) = call_chain[0]
        DMLError.__init__(self, site, bad_method.name)
        self.site = site
        self.call_chain = call_chain
        self.other_callers = other_callers

    def log(self):
        DMLError.log(self)
        for (site, m) in self.call_chain:
            self.print_site_message(
                site, "exception propagated from %s()" % (m.name,))
        (_, bad_method) = self.call_chain[0]
        for (site, m) in self.other_callers:
            self.print_site_message(
                site, "method '%s' also called here" % (bad_method.name,))

    @classmethod
    def all_errors(cls):
        shortest_call_chains = {
            m: [(site, m)] for (m, site) in cls.throwing_methods.items()}
        queue = list(cls.throwing_methods)
        i = 0
        while i < len(queue):
            m = queue[i]
            for (site, caller) in cls.uncaught_method_calls.get(m, []):
                if caller not in shortest_call_chains:
                    queue.append(caller)
                    shortest_call_chains[caller] = (
                        [(site, caller)] + shortest_call_chains[m])
            i += 1
        for m in cls.protected_calls:
            if m in shortest_call_chains:
                (site, _) = cls.protected_calls[m][0]
                yield cls(site, shortest_call_chains[m],
                    cls.protected_calls[m][1:])

class APPLY(DMLError):
    """
    The applied value is not a function.
    """
    fmt = ("illegal function application of '%s'\n"
           "type: %s")
    def __init__(self, fun, ftype = None):
        if not ftype:
            ftype = fun.ctype()
        DMLError.__init__(self, fun, fun, ftype)

class APPLYMETH(DMLError):
    """
    Calls to inline methods, methods that may throw, or methods that have
    multiple output parameters cannot be used as arbitrary expressions. In DML
    1.2, any such method must be called via the `call` or `inline` statements,
    and in DML 1.4 any such method must be called either as a standalone
    statement, or as an initializer (e.g., RHS of an assignment or argument of
    a `return` statement).
    """
    fmt = "call to method '%s' in unsupported context\n%s"
    def __init__(self, site, fun):
        if site.dml_version() == (1, 2):
            suggestion = ("use the 'call' or 'inline' statements to call "
                          + "this method")
        else:
            suggestion = ("perform this method call either as a standalone "
                          + "statement or as an initializer (e.g., as the RHS "
                          + "of an assignment)")
        DMLError.__init__(self, site, fun, suggestion)

class IDENT(DMLError):
    """
    The identifier has not been declared anywhere.
    """
    fmt = "unknown identifier: '%s'"

    def __init__(self, site, name):
        DMLError.__init__(self, site, name)
        self.identifier = name

class NAMEID(DMLError):
    """
    The name parameter does not follow identifier syntax.
    """
    fmt = "invalid name parameter value: '%s'"

class FORMAT(DMLError):
    """
    The log-statement format string is malformed.
    """
    fmt = "malformed format string: unknown format at position %d"

class DEVICE(DMLError):
    """
    The main source file given to the DML compiler must contain a
    `device` declaration.
    """
    fmt = "missing device declaration"

class LTYPE(DMLError):
    """
    Log-statement type must be one of `info`, `warning`, `error`,
    `spec_viol`, and `unimpl`.
    """
    fmt = "invalid log type: '%s'"

class LLEV(DMLError):
    """
    The log level given in a log statement must be an integer between 1 and 4,
    or 1 and 5 for a subsequent log level (`then ...`), unless the log kind is
    one of "warning", "error", or "critical", in which case it must be 1 (or 5
    for subsequent log level).
    """
    fmt = "log level must be %s"

class SYNTAX(DMLError):
    """
    The code is malformed.
    """
    fmt = "syntax error%s%s"
    def __init__(self, site, tokenstr, reason):
        if tokenstr:
            assert isinstance(tokenstr, str)
            where = " at '%s'" % truncate(tokenstr, 20)
        else:
            where = ""
        if reason:
            reason = ": " + reason
        else:
            reason = ""
        DMLError.__init__(self, site, where, reason)

class PARAM(DMLError):
    """
    The parameter is not bound to a legal value.
    """
    fmt = "illegal value for parameter '%s'"

class UNINITIALIZED(DMLError):
    """
    Some parameters that are automatically supplied by DML
    cannot be accessed in early stages of compilation, such as in object-level
    if statements.
    """
    fmt = "value of parameter %s is not yet initialized"

class BADCONDSTMT(DMLError):
    """
    `#if` statements in object scope are only allowed to contain
    certain kinds of declarations: objects, `method`, `session`,
    `saved`, `#if`, `in each`, `hook`, or `error`. This means in particular
    that `param` and `is` statements are not permitted inside an `#if` block.

    This restriction does *not* apply recursively: object or `in each` blocks
    inside an `#if` are allowed to contain `param` and `is` statements.

    Another special exception is that a `#if` on top scope may contain any
    kind of statement as long as the `#if` condition doesn't reference
    any identifiers other than `dml_1_2`, `true`, and `false`.
    """
    fmt = "'%s' declaration not allowed immediately inside `#if`"

# TODO: Consider re-wording the semantics of this error, allocate_type is only
# relevant in 1.4 when imported from 1.2, and as per SIMICS-9393 this
# error might not even be necessary

class ATTRDATA(DMLError):
    """
    Specifying `allocate_type` and using 'data'
    declarations in the same attribute object is not allowed.
    """
    fmt = ("cannot define both 'allocate_type' parameter "
           "and local data objects")
    def __init__(self, attr, allocate_type_site, data_sites):
        self.allocate_site = allocate_type_site
        self.data_sites = data_sites
        DMLError.__init__(self, attr)

    def log(self):
        DMLError.log(self)
        self.print_site_message(self.allocate_site, "'allocate_type' set here")
        for data_site in self.data_sites:
            self.print_site_message(data_site, 'data object declared here')

class RETTYPE(DMLError):
    """
    The type of the return value (if any) must be specified for methods
    that implement interfaces.
    """
    fmt = "no return type"
    def __init__(self, meth):
        DMLError.__init__(self, meth)

class RETARGNAME(DMLError):
    """
    In DML 1.4, the output arguments of a method are anonymous
    """
    version = "1.4"
    fmt = "method return type declarations may not be named: %s"

class IFREF(DMLError):
    """
    Interface function calls must be simple references to the method.
    """
    fmt = "illegal interface method reference: %s"

class REF(DMLError):
    """
    The referenced object has not been declared.
    """
    fmt = "reference to unknown object '%s'"
    def __init__(self, site, name, obj = None):
        if obj is None:
            place = name
        else:
            place = "%s.%s" % (obj, name)
        DMLError.__init__(self, site, place)

class NOBJ(DMLError):
    """
    A reference to an object was expected.
    """
    fmt = "object expected: %s"

class FMTARGN(DMLError):
    """
    The log-statement has too few or too many arguments for the given
    format string.
    """
    fmt = "wrong number of arguments for format string"

class ASZVAR(DMLError):
    """
    The size of an array must be a constant integer.
    """
    fmt = "array upper bound is not a constant integer: %s"

class ASZR(DMLError):
    """
    An array must have at least one element.
    """
    fmt = "array size is less than 1"
    def __init__(self, site):
        DMLError.__init__(self, site)

class ASZLARGE(DMLError):
    """
    Object arrays with huge dimensions are not allowed; the product of
    dimension sizes must be smaller than 2<sup>31</sup>.
    """
    # It would be cheap to bump the limit to 2**32 elements, but that
    # would require some additional testing to check that we never use
    # signed 32-bit integer arithmetic on packed indices.
    fmt = f"array has too many elements (%d >= {2**31})"

class AINCOMP(DMLError):
    """
    The array has been declared more than once, in an incompatible way.
    """
    fmt = "incompatible array declarations: %s"
    def __init__(self, site, othersite, name, reason):
        DMLError.__init__(self, site, reason)
        self.othersite = othersite
    def log(self):
        DMLError.log(self)
        if self.othersite:
            self.print_site_message(self.othersite, "conflicting declaration")

class AUNKDIMSIZE(DMLError):
    """
    The size of an array dimension of an object array must be defined at least
    once across all declarations of that object array.
    """
    fmt = ("the size of dimension %d%s is never defined")

class NCONST(DMLError):
    """
    A constant expression was expected.
    """
    fmt = "non-constant expression: %s"

class CONT(DMLError):
    """
    A `continue` statement can only be used inside a loop construct.
    """
    fmt = "nothing to continue"

class CONTU(DMLError):
    """
    A `continue` statement cannot be used in a `#foreach`
    or `#select` statement.
    """
    fmt = "continue is not possible here"

class BREAK(DMLError):
    """
    A `break` statement can only be used inside a loop or switch
    construct.
    """
    fmt = "nothing to break from"

class NMETH(DMLError):
    """
    A method name was expected. This might be caused by using
    `call` or `inline` on something that counts as a C
    function rather than a method.
    """
    fmt = "not a method: '%s'"

class NDEFAULT(DMLError):
    """
    The default implementation of a method was invoked, but there was
    no default implementation.
    """
    fmt = "no default implementation"

class ARG(DMLError):
    """
    The number of input/output arguments given in the call differs from
    the method definition.
    """
    fmt = "wrong number of %s arguments"

class RETLVALS(DMLError):
    """
    The number of return value recipients differs from the number of values
    the called method returns.
    """
    version = "1.4"
    fmt = "wrong number of return value recipients: Expected %d, got %d"

class RETARGS(DMLError):
    """
    The number of return values in a return statement must match the number
    of outputs in the method.
    """
    version = "1.4"
    fmt = "wrong number of return values: Expected %d, got %d"

class ARGD(DMLError):
    """
    All parameter names of a method must be distinct.
    """
    fmt = "duplicate method parameter name '%s'"

class ARGT(DMLError):
    """
    The data type of the argument value given for the mentioned method
    parameter differs from the method definition.
    """
    fmt = ("wrong type in %s parameter %s when %s '%s'\n"
           "got:      '%s'\n"
           "expected: '%s'")
    def __init__(self, site, invocation_type, method_name,
                 got_type, pref, ptype, direction):
        if invocation_type == 'call':
            invok = "calling"
        elif invocation_type == 'inline':
            invok = "inlining"
        elif invocation_type == 'implement':
            invok = "implementing"
        DMLError.__init__(self, site,
                          direction, pref, invok, method_name,
                          got_type, ptype)

class NARGT(DMLError):
    """
    Methods that are called must have data type declarations for all
    their parameters. (Methods that are only inlined do not need this.)
    """
    fmt = "no type for %s parameter %s"
    def __init__(self, site, pref, direction, callsite = None):
        DMLError.__init__(self, site, direction, pref)
        self.callsite = callsite
    def log(self):
        DMLError.log(self)
        if self.callsite:
            self.print_site_message(self.callsite, "called from here")

class PTYPE(DMLError):
    """
    The data type of the argument value given for the mentioned
    method or function parameter differs from the function prototype.
    """
    fmt = ("wrong type for parameter %s in %s call\n"
           "got:      %s\n"
           "expected: %s")
    def __init__(self, site, arg, ptype, pref, kind):
        DMLError.__init__(self, site, pref, kind, arg.ctype(), ptype)

class NAMECOLL(DMLError):
    """
    The name is already in use in the same scope.
    """
    fmt = "name collision on '%s'"
    def __init__(self, site, othersite, name):
        DMLError.__init__(self, site, name)
        self.othersite = othersite
    def log(self):
        DMLError.log(self)
        if self.othersite:
            self.print_site_message(self.othersite, "conflicting definition")

class NALLOW(DMLError):
    """
    Many object types have limitations on the contexts in which they may
    appear.
    """
    fmt = "this object is not allowed here"
    def __init__(self, site, parent):
        DMLError.__init__(self, site)

class NALLOC(DMLError):
    """
    An object which is not allocated at run-time cannot be referenced as
    a run-time value.
    """
    fmt = "object is not allocated at run-time: %s"
    def __init__(self, site, reg):
        DMLError.__init__(self, site, reg.identity())

class NTMPL(DMLError):
    """
    The template has not been defined.
    """
    fmt = "unknown template: '%s'"
    def __init__(self, site, name):
        if name.startswith('@'):
            # should be caught earlier
            raise ICE(site, 'missing template for ' + name[1:])
        DMLError.__init__(self, site, name)

class ISINTPL(DMLError):
    """
    A `template` block inside a `trait` block may not
    contain any `is` statements on top level. Templates should be
    instantiated with an `is` statement in the surrounding
    `trait` block.
    """
    # Traits no longer exist in 1.4, and are undocumented in 1.2
    version = "undocumented"
    fmt = ("'is' statement forbidden for template block inside trait;"
           + " please move to surrounding trait block")

class INVOVER(DMLError):
    """
    Only default declarations of parameters can be overridden.
    """
    fmt = "invalid override of non-default declaration %s"
    def __init__(self, site, non_default_site, name):
        DMLError.__init__(self, site, name)
        self.non_default_site = non_default_site
    def log(self):
        DMLError.log(self)
        self.print_site_message(self.non_default_site, "overridden assignment")

class NPARAM(DMLError):
    """
    The parameter has been declared, but is not assigned a value or a
    default value.
    """
    fmt = "no assignment to parameter '%s'"
    def __init__(self, site, name):
        DMLError.__init__(self, site, name)

class AUTOPARAM(DMLError):
    """Some parameters are predefined by DML, using the `auto`
    keyword. Such parameters may only be declared by the standard
    library, and they may not be overridden."""
    fmt = "bad declaration of automatic parameter '%s'"

class NOVERRIDEPARAM(DMLError):
    """When the `explict_param_decls` provisional feature is enabled, parameter
    definitions written using `=` and `default` are only accepted if the
    parameter has already been declared.
    To declare and define a new parameter not already declared, use the `:=` or
    `:default` syntax.
    """
    version = "1.4"
    fmt = ("parameter '%s' not declared previously."
           " To declare and define a new parameter, use the ':%s' syntax.")

    def log(self):
        from . import provisional
        DMLError.log(self)
        prov_site = self.site.provisional_enabled(
            provisional.explicit_param_decls)
        self.print_site_message(
            prov_site,
            "enabled by the explicit_param_decls provisional feature")

class OVERRIDEPARAM(DMLError):
    """When the `explict_param_decls` provisional feature is enabled,
    any parameter declared via `:=` or `:default` may not already
    have been declared. This means `:=` or `:default` syntax can't be used
    to override existing parameter declarations (not even those lacking a
    definition of the parameter.)
    """
    version = "1.4"
    fmt = ("the parameter '%s' has already been declared "
           + "(':%s' syntax may not be used for parameter overrides)")
    def __init__(self, site, other_site, name, token):
        super().__init__(site, name, token)
        self.other_site = other_site
    def log(self):
        DMLError.log(self)
        self.print_site_message(self.other_site, "existing declaration")

class EXTENSION(DMLError):
    """When the [`explicit_object_extensions` provisional
    feature](provisional-auto.html#explicit_object_extensions) is enabled,
    any object definition made via `in` syntax is considered an extension such
    that there must be some other non-extension declaration of the object, or
    DMLC will reject the extension.
    To declare and define a new object not already declared, omit the `in`
    syntax.
    """
    version = "1.4"
    fmt = ("object '%s' not declared elsewhere."
           " To declare and define a new object, omit 'in'.")

class MULTIOBJDECL(DMLError):
    """When the [`explicit_object_extensions` provisional
    feature](provisional-auto.html#explicit_object_extensions) is enabled,
    any object declaration not made using `in` syntax is considered a
    declaration of a novel object &mdash; because of that, DMLC will reject
    it if there already is another non-`in` declaration across files enabling
    `explicit_object_extensions`.
    """
    version = "1.4"
    fmt = ("object '%s' already declared."
           " To extend upon the definition of an object, use 'in %s'")
    def __init__(self, site, other_site, objtype, name):
        super().__init__(site, name, f'{objtype} {name} ...')
        self.other_site = other_site

    def log(self):
        from . import provisional
        DMLError.log(self)
        self.print_site_message(self.other_site, "existing declaration")
        prov_site = self.site.provisional_enabled(
            provisional.explicit_object_extensions)
        self.print_site_message(
            prov_site,
            "enabled by the explicit_object_extensions provisional feature")

class VARPARAM(DMLError):
    """
    The value assigned to the parameter is not a well-defined constant.
    """
    fmt = "non-constant parameter, or circular parameter dependencies: '%s'"

class RECPARAM(DMLError):
    """
    The value of a parameter may not reference the parameter itself,
    neither directly nor indirectly.
    """
    fmt = "circular dependency in parameter value"
    def __init__(self, sites):
        DMLError.__init__(self, sites[0])
        self.other_sites = sites[1:]
    def log(self):
        DMLError.log(self)
        for site in self.other_sites:
            self.print_site_message(site, "via here")

class IDXVAR(DMLError):
    """Expressions that are evaluated statically to constants cannot have
    different values for different elements in a register array.  This
    includes, for instance, the `allocate` parameter in
    registers and fields, and object-level `if` statements.
    """
    fmt = "expression may not depend on the index variable %s"
    def __init__(self, site, var):
        DMLError.__init__(self, site, var)

class INDEPENDENTVIOL(DMLError):
    """Expressions that depend on values stored in a device instance cannot be
    evaluated in contexts where the device instance is not available. This
    is within static contexts &mdash; for example when initializing typed
    template parameters  &mdash; or within independent methods."""
    fmt = "cannot access device instance in device independent context"

class TYPEDPARAMVIOL(DMLError):
    """Independent method calls are not allowed within the definitions of
    typed parameters."""
    fmt = ("typed parameter definitions may not contain independent methods "
           + "calls")

class FARRSZ(DMLError):
    """
    The bit width must be identical across the elements of a field array.
    """
    fmt = "heterogeneous bitsize in field array"

class DVAR(DMLError):
    """
    A local variable has more than one definition in the same code block.
    """
    fmt = "duplicate definition of variable '%s'"
    def __init__(self, site, othersite, name):
        DMLError.__init__(self, site, name)
        self.othersite = othersite
    def log(self):
        DMLError.log(self)
        if self.othersite:
            self.print_site_message(self.othersite, "conflicting definition")

class DDEFMETH(DMLError):
    """
    If a method has two default implementations, then at least one
    of them must be defined in a template.
    """
    fmt = "conflicting default definitions for method '%s'"
    def __init__(self, site, othersite, name):
        DMLError.__init__(self, site, name)
        self.othersite = othersite
    def log(self):
        DMLError.log(self)
        self.print_site_message(self.othersite, "conflicting definition")

class DMETH(DMLError):
    """
    A method can only be overridden if it is declared as `default`
    """
    fmt = "attempt to override non-default method '%s'"
    def __init__(self, site, othersite, name):
        DMLError.__init__(self, site, name)
        self.othersite = othersite
    def log(self):
        DMLError.log(self)
        self.print_site_message(self.othersite, "conflicting definition")

class METH(DMLError):
    """
    The default implementation is overridden by an implementation with
    different input/output parameters.
    """
    fmt = "incompatible method definitions: %s"
    def __init__(self, site, othersite, reason):
        DMLError.__init__(self, site, reason)
        self.othersite = othersite
    def log(self):
        DMLError.log(self)
        if self.othersite:
            self.print_site_message(self.othersite, "conflicting definition")

class NOVERRIDEMETH(DMLError):
    """When the `explict_method_decls` provisional feature is enabled, method
    definitions written using `{ ... }` and `default { ... }` are only accepted
    if the method has already been declared.

    To declare and define a new method not already declared, use the `:{ ... }`
    or `:default { ... }` syntax.
    """
    version = "1.4"
    fmt = ("method '%s' not declared previously."
           " To declare and define a new method, use the ':%s{...}' syntax.")

    def log(self):
        from . import provisional
        DMLError.log(self)
        prov_site = self.site.provisional_enabled(
            provisional.explicit_method_decls)
        self.print_site_message(
            prov_site,
            "enabled by the explicit_method_decls provisional feature")

class OVERRIDEMETH(DMLError):
    """When the `explict_method_decls` provisional feature is enabled,
    any method declared via `:{ ... }` or `:default { ... }` may not already
    have been declared. This means `:{ ... }` or `:default { ... }` syntax
    can't be used to override existing parameter declarations (not even those
    lacking a definition of the parameter.)
    """
    version = "1.4"
    fmt = ("the method '%s' has already been declared "
           + "(':%s{ ... }' syntax may not be used for method overrides)")
    def __init__(self, site, other_site, name, token):
        super().__init__(site, name, token)
        self.other_site = other_site
    def log(self):
        DMLError.log(self)
        self.print_site_message(self.other_site, "existing declaration")

class IMPLMEMBER(DMLError):
    """
    A method in an `implement` object corresponds to a struct member
    that isn't a function pointer
    """
    fmt = "The interface struct member %s is not a function pointer"
    def __init__(self, site, name, othersite):
        DMLError.__init__(self, site, name)
        self.othersite = othersite
    def log(self):
        DMLError.log(self)
        self.print_site_message(self.othersite, "interface struct definition")

class ANONPORT(DMLError):
    """
    An `implement` definition can only exist in a port or bank
    that has a name.
    """
    fmt = "an anonymous %s cannot implement interfaces"
    def __init__(self, site, port):
        self.port = port
        DMLError.__init__(self, site, port.objtype)
    def log(self):
        DMLError.log(self)
        self.print_site_message(self.port.site,
                                "this is the %s" % self.port.objtype)

class DBFUNC(DMLError):
    """
    The device contains two differently-named banks that use the same
    function number.
    """
    fmt = "duplicate bank function number: %d"
    def __init__(self, site, othersite, func):
        DMLError.__init__(self, site, func)
        self.othersite = othersite
    def log(self):
        DMLError.log(self)
        if self.othersite:
            self.print_site_message(self.othersite.site, "conflicting bank")

class REGNSZ(DMLError):
    """
    All registers must have a specified constant size.
    """
    fmt = "undefined register size for '%s'"
    def __init__(self, reg):
        DMLError.__init__(self, reg, reg.identity())

class REGISZ(DMLError):
    """
    The specified register size is not allowed. Possible values are 1-8.
    """
    fmt = "illegal register size for '%s'"
    def __init__(self, reg):
        DMLError.__init__(self, reg, reg.identity())

class REGOL(DMLError):
    """
    The registers are mapped to overlapping address ranges.
    """
    fmt = "overlapping registers: '%s' and '%s'"
    def __init__(self, reg1, reg2, coord1, coord2):
        self.other = reg2
        DMLError.__init__(self, reg1.site,
                          reg1.identity(coord1), reg2.identity(coord2))
    def log(self):
        DMLError.log(self)
        self.print_site_message(
            self.other.site,
            "register %s defined here" % (self.other.identity()))

class BITRR(DMLError):
    """
    The bit range of a field can only use bits present in the
    register.
    """
    fmt = "bit range of field '%s' outside register boundaries"
    def __init__(self, f):
        DMLError.__init__(self, f, f.identity())

class BITRO(DMLError):
    """
    The fields of a register must not overlap.
    """
    fmt = "bit range of field '%s' overlaps with field '%s'"
    def __init__(self, f1, i1, f2, i2):
        self.other = f2
        DMLError.__init__(self, f1,
                          f1.identity(i1), f2.identity(i2))
    def log(self):
        DMLError.log(self)
        self.print_site_message(
            self.other.site,
            "field %s defined here" % (self.other.identity(),))

class BITRN(DMLError):
    """
    The size of the bit range must be positive. Note that the [msb:lsb]
    syntax requires that the most significant bit (msb) is written to the
    left of the colon, regardless of the actual bit numbering used.
    """
    fmt = "negative size (%d < %d) of bit range for '%s'"
    def __init__(self, f, low, high):
        DMLError.__init__(self, f, low, high, f.identity())

class BITO(DMLError):
    """
    The specified bit-order is not allowed.
    """
    fmt = "illegal bitorder: '%s'"

class DEVIMP(DMLError):
    """
    Source files that are used with `import` directives may not
    contain `device` declarations.
    """
    fmt = "cannot import file containing device declaration"

class IMPRET(DMLError):
    """
    Methods within an `interface` declaration may have only have
    zero or one output parameter.
    """
    fmt = "more than one output parameter not allowed in interface methods"

class RECUR(DMLError):
    """
    Methods may not be inlined recursively.
    """
    fmt = "recursive inline of %s"
    def __init__(self, site, method):
        DMLError.__init__(self, site, method.identity())

class CONSTP(DMLError):
    """
    C function called with a pointer to a constant value for a parameter
    declared without const in the prototype.
    """
    fmt = "passing const reference for nonconst parameter %s in %s"

class CONST(DMLError):
    """
    The lvalue that is assigned to is declared as a `const` and
    thus can't be assigned to.
    """
    fmt = "assignment to constant"
    def __init__(self, site):
        DMLError.__init__(self, site);

class FUNSTRUCT(DMLError):
    """
    A member of a struct cannot have a function type.
    """
    fmt = "struct member is a function"

class FUNARRAY(DMLError):
    """
    It is illegal to express an array type where the base type is a
    function type.
    """
    fmt = "illegal type: array of functions"

class CONSTFUN(DMLError):
    """
    A function type cannot be `const` qualified;
    """
    fmt = "const qualified function type"

class DISCONST(DMLError):
    """
    A pointer to a constant value has been assigned to a pointer to a
    non-constant.
    """
    fmt = "const qualifier discarded"
    def __init__(self, site):
        DMLError.__init__(self, site);

class FMTARGT(DMLError):
    """
    Argument type mismatch in a log-statement format string.
    """
    fmt = ("wrong type for argument %d of format string ('%s')\n"
           "expected %s, got '%s'")
    def __init__(self, site, expr, n, expected):
        DMLError.__init__(self, site, n, expr, expected, expr.ctype())

class ILLCOMP(DMLError):
    """
    The values being compared do not have matching types.
    """
    fmt = ("illegal comparison; mismatching types\n"
           "LH: '%s' has type '%s'\n"
           "RH: '%s' has type '%s'")
    def __init__(self, site, expr1, typ1, expr2, typ2):
        DMLError.__init__(self, site,
                          truncate(str(expr1), 10), typ1,
                          truncate(str(expr2), 10), typ2)

class ARRAY(DMLError):
    """
    A whole array cannot be used as a single value.
    """
    fmt = "cannot use an array as a value: '%s'"
    def __init__(self, site, a):
        DMLError.__init__(self, site, a)

class MEMBER(DMLError):
    """
    Attempt to access a nonexisting member of a compound data structure.
    """
    fmt = "'%s' has no member named '%s'"
    def __init__(self, site, expr, member):
        DMLError.__init__(self, site, expr, member)

class IFTYPE(DMLError):
    """
    The interface datatype is unknown.
    """
    fmt = "unknown interface type: %s"

class VERS(DMLError):
    """
    A device declared to be written in one DML language version tried to
    import a file written in an incompatible language version.
    """
    fmt = "incompatible version (%s) while compiling a %s device"
    def __init__(self, site, impsite, impvers, devvers):
        DMLError.__init__(self, site, impvers, devvers)
        self.impsite = impsite
    def log(self):
        DMLError.log(self)
        self.print_site_message(self.impsite, "imported here")

class LAYOUT(DMLError):
    """
    The type of a member of a `layout` declaration must be an integer or
    bitfield with a bit width that is a multiple of 8, or another layout.
    """
    fmt = "illegal layout definition: %s"
    def __init__(self, site, msg):
        DMLError.__init__(self, site, msg)

class BFLD(DMLError):
    """
    A `bitfield` declaration must have an integer type that
    matches the width of the field.
    """
    fmt = "illegal bitfields definition: %s"
    def __init__(self, site, msg):
        DMLError.__init__(self, site, msg)

class INTPTRTYPE(DMLError):
    """
    Pointer types that point to integers with a bit width that is not
    a power of two are not allowed.
    """
    fmt = "illegal pointer type: %s"

class RVAL(DMLError):
    """
    The operand of `sizeof`, `typeof` and `&` must
    be a lvalue.
    """
    fmt = "operand of '%s' is not an lvalue"

class INC(DMLError):
    """
    An increment or decrement operation can only be performed on simple
    lvalues such as variables.
    """
    def __init__(self, site, hint=None):
        DMLError.__init__(self, site)
        self.hint = hint
    def log(self):
        DMLError.log(self)
        if self.hint:
            self.print_site_message(self.site, self.hint)
    fmt = "illegal increment/decrement operation"

class NTYPE(DMLError):
    """
    This expression has an unknown type.
    """
    fmt = "unknown type of expression"

class DATAINIT(DMLError):
    """
    An invalid initializer was detected. The error message provides
    the detailed information.
    """
    fmt = "invalid data initializer: %s"

class NOFILE(DMLError):
    """
    The main input file could not be found.
    """
    fmt = "file not found"

class NSHARED(DMLError):
    """If a template provides an object that is not accessible from shared
    methods, such as an untyped parameter or a non-shared method, then
    that object's name is reserved within the scope of the shared
    method.  I.e., if a shared method tries to access a symbol that
    isn't accessible, then ENSHARED is reported, even before looking
    for the symbol in the global scope. Section
    [x](language.html#shared-methods) describes which template symbols
    are accessible from a shared method.
    """
    version = "1.4"
    fmt = "%s in template %s does not belong to the template type"
    def __init__(self, site, fmt, template, decl_site):
        DMLError.__init__(self, site, fmt, template)
        self.decl_site = decl_site
    def log(self):
        DMLError.log(self)
        if self.decl_site:
            self.print_site_message(self.decl_site, "declared here")

class SERIALIZE(DMLError):
    """Some complex types, in particular most pointer types, cannot be
    automatically checkpointed by DML, and are therefore disallowed in
    contexts such as `saved` declarations.
    """
    fmt = "unserializable type: %s"

class ATTRCOLL(DMLError):
    """
    This error is signalled if two DML declarations would result in two
    Simics attributes being registered with the same name.

    This most commonly happens when an attribute name is a result of the
    object hierarchy, and there is another object named similarly. For example,
    if a bank contains one register named `g_r` and
    a group `g` containing a register named `r`.
    """

    fmt = "Declaration would result in conflicting attribute name"
    def __init__(self, site, othersite):
        DMLError.__init__(self, site)
        self.othersite = othersite

    def log(self):
        DMLError.log(self)
        self.print_site_message(self.othersite, "conflicting definition")

class STOREDINLINE(DMLError):
    """You cannot declare session or saved variables in methods marked with
    'inline'"""
    fmt = "Cannot declare '%s' variable in an inline method"

class SWITCH(DMLError):
    """A switch statement must start with a `case` label, and there
    may be at most one `default` label which must appear after
    all `case` labels"""
    fmt = "malformed switch statement: %s"

class VLALEN(DMLError):
    """
    <tt>.len</tt> cannot be used with variable-length arrays
    """
    fmt = "'.len' cannot be used with variable-length arrays"

class SAVEDCONST(DMLError):
    """
    Declaring a saved variable with a type that is (partially) const-qualified
    is not allowed, as they can be modified due to checkpoint restoration.
    """
    fmt = "saved variable declared with (partially) const-qualified type %s"

class VLACONST(DMLError):
    """
    Variable length arrays may not be declared const-qualified or with a base
    type that is (partially) const-qualified.
    """
    fmt = ("variable length array declared with (partially) const-qualified "
           + "type")

class IDENTSIZEOF(DMLError):
    """
    A variant of the EIDENT message exclusive to usages of `sizeof`: it is
    emitted when the operand of `sizeof` makes use of an identifier which is
    not present in value scope, but *is* present in type scope.
    This likely means `sizeof` was used when `sizeoftype` was intended.
    """
    fmt = ("unknown value identifier in the operand of 'sizeof': '%s'\n"
           + "'%s' is a valid type identifier. Did you mean to use "
           + "'sizeoftype'?")
    def __init__(self, site, identifier):
        DMLError.__init__(self, site, identifier, identifier)

class LOGGROUPS(DMLError):
    """
    Too many log groups were declared. A device may have a maximum of 63
    `loggroup` declarations (61 excluding the built-in `Register_Read` and
    `Register_Write` loggroups).
    """
    fmt = ("Too many loggroup declarations. A maximum of 63 log groups (61 "
           + "excluding builtins) may be declared per device.")

class NOPROV(DMLError):
    """
    An invalid identifier was passed in the `provisional` statement.
    """
    fmt = "No such provisional feature %s. Valid values are: %s"

class TQMIC(DMLError):
    """A template-qualified method implementation call can only be done if
    the specified template is actually instantiated by the object."""
    fmt = ("invalid template-qualified method implementation call, '%s' does "
           + "not instantiate '%s'")

class AMBTQMIC(DMLError):
    """A template-qualified method implementation call was made, when the
    template inheritance graph for specified template is insufficient to infer
    that one implementation overrides the others.
    To resolve this, the template-qualified method implementation call should
    instead be qualified with the specific ancestor template that has the
    desired implementation.
    """
    fmt = ("Ambiguous invocation of template-qualified method implementation "
           + "call. '%s' does not provide an implementation of '%s', and "
           + "inherits multiple unrelated implementations from its ancestor "
           + "templates.%s")
    def __init__(self, site, template, method, spec_elims, candidates):
        extra = (("\nnote: some implementation(s) have been eliminated by "
                  + "#if statements, which may have caused the ambiguity.")
                 * spec_elims)
        extra += ("\nresolution: qualify the call instead by the "
                  + "ancestor template with the desired implementation:")
        DMLError.__init__(self, site, template, method, extra)
        self.candidates = candidates
    def log(self):
        DMLError.log(self)
        for (ancestor, candidate) in self.candidates:
            self.print_site_message(
                candidate.site,
                "implementation candidate provided by ancestor template "
                + f"'{ancestor.name}'")

class MEMBERTQMIC(DMLError):
    """A template-qualified method implementation call can only be done if
    the specified template actually does provide or inherit an implementation
    of the named method for the object instantiating the template. That the
    template provides or inherits an abstract declaration of the method is not
    sufficient.

    Apart from more mundane causes (e.g. misspellings), this error could happen
    if all implementations that the specified template may provide/inherit end
    up not being provided to the object instantiating the template, due to
    every implementation being eliminated by an `#if` statement.
    """
    fmt = ("invalid template-qualified method implementation call, '%s' does "
           + "not provide nor inherit an implementation of a method "
           + "'%s'%s")
    def __init__(self, site, template, method, abstract, node):
        self.template = template
        if node:
            extra = (f" for the object '{node.identity()}': all "
                     + "implementations that could have been given have been "
                     + "eliminated by #if statements")
        elif abstract:
            extra = ", only an abstract declaration of it"
        else:
            extra = ""
        DMLError.__init__(self, site, template.name, method, extra)

    def log(self):
        DMLError.log(self)
        self.print_site_message(self.template.site, "template declaration")

class NSHAREDTQMIC(DMLError):
    """<a id="ENSHAREDTQMIC"/>
    A template-qualified method implementation call via a value of template
    type, including when `this.templates` is used within the body of a
    `shared` method, can only be done if the specified template provides or
    inherits a `shared` implementation of the specified method. If an
    implementation is never provided or inherited by the template, or the
    template provides or inherits a non-`shared` implementation, then the call
    can't be made.

    For example, the following is permitted:
    ```
    template t {
        shared method m();
    }

    template u is t {
        shared method m() default {
            log info: "implementation from 'u'";
        }
    }

    template v is t {
        shared method m() default {
            log info: "implementation from 'v'";
        }
    }

    template uv is (u, v) {
        shared method m() {
            // 'this' is a value of the template type 'uv'
            this.templates.u.m();
            // Equivalent to 'this.templates.v.m()'
            templates.v.m();
        }
    }
    ```

    But the following is not:
    ```
    template t {
        shared method m();
    }

    template u is t {
        shared method m() default {
            log info: "implementation from 'u'";
        }
    }

    template v is t {
        method m() default {
            log info: "implementation from 'v'";
        }
    }

    template uv is (u, v) {
        // Indirection as a shared implementation is not allowed to override a
        // non-shared implementation, but even if it were...
        method m() {
            m_impl();
        }

        shared method m_impl() {
            this.templates.u.m();
            // This is rejected because the implementation of 'm' provided by
            // 'v' is not shared.
            this.templates.v.m();
        }
    }
    ```

    As a result, resolving a conflict between a non-`shared` method
    implementation and a `shared` method implementation can typically only be
    done by having most parts of the overriding implementation be non-`shared`:
    ```
    template uv is (u, v) {
        method m() {
            // OK; 'this' is a compile-time reference to the object
            // instantiating the template rather than a value of template type.
            this.templates.u.m();
            this.templates.v.m();
        }
    }
    ```

    Alternatively, a new `shared` method with non-`shared` implementation can
    be declared to allow access to the specific non-`shared` implementation
    needed (at the cost of increasing the memory overhead needed for the
    template type):
    ```
    template uv is (u, v) {
        method m() {
            m_impl();
        }

        shared method m_impl_by_v();
        method m_impl_by_v() {
            this.templates.v.m();
        }

        shared method m_impl() {
            this.templates.u.m();
            // OK
            m_impl_by_v();
        }
    }
    ```
    """
    fmt = ("invalid template-qualified method implementation call made via a "
           + "value of template type: '%s' does not provide nor inherit a "
           + "shared implementation of '%s'")

class TTQMIC(DMLError):
    """A template-qualified method implementation call via a value of template
    type, including when `this.templates` is used within the body of a `shared`
    method, can only be done if the specified template is an ancestor template
    of the template type, the `object` template type, or the template type
    itself."""
    version = "1.4"
    fmt = ("invalid template-qualified method implementation call, "
           + "'%s' not a subtemplate of '%s'")

class EXTERNINCOMP(DMLError):
    """Multiple `extern` declarations with mismatching types are given for the
    same identifier."""
    fmt = "incompatible extern declarations for '%s': type mismatch"
    def __init__(self, site, other_site, name, typ, other_type):
        DMLError.__init__(self, site, name)
        self.other_site = other_site
        self.typ = typ
        self.other_type = other_type
    def log(self):
        DMLError.log(self)
        self.print_site_message(
            self.site,
            "this declaration specifies the type: " + self.typ.describe())
        self.print_site_message(
            self.other_site,
            "conflicting declaration, which specifies the type: "
            + self.other_type.describe())

class PRAGMA(DMLError):
    """
    An unknown pragma was specified
    """
    fmt = "Unknown pragma: %s"

class OLDVECT(DMLError):
    """`vect` types are only permitted if the [`simics_util_vect` provisional
    feature](provisional-auto.html#simics_util_vect) is enabled."""
    fmt = "declaration of vect type without simics_util_vect provisional"

class DISCARDREF(DMLError):
    """
    The expression `_` resolves to the [discard
    reference](language.html#discard-reference), and can only be used as an
    assignment target, in order to e.g. throw away return values of a function.
    """
    version = "1.4"
    fmt = ("'_' can only be used as an assignment target "
           + "(to discard some value)")


all_errors = {
    o.tag(): o for o in globals().values()
    if isinstance(o, type)
    and issubclass(o, DMLError)
    and o is not DMLError}
