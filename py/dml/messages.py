# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

from .logging import (DMLError, DMLWarning, SimpleSite, PortingMessage, ICE,
                      dollar)

def truncate(s, maxlen):
    "Make sure that s is not longer than maxlen"
    if len(s) > maxlen:
        return s[:maxlen-3] + '...'
    return s

def binary_dump(lh, rh):
    """Produce a string to use in warning and error messages describing
    operands to a binary operation"""
    return ("LH: '%s' of type '%s'\n"
            "RH: '%s' of type '%s'"
            % (truncate(str(lh), 40), lh.ctype(),
               truncate(str(rh), 40), rh.ctype()))

def unary_dump(rh):
    """Produce a string to use in warning and error messages describing
    operands to a binary operation"""
    return ("RH: '%s' of type '%s'"
            % (truncate(str(rh), 40), rh.ctype()))

class EAFTER(DMLError):
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
                f"\nmethod parameter '{pname}' is of unserializable type: "
                + f"{ptype}"
                for (pname, ptype) in self.unserializable or []))

class EAFTERSENDNOW(DMLError):
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
                f"\nmessage component {idx} is of unserializable type: "
                + f"{ptype}"
                for (idx, ptype) in unserializable))

        on_hook = (f"bound to hook '{target_hook}'"
                   if target_hook is not None else '')
        DMLError.__init__(self, site, on_hook, callback_hook, callback_hook,
                          clarification, unserializable_msg)

class EAFTERHOOK(DMLError):
    """
    An illegal hook-bound `after` statement was specified.
    The number of message component parameters must be equal to the number of
    message components of the hook.
    """
    version = "1.4"
    fmt = ("illegal 'after' statement bound to hook '%s': "
           + 'hook has %d message components, but %d message component '
           + 'parameters are given')

class EAFTERMSGCOMPPARAM(DMLError):
    """Message component parameters bound by a hook-bound after statement can
    only be used as direct arguments to the specified callback method, and
    cannot be used in arbitrary expressions.
    """
    version = "1.4"
    fmt = ("'%s' is a message component parameter, and can only be used as a "
           "direct argument to the callback method of the after statement")

class EHOOKTYPE(DMLError):
    """There are some minor restrictions to a hook's message component
    types. Anonymous structs and arrays of variable/unknown size are not
    supported.
    """
    version = "1.4"
    fmt = ("'%s' is a not a valid message component type for a hook, as it "
           "is or contains some %s")

class ECYCLICIMP(DMLError):
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

class ECYCLICTEMPLATE(DMLError):
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

class EAMBINH(DMLError):
    """If a method or parameter has multiple definitions, then there must
    be a unique definition that overrides all other definitions.
    """
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

class EAMBDEFAULT(DMLError):
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

class EAMETH(DMLError):
    """
    An abstract method cannot override another method.
    """
    fmt = "abstract method %s overrides existing method"

    def __init__(self, site, prev_site, name):
        DMLError.__init__(self, site, name)
        self.prev_site = prev_site

    def log(self):
        DMLError.log(self)
        self.print_site_message(
            self.prev_site, "previous declaration")

class ETMETH(DMLError):
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

class ETEMPLATEUPCAST(DMLError):
    """When casting to a template type, the source expression must be either
    an object implementing the template, or an expression whose type is a
    subtemplate of the target type."""
    version = "1.4"
    fmt = "invalid upcast, %s not a subtemplate of %s"

class EABSTEMPLATE(DMLError):
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

class EIMPORT(DMLError):
    """
    The file to imported could not be found. Use the `-I`
    option to specify additional directories to search for imported
    files.
    """
    fmt = "cannot find file to import: %s"
    def __init__(self, site, filename):
        DMLError.__init__(self, site, filename)

class ESIMAPI(DMLError):
    """
    The DML file is written in a too old version of DML. Use the
    `--simics-api` option to use a sufficiently old Simics API.
    """
    fmt = "DML version %s does not support API version %s"
    def __init__(self, site, dml_ver, api_ver):
        DMLError.__init__(self, site, dml_ver, api_ver)

class ETYPE(DMLError):
    """
    The data type is not defined in the DML code.
    """
    fmt = "unknown type: '%s'"

class EVARTYPE(DMLError):
    """A variable has been declared with a given type but the type is
    not acceptable.
    """
    fmt = "variable or field declared %s"

class ETREC(DMLError):
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

class EANONSTRUCT(DMLError):
    """
    Declarations of new structs are not permitted in certain contexts,
    such as method arguments, `new` expressions,
    `sizeoftype` expressions and `cast` expressions.
    """
    fmt = "struct declaration not allowed in a %s"

class EEMPTYSTRUCT(DMLError):
    """
    A struct or layout type must have at least one field.
    This restriction does not apply to structs declared in a
    `extern typedef`.
    """
    fmt = "struct or layout with no fields"

class ECAST(DMLError):
    """
    The cast operation was not allowed.  It is illegal to cast to void.
    """
    fmt = "illegal cast to '%s'"
    def __init__(self, site, expr, type):
        DMLError.__init__(self, site, type)

class EVOID(DMLError):
    """The type `void` is not a value, and thus cannot be used as
    the type of e.g. a variable or struct member"""
    fmt = "illegal use of void type"

class ENBOOL(DMLError):
    """
    Conditions must be properly boolean expressions; e.g., "`if (i ==
    0)`" is allowed, but "`if (i)`" is not, if `i` is an
    integer.
    """
    fmt = "non-boolean condition: '%s' of type '%s'"
    def __init__(self, expr):
        DMLError.__init__(self, expr, expr, expr.ctype())

class EASSIGN(DMLError):
    """
    The target of the assignment is not an l-value, and thus cannot be
    assigned to.
    """
    fmt = "cannot assign to this expression: '%s'"
    def __init__(self, site, target):
        DMLError.__init__(self, site, target)

class EASTYPE(DMLError):
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

class EINCTYPE(DMLError):
    """
    The prefix and postfix increment/decrement operators can only be
    used on integer and pointer expressions.
    """
    fmt = ("wrong type for '%s' operator")

class EBTYPE(DMLError):
    """
    An expression had the wrong type.
    """
    fmt = ("wrong type\n"
           "got:      %s\n"
           "expected: %s")

class ECSADD(DMLError):
    """
    Non-constant strings cannot be concatenated using `+`.
    """
    fmt = ("non-constant strings cannot be concatenated using '+'")

class EEARG(DMLError):
    """
    Function and method arguments in declarations cannot be of
    endian integer type.
    """
    fmt = ("cannot use endian integer as argument type in declaration")

class EASSINL(DMLError):
    """
    The target of the assignment is a method parameter that has been
    given a constant or undefined value when inlining the method.
    """
    fmt = "cannot assign to inlined parameter: '%s'"
    def __init__(self, site, name):
        DMLError.__init__(self, site, name)

class EERRSTMT(DMLError):
    """
    The source code contained a statement "`error;`", which
    forces a compilation error with the given message, or the standard message
    "forced compilation error in source code".
    """
    fmt = "%s"
    def __init__(self, site, msg):
        DMLError.__init__(self, site, msg)

class EEXTERN(DMLError):
    """An extern declared method must be fully typed and may not throw
    exceptions."""
    fmt = "illegal declaration of extern method"
    version = "1.2"

class EEXPORT(DMLError):
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

class ESTATICEXPORT(DMLError):
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

class EINVALID(DMLError):
    """
    The expression does not produce a proper value.
    """
    fmt = "invalid expression: '%s'"
    def __init__(self, expr):
        DMLError.__init__(self, expr, expr)

class EUNDEF(DMLError):
    """
    Caused by an attempt to generate code for an expression that
    contains the `undefined` value.
    """
    fmt = "undefined value: '%s'"
    def __init__(self, site, expr = None):
        if expr is None:
            expr = site
        DMLError.__init__(self, site, expr)

class ESHNEG(DMLError):
    """
    The right-hand side operand to a shift operator must not be negative.
    """
    fmt = "shift with negative shift count: '%s"

class EDIVZ(DMLError):
    """
    The right-hand side of the given / or % operator is always zero.
    """
    fmt = "right-hand side operand of '%s' is zero"

# TODO: also check bitwise or/xor for type errors.
class EBINOP(DMLError):
    """
    One or both of the operands have the wrong type for the given binary
    operator.
    """
    fmt = "illegal operands to binary '%s' \n%s"
    def __init__(self, site, op, lh, rh):
        DMLError.__init__(self, site, op, binary_dump(lh, rh))

class EBSLICE(DMLError):
    """
    A bitslice operation was attempted on an expression that is not an
    integer.
    """
    fmt = "illegal bitslice operation"

class EBSSIZE(DMLError):
    """
    Bit slices cannot be larger than 64 bits.
    """
    fmt = "bitslice size of %s bits is not between 1 and 64"

class EBSBE(DMLError):
    """A big-endian bit slice can only be done on an expression whose type
    is explicitly defined, such as a local variable or a register field."""
    fmt = "bitslice with big-endian bit order and uncertain bit width"
    def __init__(self, site):
        DMLError.__init__(self, site)

class EZRANGE(DMLError):
    """
    An array index range must start at zero.
    """
    fmt = "array range must start at 0"
    def __init__(self, site):
        DMLError.__init__(self, site)

class ENARRAY(DMLError):
    """
    Indexing can only be applied to arrays, integers (bit-slicing),
    and lists.
    """
    fmt = "trying to index something that isn't an array: '%s'"
    def __init__(self, expr):
        DMLError.__init__(self, expr, expr)

class EOOB(DMLError):
    """
    The used index is outside the defined range.
    """
    fmt = "array index out of bounds"
    def __init__(self, expr):
        DMLError.__init__(self, expr)

class EAVAR(DMLError):
    """
    Indexing into constant lists can only be done with constant indexes.
    """
    fmt = "cannot use variable index in a constant list"

class ENLST(DMLError):
    """
    A list was expected.
    """
    fmt = "not a list: %s"

class ENVAL(DMLError):
    """
    Only some objects can be used as values directly. An attribute can
    only be accessed directly as a value if it has been declared using the
    `allocate_type` parameter.
    """
    fmt = "not a value: %s"

class ENORET(DMLError):
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

class EATYPE(DMLError):
    """
    Either the `attr_type` or the `type` parameter of the
    attribute must be specified.
    """
    fmt = "attribute type undefined: %s"
    def __init__(self, attr):
        DMLError.__init__(self, attr, attr.identity())

class EANAME(DMLError):
    """
    This name is not available as the name of an attribute, since it is
    used for an automatically added attribute.
    """
    fmt = "illegal attribute name: %s"

class EACHK(DMLError):
    """
    An attribute must have set and get methods to be
    checkpointable. This attribute has neither, and the
    'configuration' parameter is either "required" or "optional".
    """
    fmt = "checkpointable attribute missing set or get method"

class EANULL(DMLError):
    """
    An attribute must have a set or a get method to be useful.
    """
    fmt = "attribute has no get or set method"

class EREGVAL(DMLError):
    """
    When a register has been specified with explicit fields, you have to
    use the `get` and `set` methods to access the register as
    a single value.
    """
    fmt = "cannot use a register with fields as a value: %s"
    def __init__(self, site, reg):
        DMLError.__init__(self, site, reg.identity())

class ENOPTR(DMLError):
    """
    A pointer value was expected.
    """
    fmt = "not a pointer: %s (%s)"
    def __init__(self, site, expr):
        DMLError.__init__(self, site, expr, expr.ctype().describe())

class ENOSTRUCT(DMLError):
    """
    The left-hand side operand of the `.` operator is not of struct
    type.
    """
    fmt = "trying to get a member of a non-struct: '%s' of type '%s'"
    def __init__(self, site, expr, ctype = None):
        DMLError.__init__(self, site, expr, ctype or expr.ctype())

class EBADFAIL(DMLError):
    """
    An exception is thrown in a context where it will not be caught.
    """
    fmt = "uncaught exception"

class EBADFAIL_dml12(DMLError):
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

class EAPPLY(DMLError):
    """
    The applied value is not a function.
    """
    fmt = ("illegal function application of '%s'\n"
           "type: %s")
    def __init__(self, fun, ftype = None):
        if not ftype:
            ftype = fun.ctype()
        DMLError.__init__(self, fun, fun, ftype)

class EAPPLYMETH(DMLError):
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

class EIDENT(DMLError):
    """
    The identifier has not been declared anywhere.
    """
    fmt = "unknown identifier: '%s'"

    def __init__(self, site, name):
        DMLError.__init__(self, site, name)
        self.identifier = name

class ENAMEID(DMLError):
    """
    The name parameter does not follow identifier syntax.
    """
    fmt = "invalid name parameter value: '%s'"

class EFORMAT(DMLError):
    """
    The log-statement format string is malformed.
    """
    fmt = "malformed format string: unknown format at position %d"

class EDEVICE(DMLError):
    """
    The main source file given to the DML compiler must contain a
    `device` declaration.
    """
    fmt = "missing device declaration"

class ELTYPE(DMLError):
    """
    Log-statement type must be one of `info`, `warning`, `error`,
    `spec_viol`, and `unimpl`.
    """
    fmt = "invalid log type: '%s'"

class ELLEV(DMLError):
    """
    The log level given in a log statement must be an integer between 1 and 4,
    or 1 and 5 for a subsequent log level (`then ...`), unless the log kind is
    one of "warning", "error", or "critical", in which case it must be 1 (or 5
    for subsequent log level).
    """
    fmt = "log level must be %s"

class ESYNTAX(DMLError):
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

class EPARAM(DMLError):
    """
    The parameter is not bound to a legal value.
    """
    fmt = "illegal value for parameter '%s'"

class EUNINITIALIZED(DMLError):
    """
    Some parameters that are automatically supplied by DML
    cannot be accessed in early stages of compilation, such as in object-level
    if statements.
    """
    fmt = "value of parameter %s is not yet initialized"

class ECONDP(DMLError):
    """
    It is not permitted to declare a parameter directly inside an
    `if` conditional.
    """
    fmt = "conditional parameters are not allowed"

class ECONDT(DMLError):
    """
    It is not permitted to use a template directly inside an
    `if` conditional.
    """
    fmt = "conditional templates are not allowed"

class ECONDINEACH(DMLError):
    """
    It is not permitted to have an `in each` statement directly
    inside an `if` conditional.
    """
    version = "1.4"
    fmt = "conditional 'in each' is not allowed"

# TODO: Consider re-wording the semantics of this error, allocate_type is only
# relevant in 1.4 when imported from 1.2, and as per SIMICS-9393 this
# error might not even be necessary
class EATTRDATA(DMLError):
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

class ERETTYPE(DMLError):
    """
    The type of the return value (if any) must be specified for methods
    that implement interfaces.
    """
    fmt = "no return type"
    def __init__(self, meth):
        DMLError.__init__(self, meth)

class ERETARGNAME(DMLError):
    """
    In DML 1.4, the output arguments of a method are anonymous
    """
    version = "1.4"
    fmt = "method return type declarations may not be named: %s"

class EIFREF(DMLError):
    """
    Interface function calls must be simple references to the method.
    """
    fmt = "illegal interface method reference: %s"

class EREF(DMLError):
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

class ENOBJ(DMLError):
    """
    A reference to an object was expected.
    """
    fmt = "object expected: %s"

class EFMTARGN(DMLError):
    """
    The log-statement has too few or too many arguments for the given
    format string.
    """
    fmt = "wrong number of arguments for format string"

class EASZVAR(DMLError):
    """
    The size of an array must be a constant integer.
    """
    fmt = "array upper bound is not a constant integer: %s"

class EASZR(DMLError):
    """
    An array must have at least one element.
    """
    fmt = "array size is less than 1"
    def __init__(self, site):
        DMLError.__init__(self, site)

class EASZLARGE(DMLError):
    """
    Object arrays with huge dimensions are not allowed; the product of
    dimension sizes must be smaller than 2<sup>31</sup>.
    """
    # It would be cheap to bump the limit to 2**32 elements, but that
    # would require some additional testing to check that we never use
    # signed 32-bit integer arithmetic on packed indices.
    fmt = f"array has too many elements (%d >= {2**31})"

class EAINCOMP(DMLError):
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

class EAUNKDIMSIZE(DMLError):
    """
    The size of an array dimension of an object array must be defined at least
    once across all declarations of that object array.
    """
    fmt = ("the size of dimension %d (with index variable '%s') is never "
           + "defined")

class ENCONST(DMLError):
    """
    A constant expression was expected.
    """
    fmt = "non-constant expression: %s"

class ECONT(DMLError):
    """
    A `continue` statement can only be used inside a loop construct.
    """
    fmt = "nothing to continue"

class ECONTU(DMLError):
    """
    A `continue` statement cannot be used in a `#foreach`
    or `#select` statement.
    """
    fmt = "continue is not possible here"

class EBREAK(DMLError):
    """
    A `break` statement can only be used inside a loop or switch
    construct.
    """
    fmt = "nothing to break from"

class ENMETH(DMLError):
    """
    A method name was expected. This might be caused by using
    `call` or `inline` on something that counts as a C
    function rather than a method.
    """
    fmt = "not a method: '%s'"

class ENDEFAULT(DMLError):
    """
    The default implementation of a method was invoked, but there was
    no default implementation.
    """
    fmt = "no default implementation"

class EARG(DMLError):
    """
    The number of input/output arguments given in the call differs from
    the method definition.
    """
    fmt = "wrong number of %s arguments"

class ERETLVALS(DMLError):
    """
    The number of return value recipients differs from the number of values
    the called method returns.
    """
    version = "1.4"
    fmt = "wrong number of return value recipients: Expected %d, got %d"

class ERETARGS(DMLError):
    """
    The number of return values in a return statement must match the number
    of outputs in the method.
    """
    version = "1.4"
    fmt = "wrong number of return values: Expected %d, got %d"

class EARGD(DMLError):
    """
    All parameter names of a method must be distinct.
    """
    fmt = "duplicate method parameter name '%s'"

class EARGT(DMLError):
    """
    The data type of the argument value given for the mentioned method
    parameter differs from the method definition.
    """
    fmt = ("wrong type in %s parameter '%s' when %s '%s'\n"
           "got:      '%s'\n"
           "expected: '%s'")
    def __init__(self, site, invocation_type, method_name,
                 got_type, pname, ptype, direction):
        if invocation_type == 'call':
            invok = "calling"
        elif invocation_type == 'inline':
            invok = "inlining"
        elif invocation_type == 'implement':
            invok = "implementing"
        DMLError.__init__(self, site,
                          direction, pname, invok, method_name,
                          got_type, ptype)

class ENARGT(DMLError):
    """
    Methods that are called must have data type declarations for all
    their parameters. (Methods that are only inlined do not need this.)
    """
    fmt = "no type for %s parameter '%s'"
    def __init__(self, site, pname, direction, callsite = None):
        DMLError.__init__(self, site, direction, pname)
        self.callsite = callsite
    def log(self):
        DMLError.log(self)
        if self.callsite:
            self.print_site_message(self.callsite, "called from here")

class EPTYPE(DMLError):
    """
    The data type of the argument value given for the mentioned
    method or function parameter differs from the function prototype.
    """
    fmt = ("wrong type for parameter %s in %s call\n"
           "got:      %s\n"
           "expected: %s")
    def __init__(self, site, arg, ptype, argname, kind):
        DMLError.__init__(self, site, argname, kind, arg.ctype(), ptype)

class ENAMECOLL(DMLError):
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

class ENALLOW(DMLError):
    """
    Many object types have limitations on the contexts in which they may
    appear.
    """
    fmt = "this object is not allowed here"
    def __init__(self, site, parent):
        DMLError.__init__(self, site)

class ENALLOC(DMLError):
    """
    An object which is not allocated at run-time cannot be referenced as
    a run-time value.
    """
    fmt = "object is not allocated at run-time: %s"
    def __init__(self, site, reg):
        DMLError.__init__(self, site, reg.identity())

class ENTMPL(DMLError):
    """
    The template has not been defined.
    """
    fmt = "unknown template: '%s'"
    def __init__(self, site, name):
        if name.startswith('@'):
            # should be caught earlier
            raise ICE(site, 'missing template for ' + name[1:])
        DMLError.__init__(self, site, name)

class EISINTPL(DMLError):
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

class EINVOVER(DMLError):
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

class ENPARAM(DMLError):
    """
    The parameter has been declared, but is not assigned a value or a
    default value.
    """
    fmt = "no assignment to parameter '%s'"
    def __init__(self, site, name):
        DMLError.__init__(self, site, name)

class EAUTOPARAM(DMLError):
    """Some parameters are predefined by DML, using the `auto`
    keyword. Such parameters may only be declared by the standard
    library, and they may not be overridden."""
    fmt = "bad declaration of automatic parameter '%s'"

class ENOVERRIDE(DMLError):
    """When the `explict_param_decls` provisional feature is enabled, parameter
    definitions written using `=` and `default` are only accepted if the
    parameter has already been declared.
    To declare and define a new parameter not already declared, use the `:=` or
    `:default` syntax.
    """
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


class EOVERRIDE(DMLError):
    """When the `explict_param_decls` provisional feature is enabled,
    any parameter declared via `:=` or `:default` may not already
    have been declared. This means `:=` or `:default` syntax can't be used
    to override existing parameter declarations (not even those lacking a
    definition of the parameter.)
    """
    fmt = ("the parameter '%s' has already been declared "
           + "(':%s' syntax may not be used for parameter overrides)")
    def __init__(self, site, other_site, name, token):
        super().__init__(site, name, token)
        self.other_site = other_site
    def log(self):
        DMLError.log(self)
        self.print_site_message(self.other_site, "existing declaration")

class EVARPARAM(DMLError):
    """
    The value assigned to the parameter is not a well-defined constant.
    """
    fmt = "non-constant parameter, or circular parameter dependencies: '%s'"

class ERECPARAM(DMLError):
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

class EIDXVAR(DMLError):
    """Expressions that are evaluated statically to constants cannot have
    different values for different elements in a register array.  This
    includes, for instance, the `allocate` parameter in
    registers and fields, and object-level `if` statements.
    """
    fmt = "expression may not depend on the index variable %s"
    def __init__(self, site, var):
        DMLError.__init__(self, site, var)

class EINDEPENDENTVIOL(DMLError):
    """Expressions that depend on values stored in a device instance cannot be
    evaluated in contexts where the device instance is not available. This
    is within static contexts &mdash; for example when initializing typed
    template parameters  &mdash; or within independent methods."""
    fmt = "cannot access device instance in device independent context"

class ETYPEDPARAMVIOL(DMLError):
    """Independent method calls are not allowed within the definitions of
    typed parameters."""
    fmt = ("typed parameter definitions may not contain independent methods "
           + "calls")

class EFARRSZ(DMLError):
    """
    The bit width must be identical across the elements of a field array.
    """
    fmt = "heterogeneous bitsize in field array"

class EDVAR(DMLError):
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

class EDDEFMETH(DMLError):
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

class EDMETH(DMLError):
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

class EMETH(DMLError):
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

class EIMPLMEMBER(DMLError):
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

class EANONPORT(DMLError):
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

class EDBFUNC(DMLError):
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

class EREGNSZ(DMLError):
    """
    All registers must have a specified constant size.
    """
    fmt = "undefined register size for '%s'"
    def __init__(self, reg):
        DMLError.__init__(self, reg, reg.identity())

class EREGISZ(DMLError):
    """
    The specified register size is not allowed. Possible values are 1-8.
    """
    fmt = "illegal register size for '%s'"
    def __init__(self, reg):
        DMLError.__init__(self, reg, reg.identity())

class EREGOL(DMLError):
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

class EBITRR(DMLError):
    """
    The bit range of a field can only use bits present in the
    register.
    """
    fmt = "bit range of field '%s' outside register boundaries"
    def __init__(self, f):
        DMLError.__init__(self, f, f.identity())

class EBITRO(DMLError):
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

class EBITRN(DMLError):
    """
    The size of the bit range must be positive. Note that the [msb:lsb]
    syntax requires that the most significant bit (msb) is written to the
    left of the colon, regardless of the actual bit numbering used.
    """
    fmt = "negative size (%d < %d) of bit range for '%s'"
    def __init__(self, f, low, high):
        DMLError.__init__(self, f, low, high, f.identity())

class EBITO(DMLError):
    """
    The specified bit-order is not allowed.
    """
    fmt = "illegal bitorder: '%s'"

class EDEVIMP(DMLError):
    """
    Source files that are used with `import` directives may not
    contain `device` declarations.
    """
    fmt = "cannot import file containing device declaration"

class EIMPRET(DMLError):
    """
    Methods within an `interface` declaration may have only have
    zero or one output parameter.
    """
    fmt = "more than one output parameter not allowed in interface methods"

class ERECUR(DMLError):
    """
    Methods may not be inlined recursively.
    """
    fmt = "recursive inline of %s"
    def __init__(self, site, method):
        DMLError.__init__(self, site, method.identity())

class ECONSTP(DMLError):
    """
    C function called with a pointer to a constant value for a parameter
    declared without const in the prototype.
    """
    fmt = "passing const reference for nonconst parameter %s in %s"

class ECONST(DMLError):
    """
    The lvalue that is assigned to is declared as a `const` and
    thus can't be assigned to.
    """
    fmt = "assignment to constant"
    def __init__(self, site):
        DMLError.__init__(self, site);

class EFUNSTRUCT(DMLError):
    """
    A member of a struct cannot have a function type.
    """
    fmt = "struct member is a function"

class EFUNARRAY(DMLError):
    """
    It is illegal to express an array type where the base type is a
    function type.
    """
    fmt = "illegal type: array of functions"

class ECONSTFUN(DMLError):
    """
    A function type cannot be `const` qualified;
    """
    fmt = "const qualified function type"

class EDISCONST(DMLError):
    """
    A pointer to a constant value has been assigned to a pointer to a
    non-constant.
    """
    fmt = "const qualifier discarded"
    def __init__(self, site):
        DMLError.__init__(self, site);

class EFMTARGT(DMLError):
    """
    Argument type mismatch in a log-statement format string.
    """
    fmt = ("wrong type for argument %d of format string ('%s')\n"
           "expected %s, got '%s'")
    def __init__(self, site, expr, n, expected):
        DMLError.__init__(self, site, n, expr, expected, expr.ctype())

class EILLCOMP(DMLError):
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

class EARRAY(DMLError):
    """
    A whole array cannot be used as a single value.
    """
    fmt = "cannot use an array as a value: '%s'"
    def __init__(self, site, a):
        DMLError.__init__(self, site, a)

class EMEMBER(DMLError):
    """
    Attempt to access a nonexisting member of a compound data structure.
    """
    fmt = "'%s' has no member named '%s'"
    def __init__(self, site, expr, member):
        DMLError.__init__(self, site, expr, member)

class EIFTYPE(DMLError):
    """
    The interface datatype is unknown.
    """
    fmt = "unknown interface type: %s"

class EVERS(DMLError):
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

class ELAYOUT(DMLError):
    """
    The type of a member of a `layout` declaration must be an integer or
    bitfield with a bit width that is a multiple of 8, or another layout.
    """
    fmt = "illegal layout definition: %s"
    def __init__(self, site, msg):
        DMLError.__init__(self, site, msg)

class EBFLD(DMLError):
    """
    A `bitfield` declaration must have an integer type that
    matches the width of the field.
    """
    fmt = "illegal bitfields definition: %s"
    def __init__(self, site, msg):
        DMLError.__init__(self, site, msg)

class EINTPTRTYPE(DMLError):
    """
    Pointer types that point to integers with a bit width that is not
    a power of two are not allowed.
    """
    fmt = "illegal pointer type: %s"

class ERVAL(DMLError):
    """
    The operand of `sizeof`, `typeof` and `&` must
    be a lvalue.
    """
    fmt = "operand of '%s' is not an lvalue"

class EINC(DMLError):
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

class ENTYPE(DMLError):
    """
    This expression has an unknown type.
    """
    fmt = "unknown type of expression"

class EDATAINIT(DMLError):
    """
    An invalid initializer was detected. The error message provides
    the detailed information.
    """
    fmt = "invalid data initializer: %s"

class ENOFILE(DMLError):
    """
    The main input file could not be found.
    """
    fmt = "file not found"

class ENSHARED(DMLError):
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

class ESERIALIZE(DMLError):
    """Some complex types, in particular most pointer types, cannot be
    automatically checkpointed by DML, and are therefore disallowed in
    contexts such as `saved` declarations.
    """
    fmt = "unserializable type: %s"

class EATTRCOLL(DMLError):
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

class ESTOREDINLINE(DMLError):
    """You cannot declare session or saved variables in methods marked with
    'inline'"""
    fmt = "Cannot declare '%s' variable in an inline method"

class ESWITCH(DMLError):
    """A switch statement must start with a `case` label, and there
    may be at most one `default` label which must appear after
    all `case` labels"""
    fmt = "malformed switch statement: %s"

class EVLALEN(DMLError):
    """
    <tt>.len</tt> cannot be used with variable-length arrays
    """
    fmt = "'.len' cannot be used with variable-length arrays"

class ESAVEDCONST(DMLError):
    """
    Declaring a saved variable with a type that is (partially) const-qualified
    is not allowed, as they can be modified due to checkpoint restoration.
    """
    fmt = "saved variable declared with (partially) const-qualified type %s"

class EVLACONST(DMLError):
    """
    Variable length arrays may not be declared const-qualified or with a base
    type that is (partially) const-qualified.
    """
    fmt = ("variable length array declared with (partially) const-qualified "
           + "type")

class EIDENTSIZEOF(DMLError):
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

class ELOGGROUPS(DMLError):
    """
    Too many log groups were declared. A device may have a maximum of 63
    `loggroup` declarations (61 excluding the built-in `Register_Read` and
    `Register_Write` loggroups).
    """
    fmt = ("Too many loggroup declarations. A maximum of 63 log groups (61 "
           + "excluding builtins) may be declared per device.")


class ENOPROV(DMLError):
    """
    An invalid identifier was passed in the `provisional` statement.
    """
    fmt = "No such provisional feature %s. Valid values are: %s"


class ETQMIC(DMLError):
    """A template-qualified method implementation call can only be done if
    the specified template is actually instantiated by the object."""
    fmt = ("invalid template-qualified method implementation call, '%s' does "
           + "not instantiate '%s'")

class EAMBTQMIC(DMLError):
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

class EMEMBERTQMIC(DMLError):
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


class ENSHAREDTQMIC(DMLError):
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

class ETTQMIC(DMLError):
    """A template-qualified method implementation call via a value of template
    type, including when `this.templates` is used within the body of a `shared`
    method, can only be done if the specified template is an ancestor template
    of the template type, the `object` template type, or the template type
    itself."""
    version = "1.4"
    fmt = ("invalid template-qualified method implementation call, "
           + "'%s' not a subtemplate of '%s'")

class EEXTERNINCOMP(DMLError):
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

class EPRAGMA(DMLError):
    """
    An unknown pragma was specified
    """
    fmt = "Unknown pragma: %s"

class EOLDVECT(DMLError):
    """`vect` types are only permitted if the [`simics_util_vect` provisional
    feature](provisional-auto.html#simics_util_vect) is enabled."""
    fmt = "declaration of vect type without simics_util_vect provisional"

#
# WARNINGS (keep these as few as possible)
#

class WNOVER(DMLWarning):
    """
    A DML file must start with a version statement, such as `dml 1.4;`
    """
    fmt = "file has no version tag, assuming version 1.2"

class WSHALL(DMLWarning):
    """
    The result of the shift operation will always be zero.
    (This warning is disabled by default.)
    """
    fmt = "shifting away all data\n%s"
    def __init__(self, node, lh, rh):
        DMLWarning.__init__(self, node, binary_dump(lh, rh))

class WNDOC(DMLWarning):
    """
    No documentation string was specified for the attribute.
    (This warning is disabled by default.)
    """
    fmt = "no documentation for '%s'"
    def __init__(self, node, member):
        DMLWarning.__init__(self, node, member)

class WNSHORTDESC(DMLWarning):
    """
    No short description string was specified using the 'desc' parameter.
    (This warning is disabled by default.)
    """
    fmt = "no 'desc' parameter specified for device"
    def __init__(self, node):
        DMLWarning.__init__(self, node)

class WNDOCRA(DMLWarning):
    """
    No documentation string was specified for a _required_ attribute.
    """
    fmt = "no documentation for required attribute '%s'"
    def __init__(self, node, member):
        DMLWarning.__init__(self, node, member)

class WNEGOFFS(DMLWarning):
    """
    A negative integer expression is given as a register offset.
    Register offsets are unsigned 64-bit numbers, which means that
    a negative offset expression translates to a very large offset.
    """
    fmt = "negative register offset: %d"

class WUNUSED(DMLWarning):
    """
    The object is not referenced anywhere.
    (This warning is disabled by default.; it typically causes many false
    warnings.)
    """
    fmt = "unused: %s"
    def __init__(self, obj):
        DMLWarning.__init__(self, obj, obj.identity())

class WUNUSEDDEFAULT(DMLWarning):
    """
    The object is not referenced anywhere but it matches a name of an
    object automatically referenced in another scope. This is the same
    as WUNUSED but only for known common errors and it will never be
    emitted if WUNUSED is enabled.
    """
    fmt = "unused: %s methods are not called automatically for %s objects in %s"
    def __init__(self, obj):
        DMLWarning.__init__(self, obj, obj.name, obj.parent.objtype,
                            obj.identity())

class WUNUSED_DML12(DMLWarning):
    """A DML 1.4 file contains a method implementation that would override
    a library method in DML 1.2, but which is not part of the DML 1.4
    library, because some methods have been renamed. For instance,
    implementing `read_access` in a register makes no sense
    in DML 1.4, because the method has been renamed to
    `read_register`.

    If a DML 1.4 file contains common code that also is imported from
    DML 1.2 devices, then it may need to implement methods like
    `read_access` to get the right callbacks when compiled
    for DML 1.2. Such implementations can be placed inside `#if
    (dml_1_2) { }` blocks to avoid this warning.
    """
    fmt = ("unused implementation of DML 1.2 method %s;"
           + " enclose in #if (dml_1_2) ?")
    def __init__(self, obj):
        DMLWarning.__init__(self, obj, obj.name)

class WDUPEVENT(DMLWarning):
    """
    Two or more events will be checkpointed using the same name, which
    means that the checkpoint cannot be safely read back.
    """
    fmt = "duplicate event checkpoint names: %s"
    def __init__(self, site, objlist):
        DMLWarning.__init__(self, site,
                            ", ".join(dollar(self.site) + o.logname()
                                      for o in objlist))

class WSIZEOFTYPE(DMLWarning):
    """
    The 'sizeof' operator is used on a type name, but expects an
    expression. Use the 'sizeoftype' operator for types.
    """
    fmt = "sizeof on a type is not legal, use sizeoftype instead"

class WDEPRECATED(DMLWarning):
    """
    This part of the language is deprecated, usually because the
    underlying support in Simics is deprecated.
    """
    fmt = "deprecation: %s"

class WEXPERIMENTAL(DMLWarning):
    """
    This part of the language is experimental, and not yet officially
    supported. Code relying on the feature may break without notice in
    future releases.
    """
    fmt = "Use of unsupported feature: %s"
    def preprocess(self):
        return super(WEXPERIMENTAL, self).preprocess()

class WEXPERIMENTAL_UNMAPPED(WEXPERIMENTAL):
    __doc__ = WEXPERIMENTAL.__doc__

class WCONFIDENTIAL(DMLWarning):
    """
    The object's name/qname is used as part of an expression in a
    context other than the log statement, which could potentially lead
    to the leak of confidential information.
    """
    fmt = "potential leak of confidential information"
    def __init__(self, site):
        DMLWarning.__init__(self, site)

# Not used (see ctree.py class CopyData), not documented.
# class WASSIGN(DMLWarning):
#     def __init__(self, site):
#         DMLWarning.__init__(self, site, "cannot perform assignment")

class WOLDAST(DMLWarning):
    """
    A precompiled DML file has an old time-stamp. This may happen if a
    user accidentally edits a DML file from the standard library. A
    safe way to suppress the warning is to remove the outdated
    `.dmlast` file.
    """
    fmt = "Outdated AST file: %s"
    def __init__(self, dmlfile):
        DMLWarning.__init__(self, SimpleSite(dmlfile + ":0"),
                            dmlfile + "ast")

class WWRNSTMT(DMLWarning):
    """
    The source code contained a statement "`warning;`", which
    causes a warning to be printed.
    """
    fmt = "%s"

class WSYSTEMC(DMLWarning):
    """ SystemC specific warnings """
    fmt = "%s"

    # This message should be removed, SIMICS-9886
class WREF(DMLWarning):
    """An unused parameter refers to an object that has not been declared.

    This warning message will be replaced with a hard error in future
    major versions of Simics.
    """
    instances = []
    fmt = "unused parameter %s contains %s"

    def __init__(self, site, param, eref):
        # message formatting hack is based on this assumption
        assert eref.msg.startswith('reference to unknown object ')
        DMLWarning.__init__(self, site, param, eref.msg)

class WTEMPLATEIS(DMLWarning):
    """In a template with methods marked `shared`, it is recommended that
    other templates are instantiated on the same line"""
    fmt = ("prefer 'is' statement outside template braces,"
           + " 'template ... is (x, y) {'")

class WNOIS(DMLWarning):
    """Many standard method overrides will only be recognized if a
    template named like the method is also instantiated. For instance,
    the method `set` in a field has no effect unless the
    `set` template is instantiated.
    """
    def __init__(self, site, name):
        DMLWarning.__init__(self, site, name, name)
    fmt = ("implementation of %s() without 'is %s' is ignored"
           + " by the standard library")

class WTHROWS_DML12(DMLWarning):
    """In DML 1.2, a method is by default permitted to throw an exception,
    while in DML 1.4, an annotation `throws` is required for that.
    So, if a method without annotations is ported to DML 1.4, it will
    no longer permit exceptions. If such method is overridden by
    a DML 1.2 file, then a non-throwing method is overridden by a potentially
    throwing method, which is normally a type error. However, this particular
    case is reduced to this warning. If an exception is uncaught in the
    override, then this will automatically be caught in runtime and
    an error message will be printed.
    """
    fmt = ("overriding non-throwing DML 1.4 method"
           + " with throwing DML 1.2 method")
    def __init__(self, site, other_site=None):
        DMLWarning.__init__(self, site)
        self.other_site = other_site
    def log(self):
        DMLWarning.log(self)
        self.print_site_message(
            self.other_site,
            "original non-throwing declaration")

class WNEGCONSTCOMP(DMLWarning):
    """DML uses a special method when comparing an unsigned and signed integer,
    meaning that comparing a negative constant to an unsigned integer always
    has the same result, which is usually not the intended behaviour."""
    def __init__(self, site, expr, ty):
        DMLWarning.__init__(self, site)
        self.expr = expr
        self.ty = ty
    fmt = ("Comparing negative constant to unsigned integer has a constant "
           + "result")
    def log(self):
        DMLError.log(self)
        self.print_site_message(
            self.expr.site, "Consider 'cast(%s, %s)'" % (self.expr, self.ty))

class WASTRUNC(DMLWarning):
    """The source of an assignment is a constant value that can't fit in the
    type of the target, and is thus truncated. This warning can be silenced by
    explicitly casting the expression to the target type.
    """
    fmt = ("The assignment source is a constant value which does not fit "
           + "the assign target of type '%s', and will thus be truncated")

class WREDUNDANTLEVEL(DMLWarning):
    """`X then Y` log level syntax has no effect when the
    first and subsequent levels are the same.
    """
    def __init__(self, site):
        DMLWarning.__init__(self, site)
    fmt = ("'X then Y' log level has no effect when the levels are the same")

class WTTYPEC(DMLWarning):
    """
    The delay value provided to an `after` call is subject to
    implicit type conversion which may be unexpected for certain types.
    To silence this warning, explicitly cast the delay value to the expected
    type.
    """
    fmt = ("the time value of type '%s' is implicitly converted "
           + "to the type '%s' expected by the specified time unit '%s'.")

class WPCAST(DMLWarning):
    """
    A pointer is cast to a base type which has incompatible representation
    compared to the original. Accessing the pointed-to object via the new
    pointer type will almost certainly constitute undefined behavior.

    This warning is extremely limited in scope: don't rely on it to catch every
    bad pointer cast.

    To silence this warning, first cast the pointer to `void *`, then cast it
    to the desired type.
    """
    fmt = ("very suspect pointer-to-pointer cast: the new base type has "
           + "incompatible representation. This could lead to your code "
           + "getting mangled by the C compiler, with unpredictable results.\n"
           + "old base type: %s\n"
           + "new base type: %s%s")
    def __init__(self, site, old, new, maybe_intended):
        suggestion = ('\nperhaps you meant the new base type to be '
                      + maybe_intended.describe()
                      if maybe_intended else '')
        DMLWarning.__init__(self, site, old, new, suggestion)

class WLOGMIXUP(DMLWarning):
    """
    A specified log level of a `log` looks as though you meant to specify the
    log groups instead, and/or vice versa. For example:
    ```
    // Log group used as log level, when the intention is instead to
    // specify log groups and implicitly use log level 1
    log spec_viol, some_log_group: ...;

    // Log groups and log level mistakenly specified in reverse order
    log info, (some_log_group | another_log_group), 2: ...;

    // Log level used as log groups, when the intention is instead to
    // specify the subsequent log level
    log info, 2, 3: ...;
    ```
    If you want to specify log groups, make sure to (explicitly) specify the
    log level beforehand. If you want to specify the subsequent log level, use
    `then` syntax.
    ```
    log spec_viol, 1, some_log_group: ...;
    log info, 2, (some_log_group | another_log_group): ...;
    log info, 2 then 3: ...;
    ```

    This warning is only enabled by default with Simics API version 7 or above
    (due to the compatibility feature `suppress_WLOGMIXUP`.)
    """
    fmt = ("log statement with likely misspecified log level(s) and log "
           + "groups: %s")
    def __init__(self, site, kind, level, later_level, groups):
        suggestions = []
        from .codegen import probable_loggroups_specification, \
            probable_loglevel_specification
        # There are three main scenarios for which we want to offer suggestions
        # -- those covered in the docstring. All other scenarios either involve
        # subsequent log levels -- at which point it's too difficult to guess
        # what the user actually wanted to do -- or have no obvious fix that is
        # not blatantly incorrect.
        if probable_loggroups_specification(level):
            if (not later_level
                and not (groups.constant and not (1 <= groups.value <= 4))):
                # Scenario 2: 'log info, 2, some_log_groups: ...;'
                details = ("the specified log level and log groups look as "
                           + "though they are meant to be reversed.")
                suggestions.append(f"log {kind}, {groups}, {level}: ...;")
            else:
                details = ("log group(s) and/or constant 0 are used as log "
                           + "level.")
                if not later_level and groups.constant and groups.value == 0:
                    # Scenario 1: 'log info, some_log_groups: ...;'
                    suggestions.append(f"log {kind}, 1, {level}: ...;")
        elif later_level and probable_loggroups_specification(later_level):
            details = ("log group(s) and/or constant 0 are used as subsequent "
                       + "log level.")
        else:
            assert probable_loglevel_specification(groups)
            details = "non-zero integer constant used as log groups."
            if not later_level:
                if site is None or site.dml_version != (1, 2):
                    # Scenario 3: 'log info, 2, 3: ...;'
                    suggestions.append(
                        f"log {kind}, {level} then {groups}: ...;")
                if (not probable_loglevel_specification(level)
                    and not (groups.constant and groups.value == 5)):
                    # Scenario 2: 'log info, nonconstant, 3: ...;'
                    suggestions.append(f"log {kind}, {groups}, {level}: ...;")

        if suggestions:
            details += (" Perhaps you meant%s:\n%s"
                        % (" one of the below"*(len(suggestions) > 1),
                           '\n'.join(suggestions)))

        DMLWarning.__init__(self, site, details)

class WIMMAFTER(DMLWarning):
    """
    An immediate `after` statement was specified where some argument to the
    callback is a pointer to some stack-allocated data &mdash; i.e. a pointer
    to data stored within a local variable. That data is guaranteed to be
    invalid by the point the callback is called, which presents an enormous
    security risk!
    """
    version = "1.4"
    fmt = ("***INCREDIBLY UNSAFE*** use of immediate 'after' statement: the "
           + "callback argument '%s' is a pointer to stack-allocated data!")

class WHOOKSEND(DMLWarning):
    """
    The `send` operation of a hook was called, and some provided message
    component is a pointer to some stack-allocated data &mdash; i.e. a pointer
    to data stored within a local variable. That data is guaranteed to be
    invalid by the point the message is sent, which presents an enormous
    security risk!

    If you must use pointers to stack-allocated data, then `send_now` should
    be used instead of `send`. If you want the message to be delayed to avoid
    ordering bugs, create a method which wraps the `send_now` call together
    with the declarations of the local variable(s) which you need pointers to,
    and then use immediate after (`after: m(...)`) to delay the call to that
    method.
    """
    version = "1.4"
    fmt = ("***INCREDIBLY UNSAFE*** use of the 'send' operation of a hook: "
           + "the message component '%s' is a pointer to stack-allocated "
           + "data!\n"
           + "Did you mean to use 'send_now' instead? See the Hook "
           + "Declarations section in the DML 1.4 reference manual for "
           + "information about the differences between 'send' and 'send_now'")


class PSHA1(PortingMessage):
    """The `port-dml` script requires that the DML file has not been
    changed since the tag file was generated. This is verified by a
    SHA1 checksum."""
    fmt = 'SHA1 checksum of DML file'

class PVERSION(PortingMessage):
    """DML 1.4 files should start with `dml 1.4;` instead
    of `dml 1.2;`"""
    fmt = "update version statement to 1.4"

class PNOTHROW(PortingMessage):
    """In DML 1.4, it is assumed by default that a method may not throw an
    exception. Any `nothrow` annotations must be removed."""
    fmt = "remove nothrow annotation"

class PTHROWS(PortingMessage):
    """In DML 1.4, methods that can throw an exception must explicitly
    declare that with a 'throws' annotation. Such annotation is
    automatically added for methods that override a throwing method
    that was declared in a DML 1.4 file."""
    fmt = "add throws annotation"

class PINPARAMLIST(PortingMessage):
    """If a method has no input arguments, the empty input parameter
    list `()` is now required in method declarations
    and `call`/`inline` statements."""
    fmt = "add () after method"

class PSTRUCTDECL(PortingMessage):
    """Struct type definitions can no longer be declared using the
    labeled struct syntax. The equivalent typedef declaration should
    be used instead. For example, the following is now disallowed:

    <pre>
    struct <var>xyz</var> { ... }
    </pre>

    and should be replaced with the following:

    <pre>
    typedef struct { ... } <var>xyz</var>;
    </pre>"""
    fmt = "use typedef syntax for struct type declaration"

class PFIELDRANGE(PortingMessage):
    """In a field declaration, the field range declaration must be
    preceded by `@`. In DML 1.4, the syntax `field f[4]`
    declares a field array of size 4, while in DML 1.2 it denotes a field
    occupying bit 4 in its parent register."""
    fmt = "insert @ before field range declaration"

class PINLINEDECL(PortingMessage):
    """Methods with untyped parameters must be explicitly marked 'inline'
    in DML 1.4, as in `inline method m(inline x) -> (int y)`"""
    fmt = "add inline annotation to method with untyped parameters"

class PRETVAL(PortingMessage):
    """Method output parameters are not named in DML 1.4:
    ```
    method m() -> (int, int) {
    ...
    }
    ```

    See also `PRETURNARGS`."""
    fmt = "remove name from method output parameter"

class PRETVAL_END(PortingMessage):
    """Methods with output arguments must end with an explicit return
    statement in DML 1.4; in DML 1.2, the method would return whatever
    value the output argument had upon exit. See also `PRETVAL`.
    """
    fmt = "add return statement to end of method"

class PRETURNARGS(PortingMessage):
    """In methods with return values, return statements now take arguments:
    ```
    method m() -> (int, int) {
        return (1, 4);
    }
    method n() -> (int) {
        return 3;
    }
    ```"""
    fmt = "add arguments to return statement"

class POUTARGRETURN(PortingMessage):
    """An assignment to an output argument directly followed by a
    return statement is more concisely written as a single return
    statement, without an intermediate assignment. For instance,
    ```
    dml 1.2;
    method four() -> (int x) {
       x = 4;
       return;
    }
    ```
    can be expressed as:
    ```
    dml 1.4;
    method four() -> (int) {
       return 4;
    }
    ```
    """
    fmt = "merge outarg assignment with return statement"

class PTYPEDOUTPARAM(PortingMessage):
    """Method output parameters must have explicit types in DML 1.4. The
    automatic conversion script will convert untyped output parameters
    to `uint64`."""
    fmt = "declare explicit type for output parameter"

class PINARGTYPE(PortingMessage):
    """When overriding a method in DML 1.4, the overriding and default
    implementation must declare the same arguments as `inline`.
    If the override has untyped arguments that are typed in the default
    method, then the conversion script will add a type to the override's
    argument.

    Note: The inserted type declaration uses C syntax, which in most
    cases matches DML syntax; however, there are exceptions where the
    inserted declaration will be broken and needs to be fixed manually.
    """
    fmt = ""

class PINVOKE(PortingMessage):
    """Method invocation syntax changed. Replace

    ```
    call m(x) -> (a, b);
    inline n() -> (c);
    call o();
    ```
    with:
    ```
    (a, b) = m(x);
    c = n();
    o();
    ```"""
    fmt = "Use assignment syntax for method invocation"

class PLOGKIND(PortingMessage):
    """In log statements, the old syntax where log type is denoted by a string
    has been removed. Use the new identifier-based syntax instead. E.g.,
    instead of:
    ```
    log "info": "foo";
    log "spec_violation": "foo";
    ```
    you must now write:
    ```
    log info: "foo";
    log spec_viol: "foo";
    ```
"""
    fmt = "Don't use string literal syntax for log type"

class PAFTER(PortingMessage):
    """The syntax of `after` statements changed: The delay should
    be followed by `s` to denote time unit; furthermore, the
    `call` keyword should no longer be used, and brackets
    around the delay are optional. Example:
    ```
    after (1.3) call send_frame();  // DML 1.2 syntax
    after 1.3 s: send_frame();       // DML 1.4 syntax
    ```"""
    fmt = "Remove 'call' and add 's' in 'after' statement"

class PAUTO(PortingMessage):
    """The `auto` keyword is deprecated; use the equivalent
    `local` instead."""
    fmt = "replace 'auto' with 'local'"

class PSESSION(PortingMessage):
    """The `data` and `static` keywords have both been replaced
    with `session`:
    ```
    session uint32 x;
    ```"""
    fmt = "replace 'data' and 'static' with 'session'"

class PHARD_RESET_VALUE(PortingMessage):
    """The `hard_reset_value` parameter is no longer
    recognized. The parameter is automatically renamed to
    `init_val`, which has roughly the same effect."""
    fmt = "change hard_reset_value to init_val"

class PSOFT_RESET_VALUE(PortingMessage):
    """The `soft_reset_value` parameter is renamed to
    `soft_reset_val`, and requires the template
    `soft_reset_val` to be instantiated."""
    fmt = "change soft_reset_value to soft_reset_val"

class PMISS_PATTERN(PortingMessage):
    """The `miss_pattern` parameter is no longer recognized by
    banks, unless the `miss_pattern_bank` template, from utility.dml, is
    instantiated.  An instantiation is automatically added."""
    fmt = "instantiate miss_pattern_bank to use the miss_pattern parameter"

class PATTRIBUTE(PortingMessage):
    """The `allocate_type` parameter is no longer valid for attributes.
    Integer, boolean and floating-point attributes instead use
    standard templates such as `uint64_attr`, `int64_attr`, `bool_attr` and
    `double_attr`.

    The porting rule will remove `allocate_type`, together with an
    explicit type parameter if present. All integer types will be
    changed to 64-bit types. Attributes with `allocate_type="string"`
    have to be manually rewritten.
    """
    fmt = "use uint64_attr template instead of allocate_type parameter"

class PEVENT(PortingMessage):
    """Event objects no longer have the `timebase` parameter;
    instead you must choose a standard template to instantiate. The
    conversion script will pick `custom_time_event`,
    `custom_cycle_event`, `simple_time_event`, or `simple_cycle_event`.
    """

class PEVENT_NO_ARG(PortingMessage):
    """When `PEVENT` converts an event to `simple_time_event` or
    `simple_cycle_event`, the `data` function argument to methods
    `post`, `posted`, `next` and `remove` are removed.
    """

class PEVENT_UINT64_ARG(PortingMessage):
    """When one of the methods `post`, `posted`, `next` and `remove`
    is called with an integer value cast to a pointer in the `data`
    arg, that event is converted to a `uint64_time_event` or
    `uint64_cycle_event`. The converter removes the cast and
    causes the event object to be converted to a
    `uint64_time_event` instead of `uint64_custom_event`.
    """

class PEVENT_REMOVE_INFO(PortingMessage):
    """When an event is converted to a `uint64_time_event` or
    `uint64_cycle_event`, the `set_event_info` and `get_event_info` methods
    are removed.
    """

class POVERRIDE(PortingMessage):
    """If a method has exactly one default and one non-default
    declaration, and the non-default declaration resides in a
    template, then DML 1.4 requires that this template inherits from
    the template containing the default declaration. Conversion is done
    automatically for the most common case."""
    fmt = "template with method override must instantiate overridden template"

class POVERRIDE_IMPORT(PortingMessage):
    """Similar to POVERRIDE, but if a default method does _not_
    reside in a template, then the file that instantiates the non-default
    declaration must import the file containing the default declaration."""
    fmt = "template with method override must import overridden file"

# TODO: convert <fun> <em> <var> <v> <pre>

class PBEFAFT(PortingMessage):
    """In objects of type attribute, register and connect, before/after
    methods (e.g. `before_write`, `after_read`,
    `after_set`), are no longer called. They are transformed
    into an override of the corresponding base function
    (`write_register`, `read_register`, `set`).
    Note that when a before/after method is implemented by a template,
    then an additional `is register` declaration may be needed in
    1.4; this is not automatically added.
    """
    fmt = "before / after method no longer called, override base method instead"

class PABSTRACT_TEMPLATE(PortingMessage):
    """When implementing methods `read` or `write` in a
    field or register, or `hard_reset` or
    `soft_reset` in a bank, or `get`, `set`,
    `read_field`, `write_field` in a field, then this will
    have no effect unless a template named like the method is instantiated. The
    automatic converter will add e.g. `is read;` before an
    implementation of `read`."""
    fmt = ("method has no effect unless corresponding abstract template is"
           + " instantiated")

class PTRAMPOLINE(PortingMessage):
    """The methods `miss_read_access` and
    `miss_write_access` in `bank` objects have been
    renamed to `unmapped_read` and `unmapped_write`
    in 1.4. The converter creates methods with the new names, which
    call the existing unmodified methods."""
    fmt = "method has been renamed in 1.4, will insert trampoline method"

class PIMPORT_DML12COMPAT(PortingMessage):
    """The `PTRAMPOLINE` porting rule sometimes requires
    that `dml12-compatibility.dml` is imported."""
    fmt = ''

class PCHANGE_INARGS(PortingMessage):
    """Many overridable methods in the standard library have changed, and
    method overrides must be changed accordingly. A method in 1.2
    usually has a counterpart in 1.4, but its semantics may have
    changed slightly and quite often the set of arguments has changed
    as well.  The automatic converter will adjust the names and
    signatures of a number of methods. Invocations, including
    `default()`, are not updated and must be manually converted
    (compile errors). Some method arguments are removed by the
    converter; if these arguments are used, the implementation must be
    modified accordingly. In particular, the `memop` arg of
    various bank and register methods is no longer present. This has
    been replaced with an argument `void *aux`, which is
    normally NULL.  If some register in a bank needs a memop, then the
    `io_memory_access` method can be updated to pass down the
    memop in the `aux` argument (just call `default`
    with `memop` in the last arg). An explicit call to a bank
    method, e.g. a redirection of an access to a different bank,
    should normally be rewritten as a call to `read` or
    `write`, usually with NULL in the `aux`
    argument.

    Bank methods are converted as follows: `access` →
    `io_memory_access`; `read_access`,
    `read_access_memop` → `read`;
    `write_access`, `write_access_memop` →
    `write`

    Register methods are converted like so: `read_access`
    → `read_register`, `write_access` →
    `write_register`

    Field methods are converted thusly: `read_access`
    → `read_field`, `write_access` →
    `write_field`

    In `register` and `field` objects, the
    `set` and
    `write_register`/`write_field` methods will get
    an explicit type `uint64`.

    The read and write methods on `bank`, `register`,
    and `field` objects all take a new `uint64` argument
    denoting enabled bytes or bits, depending on the context, which
    may mask an access.

    In `connect` objects, `validate_port` is converted
    to `validate`; named ports are deprecated in Simics 6, but
    the port name is available in the `port` session variable.

    In `attribute` objects, the `set` method will get
    an explicit argument type `attr_value_t`, and a
    `throws` annotation.
    """
    fmt = "override of library method with new name or signature"

class PBITNEQ(PortingMessage):
    """DML 1.2 permits using 1-bit fields as boolean values. In DML 1.4,
    field values are 64 bit, and thus require an explicit `!= 0`"""
    fmt = ""

class PVAL(PortingMessage):
    """The value of a `register`, `field` or `attribute`
    object, and the interface struct of a `interface` object, is now
    accessed through the `.val` member."""
    fmt = "use .val member to access register/attribute value"

class PNODOLLAR(PortingMessage):
    """The `$` character is no longer needed when referencing objects."""
    fmt = "remove $"

class PDOLLAR_QUALIFY(PortingMessage):
    """In DML 1.4, there is no separate scope for `$`, so local
    variables can shadow object references. This conversion rule
    attempts to detect this, and add `this.` or
    <tt>dev.<i>path</i>.</tt> where needed."""
    fmt = "use qualified object reference to escape shadowing"

class PCONSTANT(PortingMessage):
    """`constant` declarations are removed in 1.4 and should be
    replaced with `param` declarations. Both are accessible
    from the top-level scope of a device."""
    fmt = "change 'constant' to 'param'"

class PPARAMETER(PortingMessage):
    """The `parameter` keyword has been renamed to `param`."""
    fmt = "change 'parameter' to 'param'"

class PARRAY_I(PortingMessage):
    """The syntax for object arrays has changed: Instead of
    `register r[12]`, you write `register r[i < 12]`"""
    fmt = "explicit index in object array declaration"

class PARRAY(PortingMessage):
    """The syntax for object arrays has changed: Instead of
    `register r[j in 0..size - 1]`, you write
    `register r[j < size]`"""
    fmt = "'<' replaces 'in 0..' in object array declaration"

class PHASH(PortingMessage):
    """`if` statements on top level must be prefixed with a `#`
    character. The same applies to `if` statement inside a method body
    if the condition is constant and the dead branch contains errors.

    Similarly, the conditional operator (`? :`) is updated to #? #:
    when needed, `select` is updated to `#select`, and
    `foreach` is updated to `#foreach`."""
    fmt = "insert '#' before 'if'"

class PHASHELSE(PortingMessage):
    """If an `if` is updated to `#if`, and there is a
    corresponding `else` clause, then it must be updated to
    `#else`. The same applies to the `else` clause in
    `select` statements."""
    fmt = "insert '#' before 'else'"

class PANDOR(PortingMessage):
    """DML 1.2 permits the right operand of an `&&` or
    `||` expression to contain errors, as long as the left
    operand evaluates to a constant that makes the right operand
    dead. DML 1.4 does not permit this, so the expression `A
    && B` must be converted to `A #? B #: false`.
    One common use case is expressions like
    `(defined X && X.y == ...)`.
    """
    fmt = ''

class PIFAND(PortingMessage):
    """If statements on the form `if (defined(X) && X.y) { ... }`
    are converted to a nested `#if` statement"""
    fmt = ''

class PSTRINGIFY(PortingMessage):
    """In DML 1.4, `#` is changed to the
    `stringify` operator"""
    fmt = "Replace '#(X)' with 'stringify(X)' operator"

class PWUNUSED(PortingMessage):
    """This message is used to signal to the porting script that a piece
    of conditional code was not fully covered by the compile. Some
    portings are never applied on dead code. The porting script
    can signal a warning for this."""
    fmt = ""
    typed_methods = set()
    # site -> objects.Method
    inline_methods = {}
    inlined_methods = set()
    positive_conds = set()
    negative_conds = set()
    satisfied_conds = set()
    used_templates = set()

class PNO_WUNUSED(PortingMessage):
    """Used in conjunction with PWUNUSED to signal that a piece of
    conditional code was indeed used. If using multiple runs of DMLC
    as input to the porting script, and a piece of code was used in
    some runs and unused in others, then no warning is shown.
    """
    fmt = ""

class PRENAME_TEMPLATE(PortingMessage):
    """Some templates in `utility.dml` have been renamed; in
    particular, `unimplemented` has been renamed to
    `unimpl`.
    """
    fmt = ""

class PUNDEFOFFS(PortingMessage):
    """`undefined` is no longer a valid offset for registers; `unmapped_offset`
    should be used instead.
    """
    fmt = "Use 'unmapped_offset' instead of 'undefined'"

class PINT1(PortingMessage):
    """Integer types have different semantics in DML 1.2 and DML 1.4;
    the `int1` type in DML 1.2 is converted to `uint1` in DML 1.4
    because that is a better match for some common operations. In
    particular, if the value 1 is assigned to variables of these
    types, then the value of the variable becomes 1, whereas for
    `int1` in DML 1.4 the value is -1."""
    fmt = "Change int1 to uint1"

warnings = {name: cls for (name, cls) in globals().items()
            if isinstance(cls, type) and issubclass(cls, DMLWarning)
            and cls is not DMLWarning}
