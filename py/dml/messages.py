# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

from .logging import (DMLError, DMLWarning, SimpleSite, PortingMessage, ICE,
                      dollar, truncate, binary_dump)

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
    """<a id="WLOGMIXUP"/>

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
    (due to the breaking change `enable_WLOGMIXUP`.)
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

class WSTRAYIS(DMLWarning):
    """
    A standalone `is` statement was found that looks like it was instead
    intended to affect a preceding object declaration rather than the enclosing
    object/template in which the `is` statement and (sub)object declaration are
    made.

    This typically happens due to a stray semicolon before the `is`, e.g.:
    ```
    field f @ [31:0]; is read_only;
    ```
    or
    ```
    field f @ [31:0];
        is read_only;
    ```

    If done unintentionally, address this warning by making the `is` part of
    the declared object. If there is indeed a stray semicolon this can
    typically be accomplished simply by removing it.

    If the standalone `is` statement is intentional, silence this warning
    by making sure the `is` statement is on a new line separate from the object
    declaration, and is not indented any deeper than the object declaration is.
    """
    fmt = ("suspect standalone 'is': formatting suggests it was meant to "
           + "affect the %s declared just before it rather than the "
           + "enclosing object/template. "
           + "Perhaps you have a stray ';' before the 'is'?")

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
