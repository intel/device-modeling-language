# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

from .logging import DMLWarning, binary_dump, dollar, SimpleSite

class NOVER(DMLWarning):
    """
    A DML file must start with a version statement, such as `dml 1.4;`
    """
    fmt = "file has no version tag, assuming version 1.2"

class SHALL(DMLWarning):
    """
    The result of the shift operation will always be zero.
    (This warning is disabled by default.)
    """
    fmt = "shifting away all data\n%s"
    def __init__(self, node, lh, rh):
        DMLWarning.__init__(self, node, binary_dump(lh, rh))

class NDOC(DMLWarning):
    """
    No documentation string was specified for the attribute.
    (This warning is disabled by default.)
    """
    fmt = "no documentation for '%s'"
    def __init__(self, node, member):
        DMLWarning.__init__(self, node, member)

class NSHORTDESC(DMLWarning):
    """
    No short description string was specified using the 'desc' parameter.
    (This warning is disabled by default.)
    """
    fmt = "no 'desc' parameter specified for device"
    def __init__(self, node):
        DMLWarning.__init__(self, node)

class NDOCRA(DMLWarning):
    """
    No documentation string was specified for a _required_ attribute.
    """
    fmt = "no documentation for required attribute '%s'"
    def __init__(self, node, member):
        DMLWarning.__init__(self, node, member)

class NEGOFFS(DMLWarning):
    """
    A negative integer expression is given as a register offset.
    Register offsets are unsigned 64-bit numbers, which means that
    a negative offset expression translates to a very large offset.
    """
    fmt = "negative register offset: %d"

class UNUSED(DMLWarning):
    """
    The object is not referenced anywhere.
    (This warning is disabled by default.; it typically causes many false
    warnings.)
    """
    fmt = "unused: %s"
    def __init__(self, obj):
        DMLWarning.__init__(self, obj, obj.identity())

class UNUSEDDEFAULT(DMLWarning):
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

class UNUSED_DML12(DMLWarning):
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

class DUPEVENT(DMLWarning):
    """
    Two or more events will be checkpointed using the same name, which
    means that the checkpoint cannot be safely read back.
    """
    fmt = "duplicate event checkpoint names: %s"
    def __init__(self, site, objlist):
        DMLWarning.__init__(self, site,
                            ", ".join(dollar(self.site) + o.logname()
                                      for o in objlist))

class SIZEOFTYPE(DMLWarning):
    """
    The 'sizeof' operator is used on a type name, but expects an
    expression. Use the 'sizeoftype' operator for types.
    """
    fmt = "sizeof on a type is not legal, use sizeoftype instead"

class DEPRECATED(DMLWarning):
    """
    This part of the language is deprecated, usually because the
    underlying support in Simics is deprecated.
    """
    fmt = "deprecation: %s"

class EXPERIMENTAL(DMLWarning):
    """
    This part of the language is experimental, and not yet officially
    supported. Code relying on the feature may break without notice in
    future releases.
    """
    fmt = "Use of unsupported feature: %s"
    def preprocess(self):
        return super(EXPERIMENTAL, self).preprocess()

class EXPERIMENTAL_UNMAPPED(EXPERIMENTAL):
    __doc__ = EXPERIMENTAL.__doc__

class CONFIDENTIAL(DMLWarning):
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

class OLDAST(DMLWarning):
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

class WRNSTMT(DMLWarning):
    """
    The source code contained a statement "`warning;`", which
    causes a warning to be printed.
    """
    fmt = "%s"

    # This message should be removed, SIMICS-9886

class REF(DMLWarning):
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

class TEMPLATEIS(DMLWarning):
    """In a template with methods marked `shared`, it is recommended that
    other templates are instantiated on the same line"""
    fmt = ("prefer 'is' statement outside template braces,"
           + " 'template ... is (x, y) {'")

class NOIS(DMLWarning):
    """Many standard method overrides will only be recognized if a
    template named like the method is also instantiated. For instance,
    the method `set` in a field has no effect unless the
    `set` template is instantiated.
    """
    def __init__(self, site, name):
        DMLWarning.__init__(self, site, name, name)
    fmt = ("implementation of %s() without 'is %s' is ignored"
           + " by the standard library")

class THROWS_DML12(DMLWarning):
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

class NEGCONSTCOMP(DMLWarning):
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
        DMLWarning.log(self)
        self.print_site_message(
            self.expr.site, "Consider 'cast(%s, %s)'" % (self.expr, self.ty))

class ASTRUNC(DMLWarning):
    """The source of an assignment is a constant value that can't fit in the
    type of the target, and is thus truncated. This warning can be silenced by
    explicitly casting the expression to the target type.
    """
    fmt = ("The assignment source is a constant value which does not fit "
           + "the assign target of type '%s', and will thus be truncated")

class REDUNDANTLEVEL(DMLWarning):
    """`X then Y` log level syntax has no effect when the
    first and subsequent levels are the same.
    """
    def __init__(self, site):
        DMLWarning.__init__(self, site)
    fmt = ("'X then Y' log level has no effect when the levels are the same")

class TTYPEC(DMLWarning):
    """
    The delay value provided to an `after` call is subject to
    implicit type conversion which may be unexpected for certain types.
    To silence this warning, explicitly cast the delay value to the expected
    type.
    """
    fmt = ("the time value of type '%s' is implicitly converted "
           + "to the type '%s' expected by the specified time unit '%s'.")

class PCAST(DMLWarning):
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

class LOGMIXUP(DMLWarning):
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

class IMMAFTER(DMLWarning):
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

class HOOKSEND(DMLWarning):
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

class STRAYIS(DMLWarning):
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


all_warnings = dict(sorted(
    (o.tag(), o) for o in globals().values()
    if isinstance(o, type)
    and issubclass(o, DMLWarning)
    and o is not DMLWarning))
