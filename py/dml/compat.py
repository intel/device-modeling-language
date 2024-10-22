# © 2024 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import abc
from dataclasses import dataclass


@dataclass(order=True, frozen=True)
class API:
    ordinal: int
    str: str


api_4_8 = API(4, "4.8")
api_5 = API(5, "5")
api_6 = API(6, "6")
api_7 = API(7, "7")


# All API versions known to the DML implementation. Note that the set
# of APIs accessible to the end-user is limited to what the associated
# Simics version supports.
apis = {api.str: api
        for api in [api_4_8, api_5, api_6, api_7]}


class CompatFeature(abc.ABC):
    def tag(self) -> str:
        return self.__class__.__name__

    @abc.abstractproperty
    def __doc__(self): pass

    @abc.abstractproperty
    def short(self) -> str: pass

    @abc.abstractproperty
    def last_api_version(self) -> API: pass


# tag -> feature
features: dict[str, CompatFeature] = {}


def feature(cls: type[CompatFeature]):
    assert issubclass(cls, CompatFeature)
    singleton = cls()
    features[singleton.tag()] = singleton
    return singleton


@feature
class broken_unused_types(CompatFeature):
    '''This compatibility feature prevents DML from
    reporting errors on unused `extern`-declared types:
    ```
    extern typedef struct {
        undefined_type_t member;
    } never_used_t;
    ```
    Up to Simics 7, a bug prevented DMLC from reporting an error; this
    feature exists to preserve that behaviour.
    '''
    short = "Allow (and ignore) errors in unused types"
    last_api_version = api_7


@feature
class broken_conditional_is(CompatFeature):
    '''This compatibility feature prevents DML from
    reporting errors when instantiating a template within an `#if` block:
    ```
    #if (true) {
        group g {
            // should be an error, but silently ignored when this
            // feature is enabled
            is nonexisting_template;
        }
    }
    ```
    Up to Simics 7, a bug prevented DMLC from reporting an error; this
    feature exists to preserve that behaviour.
    '''
    short = ("Allow (and ignore) instantiating nonexistent templates within "
             + "'#if' blocks")
    last_api_version = api_7


@feature
class port_proxy_ifaces(CompatFeature):
    '''Version 5 and earlier of Simics relied on interface ports (as
    registered by the `SIM_register_port_interface` API function) for
    exposing the interfaces of ports and banks. In newer versions of
    Simics, interfaces are instead exposed on separate configuration
    objects.  When this feature is enabled, old-style interface ports
    are created as proxies to the interfaces on the respective port
    objects. Such proxies are not created for all banks and ports;
    e.g., banks inside groups were not allowed in Simics 5, so such
    banks do not need proxies for backward compatibility.
    '''
    short = "Generate proxy port interfaces for banks and ports"
    last_api_version = api_7


@feature
class port_proxy_attrs(CompatFeature):
    r'''In Simics 5, configuration attributes for `connect`,
    `attribute` and `register` objects inside banks and ports were
    registered on the device object, named like
    <code><em>bankname</em>\_<em>attrname</em></code>. Such proxy
    attributes are only created When this feature is enabled.
    Proxy attributes are not created for all banks and ports, in the
    same manner as documented in the `port_proxy_ifaces` feature.
    '''
    short = ("Generate top-level proxy attributes for attributes in banks and "
             + "ports")
    last_api_version = api_7


@feature
class function_in_extern_struct(CompatFeature):
    '''
    This compatibility feature enables a traditionally allowed syntax for
    function pointer members of `extern typedef struct` declarations, where
    the `*` is omitted in the pointer type. When disabling this feature,
    any declarations on this form:
    ```
    extern typedef struct {
        void m(conf_object_t *);
    } my_interface_t;
    ```
    need to be changed to the standard C form:
    ```
    extern typedef struct {
        void (*m)(conf_object_t *);
    } my_interface_t;
    ```
    '''
    short = 'Allow non-pointer function members in extern structs'
    last_api_version = api_7


@feature
class optional_version_statement(CompatFeature):
    '''When this compatibility feature is enabled, the version
    specification statement (`dml 1.4;`) statement at the start of
    each file is optional (but the compiler warns if it is
    omitted). Also, `dml 1.3;` is permitted as a deprecated alias for
    `dml 1.4;`, with a warning.'''
    short = "Make the DML version statement optional"
    last_api_version = api_7


@feature
class io_memory(CompatFeature):
    '''The `transaction` interface was introduced in 6, and will
    eventually replace the `io_memory` interface. When this feature is
    enabled, the top-level parameter `use_io_memory` defaults to
    `true`, causing `bank` objects to implement `io_memory` instead of
    `transaction` by default.'''
    short = 'Use the io_memory interface by default in banks'
    last_api_version = api_6


@feature
class port_obj_param(CompatFeature):
    '''This compatibility feature changes the value of the `obj`
    parameter in `bank` and `port` objects: Before Simics 6 there were
    no dedicated port objects, so this parameter did not exist and if
    you wrote `obj` inside a bank, this would resolve to
    `dev.obj`. This feature preserves this legacy behaviour by making
    the `obj` parameter of banks and ports resolves to `dev.obj`
    rather than the port object.
    '''
    short = "Make 'obj' of ports and banks resolve to 'dev.obj'"
    last_api_version = api_5


@feature
class shared_logs_on_device(CompatFeature):
    '''This compatibility feature changes the semantics of log statements
    inside shared methods so that they always log on the device object, instead
    of the nearest enclosing configuration object like with non-shared methods.
    This behaviour was a bug present since the very introduction of shared
    methods, which has lead to plenty of script code having become reliant
    on it, especially in regards to how banks log. This feature preserves the
    bugged behaviour.'''
    short = "Make logs inside shared methods always log on the device object"
    last_api_version = api_6


@feature
class suppress_WLOGMIXUP(CompatFeature):
    '''This compatibility feature makes it so the warning `WLOGMIXUP` is
    suppressed by default. `WLOGMIXUP` warns about usages of a common faulty
    pattern which results in broken log statements &mdash; for more
    information, see the documentation of `WLOGMIXUP` in the
    [Messages](messages.html) section.

    `WLOGMIXUP` is suppressed by default below Simics API version 7 in order
    to avoid overwhelming users with warnings, as the faulty pattern that
    `WLOGMIXUP` reports is very prevalent within existing code. Addressing
    applications of the faulty pattern should be done before or as part of
    migration to Simics API version 7.

    Passing `--no-compat=suppress_WLOGMIXUP` to DMLC has almost the same effect
    as passing `--warn=WLOGMIXUP`; either will cause DMLC to report the warning
    even when the Simics API version in use is below 7. The only difference
    between these two options is that if `--no-compat=suppress_WLOGMIXUP` is
    used (and `--warn=WLOGMIXUP` is not), then `WLOGMIXUP` may still be
    explicitly suppressed via `--no-warn=WLOGMIXUP`. In contrast,
    `--warn=WLOGMIXUP` doesn't allow for `WLOGMIXUP` to be suppressed at
    all.'''
    short = "Suppress the warning 'WLOGMIXUP' by default"
    last_api_version = api_6

@feature
class legacy_attributes(CompatFeature):
    '''This compatibility feature makes DMLC register all attributes using the
    legacy `SIM_register_typed_attribute` API function instead of the modern
    `SIM_register_attribute` family of API functions.

    Disabling this feature will make the dictionary attribute type ("D" in type
    strings) to become unsupported, and any usage of it rejected by Simics.
    Thus, when migrating away from this feature, any attribute of the model
    that leverages dictionary values should be changed to leverage a different
    representation. In general, any dictionary can instead be represented by a
    list of two-element lists, e.g. <code>[[<em>X</em>,<em>Y</em>]*]</code>,
    where _X_ describes the type of keys, and _Y_ describes the type of
    values.'''
    short = ("Use legacy attribute registration, which supports use of the "
             + "dictionary type ('D')")
    last_api_version = api_7

@feature
class lenient_typechecking(CompatFeature):
    '''This compatibility feature makes DMLC's type checking very inexact and
    lenient in multiple respects when compared to GCC's type checking of the
    generated C.
    This discrepency mostly affects method overrides or uses of `extern`:d C
    macros, because in those scenarios DMLC can become wholly responsible for
    proper type checking.

    While migrating away from this feature, the most common type errors that
    its disablement introduces are due to discrepencies between pointer
    types. In particular, implicitly discarding `const`-qualification of a
    pointer's base type will never be tolerated, and `void` pointers are only
    considered equivalent with any other pointer type in the same contexts as
    C.

    Novel type errors from uses of `extern`:d macros can often be resolved by
    changing the signature of the `extern` declaration to more accurately
    reflect the macro's effective type.
    '''
    short = "Make type checking inexact and lenient"
    last_api_version = api_7

@feature
class no_method_index_asserts(CompatFeature):
    '''This compatibility feature makes it so that methods defined under
    object arrays don't implicitly assert that the indices used to reference
    the object array when calling the method are in bounds.

    Migrating away from this compatibility feature should be a priority. If
    its disablement makes the simulation crash due to an assertion failing,
    then that **definitely signifies a bug in your model; a bug that would
    very likely result in memory corruption if the assertion were not to
    be made.**'''
    short = ("Disable assertions made by methods of object arrays that the "
             + "the object array indices are valid. Migrate away from this "
             + "ASAP!")
    last_api_version = api_7

@feature
class meaningless_log_levels(CompatFeature):
    '''The log level that may be specified for logs of kind "warning", "error"
    or "critical" typically must be 1, and any subsequent log level must
    typically be 5. This compatibility feature makes it so either log level may
    be any integer between 1 and 4 for these log kinds. The primary log level
    is always treated as 1, and any other value than 1 for the subsequent log
    level will be treated as 5 (that is, the log will only happen once)'''
    short = ("Allow other log levels than '1' and '1 then 5' for log kinds "
             + "'warning', 'error', and 'critical'")
    last_api_version = api_7

@feature
class dml12_inline(CompatFeature):
    '''When using `inline` to inline a method in a DML 1.2 device,
    constant arguments passed in typed parameters are inlined as
    constants when this feature is enabled. This can improve
    compilation time in some cases, but has some unintuitive semantic
    implications.
    '''
    short = ("Make inline method calls in DML 1.2 inline every constant "
             + "argument, even when typed")
    last_api_version = api_6


# separate class only because last_api_version differs
@feature
class dml12_not(CompatFeature):
    '''DML 1.2-specific: the operand of the `!` operator is not
    type-checked; in particular, negation expressions on the form
    `!$reg`, where `reg` is a register, are permitted'''
    short = "Allow ! operator on register references in DML 1.2"
    last_api_version = api_5


@feature
class dml12_misc(CompatFeature):
    '''This compatibility feature preserves a number of minor language quirks
    that were originally in DML 1.2, but were cleaned up in
    DML 1.4. When this feature is enabled, DML 1.2 will permit the following:

    * `sizeof(typename)` (see `WSIZEOFTYPE`)

    * the `typeof` operator on an expression that isn't an lvalue

    * `select` statements over `vect` types

    * Passing a string literal in a (non-`const`) `char *` method argument

    * Using the character `-` in the `c_name` parameter of `interface` objects

    * Using the `c_name` parameter to override interface type in
      `implement` objects

    * `loggroup` identifiers are accessible under the same name in
      generated C code

    * Applying the `&` operator on something that isn't an lvalue
      (typically gives broken C code)

    * `extern` statements that do not specify a type (`extern foo;`)

    * Anonymous banks (`bank { ... }`)

    * Unused templates may instantiate non-existing templates

    * The same symbol may be used both for a top-level object (`$`
      scope) and a top-level symbol (non-`$` scope, e.g. `extern`,
      `constant` or `loggroup`)

    '''
    short = "Enable various legacy DML 1.2 quirks"
    last_api_version = api_6


@feature
class dml12_goto(CompatFeature):
    '''The `goto` statement is deprecated; this compatibility feature
    preserves it. Most `goto` based control structures can be reworked by
    changing the `goto` into a `throw`, and its label into a `catch`
    block; since this is sometimes nontrivial, it can be useful to disable
    the `goto` statement separately.
    '''
    short = "Enable the goto statement in DML 1.2"
    last_api_version = api_6


@feature
class dml12_int(CompatFeature):
    '''This compatibility feature affects many semantic details of
    integer arithmetic. When this feature is enabled, DMLC translates
    most integer operations directly into C, without compensating for
    DML-specifics, like the support for odd-sized
    <tt>uint<i>NN</i></tt> types; this can sometimes have unexpected
    consequences. The most immediate effect of disabling this feature
    is that DMLC will report errors on statements like `assert 0;` and
    `while (1) { ... }`, which need to change into `assert false;` and
    `while (true) { ... }`, respectively. Other effects include:

    * Integers of non-standard sizes are represented as a native C
      type, e.g. `uint5` is represented as `uint8`, allowing it to
      store numbers too large to fit in 5 bits. With modern DML
      semantics, arithmetic is done on 64-bit integers and bits are
      truncated if casting or storing in a smaller type.

      Old code sometimes relies on this feature by comparing variables
      of type `int1` to the value `1`. In DML 1.4, the only values of
      type `int1` are `0` and `-1`, so such code should be rewritten
      to use the `uint1` type. It can be a good idea to grep for
      `[^a-z_]int1[^0-9]` and review if `uint1` is a better choice.

    * Some operations have undefined behaviour in C, which is
      inherited by traditional DML 1.2. In modern DML this is
      well-defined, e.g., an unconditional critical error on negative
      shift or division by zero, and truncation on too large shift
      operands or signed shift overflow.

    * Comparison operators `<`, `<=`, `==`, `>=`, `>` inherit C
      semantics in traditional DML, whereas in modern DML they are
      compared as integers. This sometimes makes a difference when
      comparing signed and unsigned numbers; in particular, `-1 !=
      0xffffffffffffffff` consistently in modern DML, whereas with
      compatibility semantics, they are consiered different only if
      both are constant.

    The `dml12_int` feature only applies to DML 1.2 files; if a DML
    1.4 file is imported from a DML 1.2 file, then modern DML
    semantics is still used for operations in that file.
    '''
    short = "Use legacy integer semantics in DML 1.2"
    last_api_version = api_6


@feature
class experimental_vect(CompatFeature):
    '''<a id="experimental_vect"/> This compat feature
    controls how DMLC reacts to uses of the `vect` syntax in files
    where the [`simics_util_vect` provisional feature](provisional-auto.html#simics_util_vect)
    is not enabled.

    When the `experimental_vect` compatibility feature is
    enabled, such uses are permitted, and give a `WEXPERIMENTAL`
    warning in DML 1.4 (but no warning in DML 1.2). When
    `experimental_vect` is disabled, DMLC forbids the `vect`
    syntax.

    '''
    short = "Permit vect syntax without 'provisional simics_util_vect'"
    last_api_version = api_7


@feature
class warning_statement(CompatFeature):
    '''This compatibility feature enables the `_warning` statement.
    This turned out to not be very useful, so in Simics API 8 and
    newer the feature is no longer allowed.'''
    short = "Allow the `_warning` statement"
    last_api_version = api_7
