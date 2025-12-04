# © 2025 Intel Corporation
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


class BreakingChange(abc.ABC):
    enabled_breaking_changes = None

    def tag(self) -> str:
        return self.__class__.__name__.replace('_', '-')

    @property
    def enabled(self):
        # `None` happens in unit tests; assume everything is enabled
        return (True if BreakingChange.enabled_breaking_changes is None
                else self in BreakingChange.enabled_breaking_changes)

    @abc.abstractproperty
    def __doc__(self): pass

    @abc.abstractproperty
    def short(self) -> str: pass

    @property
    @abc.abstractmethod
    def required_after(self) -> API: pass

# tag -> change
changes: dict[str, BreakingChange] = {}


def change(cls: type[BreakingChange]):
    assert issubclass(cls, BreakingChange)
    singleton = cls()
    changes[singleton.tag()] = singleton
    return singleton


@change
class forbid_broken_unused_types(BreakingChange):
    '''
    Up to Simics 7, a bug prevented DMLC from reporting certain
    errors on unused unused `extern`-declared types. For example,
    the following would compile without errors:
    ```
    extern typedef struct {
        undefined_type_t member;
    } never_used_t;
    ```
    This change causes DMLC to report an error on such code.
    '''
    short = "Report errors in unused types"
    required_after = api_7


@change
class forbid_broken_conditional_is(BreakingChange):
    '''
    Up to Simics 7, a bug prevented DMLC from reporting an error
    when instantiating a nonexistent template within an `#if` block.
    For example, the following would compile without errors:
    ```
    #if (true) {
        group g {
            // should be an error, but silently ignored when this
            // feature is enabled
            is nonexisting_template;
        }
    }
    ```
    This change causes DMLC to report an error on such code.
    '''
    short = ("Report errors when instantiating a nonexistent template within "
             + "a '#if' block")
    required_after = api_7


@change
class remove_port_proxy_ifaces(BreakingChange):
    '''Version 5 and earlier of Simics relied on interface ports (as
    registered by the `SIM_register_port_interface` API function) for
    exposing the interfaces of ports and banks. In newer versions of
    Simics, interfaces are instead exposed on separate configuration
    objects.

    When this change is enabled, ports and banks will only expose
    interfaces through the dedicated port object. When this change is not
    yet enabled, ports and banks are also redundantly exposed through
    an old-style interface port for compatibility.

    When enabling this change, you can expect that initialization of connection
    attributes of other objects have to be updated. For instance, if your
    device has a port named `p`, then
    ```
    other_dev.target = [your_dev, "p"]
    ```
    will give an error, and can be changed into:
    ```
    other_dev.target = your_dev.port.p
    ```

    > [!NOTE]
    > Interface ports are only ever created for constructs that were permitted
    > in Simics 5. For instance, interfaces in banks inside groups
    > will not get an interface port even when this change is not enabled.
    '''
    short = "Generate proxy port interfaces for banks and ports"
    required_after = api_7

@change
class remove_port_proxy_attrs(BreakingChange):
    r'''In Simics 5, configuration attributes for `connect`,
    `attribute` and `register` objects inside banks and ports were
    registered on the device object, named like
    <code><em>bankname</em>\_<em>attrname</em></code>.

    When this change is enabled, attributes are only registered on
    the bank or port object itself. When the change is not yet enabled,
    ports and banks are also redundantly exposed through a proxy
    attribute on the device object.

    When enabling this change, you can expect that some code that accesses
    some attributes directly will have to be updated. For instance, if your device has
    a bank `regs` with a register `R`, then
    ```
    your_dev.regs_R = 4711
    ```
    will give an error, and can be changed into:
    ```
    your_dev.bank.regs.R = 4711
    ```

    > [!NOTE]
    > Proxy attributes are only ever created for constructs that were permitted
    > in Simics 5. For instance, attributes under banks inside groups
    > will not get a proxy attribute even when this change is not enabled.
    '''
    short = ("Remove top-level proxy attributes for attributes in banks and "
             + "ports")
    required_after = api_7


@change
class forbid_function_in_extern_struct(BreakingChange):
    '''
    Up to Simics 7, a bug allowed omitting the `*` in function pointer
    members of `extern typedef struct` declarations. For example, the
    following would compile without errors:
    ```
    extern typedef struct {
        void m(conf_object_t *);
    } my_interface_t;
    ```
    This change causes DMLC to report an error on such code. To fix it,
    use the standard C form:
    ```
    extern typedef struct {
        void (*m)(conf_object_t *);
    } my_interface_t;
    ```
    '''
    short = 'Report errors on non-pointer function members in extern structs'
    required_after = api_7


@change
class require_version_statement(BreakingChange):
    '''
    Up to Simics 7, the version specification statement (`dml 1.4;`) at
    the start of each file was optional, and `dml 1.3;` was permitted as
    a deprecated alias for `dml 1.4;`. When this change is enabled, the
    version statement becomes mandatory, and `dml 1.3;` is no longer
    accepted.
    '''
    short = 'Require a DML version statement at the start of each file'
    required_after = api_7


@change
class transaction_by_default(BreakingChange):
    '''
    Up to Simics 6, the top-level parameter `use_io_memory` defaulted to
    `true`, causing `bank` objects to implement `io_memory` instead of
    `transaction` by default. When this change is enabled, banks will
    implement `transaction` by default, and `use_io_memory` must be set
    explicitly to `true` if the old behavior is desired.
    '''
    short = 'Use the transaction interface by default in banks'
    required_after = api_6


@change
class port_obj_param(BreakingChange):
    '''
    Up to Simics 5, there were no dedicated port objects and no `obj`
    parameter in banks or ports, so the expression `obj` inside a `bank`
    resolved to `dev.obj`; to preserve this behaviour the `obj` parameter
    in banks and ports initially resolved to `dev.obj`.  When this change is
    enabled, the `obj` parameter of banks and ports instead resolves to the
    configuration object of the port object itself. Code that relied on the
    old behavior must be updated accordingly.
    '''
    short = "Make 'obj' of ports and banks resolve to the port object"
    required_after = api_5


@change
class shared_logs_locally(BreakingChange):
    '''
    Up to Simics 6, log statements inside shared methods always logged on
    the device object instead of the nearest enclosing configuration object.
    This was a bug that many scripts relied on. When this change is enabled,
    logs inside shared methods behave consistently with non-shared methods
    and log on the nearest enclosing configuration object.
    '''
    short = "Make logs inside shared methods log on the enclosing object"
    required_after = api_6


@change
class enable_WLOGMIXUP(BreakingChange):
    '''
    Up to Simics 6, the warning [`WLOGMIXUP`](messages.html#WLOGMIXUP)
    was suppressed by default to avoid overwhelming users, as the
    faulty pattern it reports was common.  When this change is
    enabled, `WLOGMIXUP` is reported by default. Code using the faulty
    pattern should be fixed before enabling this change.
    '''
    short = "Enable the warning 'WLOGMIXUP' by default"
    required_after = api_6


@change
class modern_attributes(BreakingChange):
    '''Up to Simics 7, attributes were registered using the legacy
    `SIM_register_typed_attribute` API, which supported the dictionary
    type ("D" in type strings). When this change is enabled,
    attributes are registered using the modern
    `SIM_register_attribute` family, and the dictionary type becomes
    unsupported. Models using dictionary attributes must migrate to
    another representation, such as a list of two-element lists. E.g.,
    a dict from integers to strings can be represented as an attribute
    of type `[[is]*]` instead of `D`. The outer list is created using
    `SIM_alloc_attr_list` instead of `SIM_alloc_attr_dict`, and items
    are added using `SIM_set_attr_list_item(&outer, i,
    SIM_make_attr_list(2, key, value))` rather than
    `SIM_attr_dict_set_item(&outer, i, key, value)`.
    '''
    short = "Use modern attribute registration (dictionary type unsupported)"
    required_after = api_7


@change
class strict_typechecking(BreakingChange):
    '''
    Up to Simics 7, DMLC's type checking was very lenient compared to GCC,
    especially for method overrides and uses of `extern` macros. When this
    change is enabled, type checking becomes more strict.

    The most common type of errors triggered by enabling this change are
    due to discrepencies between pointer
    types. In particular, implicitly discarding `const`-qualification of a
    pointer's base type will never be tolerated, and `void` pointers are only
    considered equivalent with any other pointer type in the same contexts as
    C.

    Novel type errors from uses of C macros exposed to DML through `extern`
    declarations can often be resolved by
    changing the signature of the `extern` declaration to more accurately
    reflect the macro's effective type.
    '''
    short = "Enforce strict type checking similar to C"
    required_after = api_7


@change
class range_check_method_indices(BreakingChange):
    '''
    Up to Simics 7, methods defined under object arrays did not validate that
    indices used when calling the method were within bounds. When this change is
    enabled, indices are implicitly range checked. If enabling this change causes
    crashes,
    then that **definitely signifies a bug in your model; a bug that would
    very likely result in memory corruption if the assertion were not to
    be made.**
    '''
    short = ("Assert object array indices in method calls. Migrate away from"
             " this ASAP!")
    required_after = api_7


@change
class restrict_log_levels(BreakingChange):
    '''
    Up to Simics 7, log levels for "warning", "error", and "critical" logs
    could be any integer between 1 and 5, even though it is only meaningful
    to provide 1 as the primary level and 5 as the subsequent level.
    When this change is enabled, the primary log level must be 1,
    and if the subsequent level is provided it must be 5;
    any other values will be rejected.
    '''
    short = "Restrict log levels for warning/error/critical logs to 1 and 5"
    required_after = api_7


@change
class dml12_disable_inline_constants(BreakingChange):
    '''
    When using `inline` in DML 1.2, constant arguments passed in
    typed parameters were inlined as constants, which had some
    unintuitive semantic implications. In DML 1.4, constants are only
    inlined in parameters declared using `inline` as quasi-type.  When
    this change is enabled, DML 1.2 only inlines constants in untyped
    method parameters, causing its behaviour to closer resemble DML 1.4.
    '''
    short = "Do not inline constant arguments in DML 1.2 inline methods"
    required_after = api_6


@change
class dml12_not_typecheck(BreakingChange):
    '''
    The operand of the `!` operator in DML 1.2 was not
    type-checked, allowing expressions like `!$reg` where `reg` is a register.
    In DML 1.4, the operand of `!` is type-checked, and such
    expressions are rejected. When this change is enabled, the DML 1.4 behaviour
    applies also in DML 1.2.
    '''
    short = "Type-check the operand of '!' in DML 1.2"
    required_after = api_5


@change
class dml12_remove_misc_quirks(BreakingChange):
    '''
    DML 1.2 had several quirks that inadvertedly caused it to
    permit some strange patterns. In DML 1.4 this was cleaned up and
    some patterns were disallowed. When this change is enabled, the
    DML 1.4 behaviour applies also in DML 1.2.

    Examples of forbidden patterns include:

    * `sizeof(typename)` (see `WSIZEOFTYPE`)
    * `typeof` on non-lvalue expressions
    * `select` statements over `vect` types
    * Passing a string literal to `char *` arguments (now requires `const char *`)
    * Using the character `-` in the `c_name` parameter of `interface` objects
    * Overriding interface type via the `c_name` parameter in `implement` objects
    * `loggroup` identifiers accessible under same name in generated C
    * Applying `&` to non-lvalues
    * `extern` statements without type (`extern foo;`)
    * Anonymous banks (`bank { ... }`)
    * Unused templates instantiating non-existing templates
    * Using the same symbol for both top-level object (`$` scope) and
      top-level symbol (non-`$` scope, e.g. `extern`, `constant` or `loggroup`)
    '''
    short = "Disallow legacy DML 1.2 quirks"
    required_after = api_6


@change
class dml12_remove_goto(BreakingChange):
    '''
    Up to Simics 6, the `goto` statement was allowed in DML 1.2.
    The statement is not allowed in DML 1.4. When this
    change is enabled, `goto` is disallowed also in DML 1.2.
    Most `goto`-based control
    structures can be rewritten using `throw` and `catch` blocks.
    '''
    short = "Disallow the goto statement in DML 1.2"
    required_after = api_6


@change
class dml12_modern_int(BreakingChange):
    '''
    Up to Simics 6, DML 1.2 used legacy integer semantics, translating most
    operations directly into C without compensating for DML-specifics like
    the support for odd-sized <tt>uint<i>NN</i></tt> types. This can
    sometimes have unexpected consequences.
    When this change is enabled, modern DML 1.4 integer semantics is used.

    The most immediate effect of enabling this change
    is that DMLC will report errors on statements like `assert 0;` and
    `while (1) { ... }`, which need to change into `assert false;` and
    `while (true) { ... }`, respectively. Other effects include:

    * With legacy integer semantics, integers of non-standard sizes
      are represented as a native C
      type, e.g. `uint5` is represented as `uint8`, allowing it to
      store numbers too large to fit in 5 bits. With modern integer
      semantics, arithmetic is done on 64-bit integers and bits are
      truncated if casting or storing in a smaller type.

      Old code sometimes relies on this feature by comparing variables
      of type `int1` to the value `1`. In DML 1.4, the only values of
      type `int1` are `0` and `-1`, so such code should be rewritten
      to use the `uint1` type. It can be a good idea to grep for
      `[^a-z_]int1[^0-9]` and review if `uint1` is a better choice.

    * With legacy semantics, some operations that have undefined behaviour
      in C effectively counts as undefined behaviour in DML as well,
      whereas modern semantics have well-defined semantics for
      all operations. For instance, negative
      shift or division by zero is handled with an unconditional critical
      error, and too large shift operands or signed shift overflow is handled
      by truncation.

    * With legacy integer semantics, comparison operators `<`, `<=`, `==`,
      `>=`, `>` inherit C semantics, whereas with modern integer semantics
      they are compared mathematically as integers. This sometimes makes a
      difference when comparing signed and unsigned numbers; in particular, `-1 !=
      0xffffffffffffffff` consistently with modern semantics, whereas with
      legacy semantics, they are consiered different only if
      both are constant.
    '''
    short = "Apply modern integer semantics in DML 1.2"
    required_after = api_6


@change
class vect_needs_provisional(BreakingChange):
    '''<a id="vect_needs_provisional"/>

    Up to Simics 7, the `vect` syntax was permitted without enabling the
    [`simics_util_vect` provisional feature](provisional-auto.html#simics_util_vect),
    issuing only a warning in DML 1.4.
    When this change is enabled, the `vect` syntax is forbidden unless the
    provisional feature is explicitly enabled.
    '''
    short = "Disallow vect syntax without 'provisional simics_util_vect'"
    required_after = api_7


@change
class forbid_warning_statement(BreakingChange):
    '''
    Up to Simics 7, the `_warning` statement was allowed, though rarely useful.
    When this change is enabled, `_warning` statements are disallowed.
    '''
    short = "Disallow the `_warning` statement"
    required_after = api_7



compat_features = {
    'broken_unused_types': forbid_broken_unused_types,
    'broken_conditional_is': forbid_broken_conditional_is,
    'port_proxy_ifaces': remove_port_proxy_ifaces,
    'port_proxy_attrs': remove_port_proxy_attrs,
    'function_in_extern_struct': forbid_function_in_extern_struct,
    'optional_version_statement': require_version_statement,
    'io_memory': transaction_by_default,
    'port_obj_param': port_obj_param,
    'shared_logs_on_device': shared_logs_locally,
    'suppress_WLOGMIXUP': enable_WLOGMIXUP,
    'legacy_attributes': modern_attributes,
    'lenient_typechecking': strict_typechecking,
    'no_method_index_asserts': range_check_method_indices,
    'meaningless_log_levels': restrict_log_levels,
    'dml12_inline': dml12_disable_inline_constants,
    'dml12_not': dml12_not_typecheck,
    'dml12_misc': dml12_remove_misc_quirks,
    'dml12_goto': dml12_remove_goto,
    'dml12_int': dml12_modern_int,
    'experimental_vect': vect_needs_provisional,
    'warning_statement': forbid_warning_statement,
}

# temporary hack to keep existing dml.globals.enabled_compat based checks working
for name in compat_features:
    globals()[name] = name
