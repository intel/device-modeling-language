# © 2023 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import abc

from . import env


class CompatFeature(abc.ABC):
    def tag(self):
        return self.__class__.__name__

    @abc.abstractproperty
    def __doc__(self): pass

    @abc.abstractproperty
    def short(self): pass

    @abc.abstractproperty
    def last_api_version(self): pass


# API version -> tag -> feature
features: dict[str, dict[str, CompatFeature]] = {
    api: {} for api in env.api_versions().values()}


def feature(cls: type[CompatFeature]):
    assert issubclass(cls, CompatFeature)
    singleton = cls()
    features[cls.last_api_version][singleton.tag()] = singleton
    return singleton


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
    short = "Don't generate proxy port interfaces for banks and ports"
    last_api_version = 6


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
    short = ("Don't generate top-level proxy attributes"
             + " for attributes in banks and ports")
    last_api_version = 6


@feature
class dml12_inline(CompatFeature):
    '''When using `inline` to inline a method in a DML 1.2 device,
    constant parameters passed in typed arguments are inlined as
    constants when this feature is enabled. This can improve
    compilation time in some cases, but has some unintuitive semantic
    implications.
    '''
    short = "Don't inline method arguments with a declared type in DML 1.2"
    last_api_version = 6


# separate class only because last_api_version differs
@feature
class dml12_not(CompatFeature):
    '''DML 1.2-specific: the operand of the `!` operator is not
    type-checked; in particular, negation expressions on the form
    `!$reg`, where `reg` is a register, are permitted'''
    short = "Disallow ! operator on register references in DML 1.2"
    last_api_version = 5


@feature
class dml12_misc(CompatFeature):
    '''This compatibility feature preserves a number of minor language quirks
    that were originally in DML 1.2, but were cleaned up in
    DML 1.4. When this feature is enabled, DML 1.2 will permit the following:

    * `sizeof(typename)` (see `WSIZEOFTYPE`)

    * the `typeof` operator on an expression that isn't an lvalue

    * the `goto` statement

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
    short = "Disable various DML 1.2 quirks"
    last_api_version = 6
