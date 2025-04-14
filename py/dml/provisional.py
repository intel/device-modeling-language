# © 2024 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import abc
from . import logging
from . import messages

class ProvisionalFeature(abc.ABC):
    def tag(self) -> str:
        return self.__class__.__name__

    @abc.abstractproperty
    def __doc__(self): pass

    @abc.abstractproperty
    def short(self) -> str: pass

    @abc.abstractproperty
    def stable(self) -> str: pass


# tag -> feature
features: dict[str, ProvisionalFeature] = {}


def feature(cls: type[ProvisionalFeature]):
    assert issubclass(cls, ProvisionalFeature)
    singleton = cls()
    features[singleton.tag()] = singleton
    return singleton


@feature
class explicit_param_decls(ProvisionalFeature):
    '''This feature extends the DML syntax for parameter definitions to
    distinguish between an intent to declare a new parameter, and an intent to
    override an existing parameter (including when providing a definition
    for an abstract parameter). This distinction allows DML to capture
    misspelled parameter overrides as compile errors.

    The following new forms are introduced to mark the intent of
    declaring a new parameter:

    * For typed parameters, `param NAME: TYPE = value;` and `param
      NAME: TYPE default value;` are essentially shorthands for `param
      NAME: TYPE;` followed by `param NAME = value;` or `param NAME
      default value;`.

    * For untyped parameters, `param NAME := value;` and `param
      :default value;` are essentially shorthands for `param NAME;`
      followed by `param NAME = value;` or `param NAME default
      value;`.

    If one of these forms is used for overriding an existing
    parameter, then DMLC will signal an error, because the declaration
    was not intended as an override. DMLC will also signal an error if
    a plain `param NAME = value;` or `param NAME default value;`
    declaration appears that does not override a pre-existing
    parameter.

    In some rare cases, you may need to declare a parameter without
    knowing if it's an override or a new parameter. In this case, one
    can accompany a `param NAME = value;` or `param NAME default
    value;` declaration with a `param NAME;` declaration in the same
    scope/rank. This marks that the parameter assignment may be either
    an override or a new parameter, and no error will be printed.

    Enabling the `explicit_param_decls` feature in a file only affects
    the parameter definitions specified in that file.
    '''
    short = "Require := syntax for defining new params"
    stable = True

@feature
class stringify_list(ProvisionalFeature):
    '''This feature extends upon `stringify`, making it able to stringify
    compile-time lists that feature stringifiable items. As with all other
    valid uses of `stringify`, the resulting string will be considered
    constant.

    This is primarily meant as a stop-gap solution for expressing lists of
    items in documentation strings, which would not be possible otherwise as
    e.g. the `documentation` parameter requires the definition to be a constant
    string. `stringify_list` is provisional rather than built-in as it's a
    sub-par solution to the problem, while also making `stringify` more open to
    abuse.

    Don't take advantage of this feature so that you may use `stringify` as a
    means of determining whether or not something is a compile-time list, as
    that is not considered a valid, supported use-case. You are only meant
    to use `stringify` to produce user-facing strings (e.g. for documentation
    or logging), or for anonymization (the `name` param).
    '''
    short = "Extend 'stringify' to support compile-time lists"
    stable = True

def parse_provisional(
        provs: list[("Site", str)]) -> dict[ProvisionalFeature, "Site"]:
    ret = {}
    for (site, name) in provs:
        if name in features:
            ret[features[name]] = site
        else:
            logging.report(messages.ENOPROV(
                site, name, ', '.join(sorted(features))))
    return ret
