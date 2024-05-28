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


# tag -> feature
features: dict[str, ProvisionalFeature] = {}


def feature(cls: type[ProvisionalFeature]):
    assert issubclass(cls, ProvisionalFeature)
    singleton = cls()
    features[singleton.tag()] = singleton
    return singleton


@feature
class explicit_param_decls(ProvisionalFeature):
    '''This feature extends the DML syntax for parameter declarations to
    distinguish between an intent to declare a new parameter, and an intent to
    override an existing parameter. This distinction allows DML to capture
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
    parameter declarations in that file.
    '''
    short = "Require := syntax for assigning new params"


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
