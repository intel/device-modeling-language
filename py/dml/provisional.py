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
