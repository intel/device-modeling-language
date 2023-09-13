# © 2023 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import abc

from . import env


class DeprecatedFeature(abc.ABC):
    def tag(self):
        return self.__class__.__name__

    @abc.abstractproperty
    def __doc__(self): pass

    @abc.abstractproperty
    def short(self): pass

    @abc.abstractproperty
    def last_api_version(self): pass


# API version -> tag -> deprecation
deprecations: dict[str, dict[str, DeprecatedFeature]] = {
    api: {} for api in env.api_versions()}


def deprecation(cls: type[DeprecatedFeature]):
    assert issubclass(cls, DeprecatedFeature)
    singleton = cls()
    deprecations[cls.last_api_version][singleton.tag()] = singleton
    return singleton


@deprecation
class port_proxy_ifaces(DeprecatedFeature):
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
    last_api_version = "6"
