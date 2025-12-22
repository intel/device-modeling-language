# © 2025 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import abc
from dataclasses import dataclass
from importlib import resources
import yaml

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

apis_by_ordinal = {api.ordinal: api for api in apis.values()}

class BreakingChange:
    enabled_breaking_changes = None

    def __init__(self, tag, opt_in, doc, short):
        self.tag = tag
        self.opt_in = sorted(apis_by_ordinal[i] for i in opt_in)
        # version range is contiguous
        assert opt_in[-1] - opt_in[0] == len(self.opt_in) - 1
        self.__doc__ = doc
        self.short = short

    def ident(self) -> str:
        return self.tag.replace('-', '_')

    @property
    def enabled(self):
        # `None` happens in unit tests; assume everything is enabled
        return (BreakingChange.enabled_breaking_changes is None
                or self in BreakingChange.enabled_breaking_changes)

    @property
    @abc.abstractmethod
    def required_after(self) -> API:
        return self.opt_in[-1]

# tag -> change
changes: dict[str, BreakingChange] = {
    name: BreakingChange(
        name, **{key.replace('-', '_'): value for (key, value) in obj.items()})
    for (name, obj) in yaml.safe_load(
            resources.read_binary(__package__, 'breaking-changes.yaml')).items()
}

for _change in changes.values():
    globals()[_change.ident()] = _change
del _change
