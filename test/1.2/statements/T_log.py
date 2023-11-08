# © 2021-2023 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

conf.sim.fail_on_warnings = False
try:
    obj.log_stuff = None
except simics.CriticalErrors:
    pass
