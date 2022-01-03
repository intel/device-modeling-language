# © 2021-2022 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

conf.sim.fail_on_warnings = False
if not obj.runtest:
    print('test attribute returned false')
    SIM_quit(1)


