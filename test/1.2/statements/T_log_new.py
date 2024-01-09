# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

conf.sim.stop_on_error = False
if not obj.runtest:
    print('test attribute returned false')
    SIM_quit(1)


