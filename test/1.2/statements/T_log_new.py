# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import simics
conf.sim.stop_on_error = False
if not obj.runtest:
    print('test attribute returned false')
    simics.SIM_quit(1)


