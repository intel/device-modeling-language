# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import testenv
import conf
obj = testenv.instantiate()
conf.sim.stop_on_error = False
obj.log_stuff = None
