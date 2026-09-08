# © 2024 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import simics
import conf
simics.SIM_read_configuration("checkpointing.chkp")

conf.obj.test_state = None
