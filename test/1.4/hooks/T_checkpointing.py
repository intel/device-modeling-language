# © 2024 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import simics
from os.path import join
import subprocess
from simicsutils.host import batch_suffix

obj.setup_state = None

simics.SIM_write_configuration_to_file("checkpointing.chkp", simics.Sim_Save_Nobundle)

subprocess.check_call(
    [f'{conf.sim.project}/bin/simics{batch_suffix()}'] +
    ["--batch-mode", "--quiet", "--no-copyright", "--dump-core", "--werror",
     '--project', conf.sim.project,
     "-L", scratchdir,
     join(basedir, "T_checkpointing.cont.py")])
