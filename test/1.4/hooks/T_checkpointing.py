# © 2023 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

from os.path import join
import subprocess
from simicsutils.host import batch_suffix

obj.setup_state = None

SIM_write_configuration_to_file("checkpointing.chkp", Sim_Save_Nobundle)

subprocess.check_call(
    [f'{conf.sim.project}/bin/simics{batch_suffix()}'] +
    ["--batch-mode", "--quiet", "--no-copyright", "--dump-core", "--werror",
     '--project', conf.sim.project,
     "-L", scratchdir,
     "-c", "checkpointing.chkp",
     "-p", join(basedir, "T_checkpointing.cont.py")])
