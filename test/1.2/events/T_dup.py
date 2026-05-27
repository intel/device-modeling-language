# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import simics
from os.path import join
import subprocess
from simicsutils.host import batch_suffix

cpu = simics.SIM_create_object("clock", "clock", [["freq_mhz", 1]])
obj.queue = cpu
obj.postall = 1

simics.SIM_write_configuration_to_file("dup.chkp", simics.Sim_Save_Nobundle)

subprocess.check_call(
    [f'{conf.sim.project}/bin/simics{batch_suffix()}'] +
    ["--batch-mode", "--quiet", "--no-copyright", "--dump-core", "--werror",
     '--project', conf.sim.project,
     "--module-path", scratchdir,
     "-e", "read-configuration dup.chkp",
     join(basedir, "T_dup.cont.py")])
