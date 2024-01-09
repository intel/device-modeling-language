# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

from os.path import join
import subprocess
from simicsutils.host import batch_suffix

cpu = SIM_create_object("clock", "clock", [["freq_mhz", 1]])
obj.queue = cpu
obj.alttest = 1

SIM_write_configuration_to_file(testname + ".chkp", Sim_Save_Nobundle)

subprocess.check_call(
    [f'{conf.sim.project}/bin/simics{batch_suffix()}'] +
    ["--batch-mode", "--quiet", "--no-copyright", "--dump-core", "--werror",
     '--project', conf.sim.project,
     "--module-path", scratchdir,
     "-e", f'read-configuration {testname + ".chkp"}',
     join(basedir, "T_"+testname+".cont.py")])
