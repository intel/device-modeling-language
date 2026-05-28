# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import simics
from os.path import join, dirname
from pathlib import Path
import subprocess
from simicsutils.host import batch_suffix
import testenv
import conf
obj = testenv.instantiate()

cpu = simics.SIM_create_object("clock", "clock", [["freq_mhz", 1]])
obj.queue = cpu
obj.alttest = 1

stem = Path(__file__).stem
simics.SIM_write_configuration_to_file(stem + ".chkp", simics.Sim_Save_Nobundle)

subprocess.check_call(
    [f'{conf.sim.project}/bin/simics{batch_suffix()}'] +
    ["--batch-mode", "--quiet", "--no-copyright", "--dump-core", "--werror",
     '--project', conf.sim.project,
     "--module-path", testenv.scratchdir(),
     "-e", f'read-configuration {stem}.chkp',
     join(dirname(__file__), stem + ".cont.py")])
