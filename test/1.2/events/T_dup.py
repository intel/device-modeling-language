# © 2021-2022 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import sys
from os.path import join
import subprocess

cpu = SIM_create_object("clock", "clock", [["freq_mhz", 1]])
obj.queue = cpu
obj.postall = 1

SIM_write_configuration_to_file("dup.chkp", Sim_Save_Nobundle)

subprocess.check_call(
    [sys.argv[0], "-batch-mode", "-quiet", "-no-copyright", "-core", "-werror",
     '-py3k-warnings',
     '-project', conf.sim.project,
     "-L", scratchdir,
     "-c", "dup.chkp",
     "-p", join(basedir, "T_dup.cont.py")])
