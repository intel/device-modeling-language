# © 2023 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

from os.path import join
import subprocess
from simicsutils.host import batch_suffix
import stest

cpu = SIM_create_object("clock", "clock", [["freq_mhz", 1]])
obj.queue = cpu

obj.trigger_constig = None
SIM_continue(99999)
stest.expect_equal(obj.constig_res, [0, 0])
SIM_continue(2)
stest.expect_equal(obj.constig_res, [4, 7 << 32 | 11])

obj.single_operator = 0
obj.multi_operator = [[0, 0], [0, 0]]

obj.trigger = [[1, 1], [1, 1]]
obj.trigger_hook = [1, 1]
obj.operate = None

SIM_write_configuration_to_file("checkpointing.chkp", Sim_Save_Nobundle)

subprocess.check_call(
    [f'{conf.sim.project}/bin/simics{batch_suffix()}'] +
    ["--batch-mode", "--quiet", "--no-copyright", "--dump-core", "--werror",
     '--project', conf.sim.project,
     "-L", scratchdir,
     "-c", "checkpointing.chkp",
     "-p", join(basedir, "T_after.cont.py")])
