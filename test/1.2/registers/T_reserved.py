# © 2021-2023 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

SIM_run_command("log-level 4")
mem = SIM_create_object("memory-space", "mem",
                        [["map", [[0, [obj, "regs"], 0, 0, 0x10000000]]]])

def loghap(arg, obj, logtype, msg):
    global specviol
    if logtype == Sim_Log_Spec_Violation:
        specviol = True

SIM_hap_add_callback("Core_Log_Message", loghap, None)

def dowrite(offset, data, expect_failure):
    global specviol
    specviol = False
    exc = mem.iface.memory_space.write(None, 0, data, 0)
    if exc != Sim_PE_No_Exception:
        print("exception when writing")
        SIM_quit(1)
    if expect_failure and not specviol:
        print("no violation detected")
        SIM_quit(1)
    if not expect_failure and specviol:
        print("stray violation detected")
        SIM_quit(1)

# Should always be OK to write zero
dowrite(0, (0,0,0,0), False)
dowrite(4, (0,0,0,0), False)

# Write ones
dowrite(0, (255,255,255,255), True)
dowrite(4, (255,255,255,255), True)

# Write ones where allowed
dowrite(0, (255,255,0,255), False)
dowrite(4, (255,255,0,255), False)
