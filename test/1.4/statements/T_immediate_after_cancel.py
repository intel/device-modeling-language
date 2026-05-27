# © 2024 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import simics
cpu = simics.SIM_create_object("clock", "clock", [["freq_mhz", 1]])
obj.queue = cpu

setup_ev = simics.SIM_register_event("setup_ev", None, simics.Sim_EC_Notsaved,
                       lambda _o, _d: obj.setup, None, None, None, None)
test_ev = simics.SIM_register_event("test_ev", None, simics.Sim_EC_Notsaved,
                       lambda _o, _d: obj.test, None, None, None, None)

# global context
_ = obj.setup
simics.SIM_process_pending_work()
_ = obj.test

# execution context
simics.SIM_event_post_cycle(cpu, setup_ev, obj, 0, None)
simics.SIM_continue(1)
_ = obj.test

# global context into execution context
simics.SIM_event_post_cycle(cpu, test_ev, obj, 0, None)
_ = obj.setup
simics.SIM_continue(1)
