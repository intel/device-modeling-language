# © 2024 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest

cpu1 = SIM_create_object("clock", "clock1", freq_mhz=1)
cpu2 = SIM_create_object("clock", "clock2", freq_mhz=1, cell=cpu1.cell)

buddy = SIM_create_object("test", "buddy")
obj.queue = cpu1
obj.buddy = buddy
buddy.queue = cpu2

SIM_run_command('enable-multicore-accelerator')
for cpu in (cpu1, cpu2):
    stest.expect_equal(cpu.multicore_accelerator_enabled, True)

for dev in (obj, buddy):
    stest.expect_equal(dev.iface.concurrency_mode.current_mode(),
                       Sim_Concurrency_Mode_Serialized_Memory)
    stest.expect_equal(
        sorted(dev.iface.concurrency_group.execution_group(0)),
        sorted([dev,
                dev.bank.b, dev.sd.bank.b, dev.sd.sd.bank.b,
                dev.port.p, dev.sd.port.p, dev.sd.sd.port.p,
                dev.sd, dev.sd.sd]))

obj.setup_main = None
buddy.setup_buddy = None

SIM_continue(1)
stest.expect_equal(obj.shared_state_attr, 3)


# # stest.expect_equal(obj.count, 0)

# obj.a = None

# # stest.expect_equal(obj.count, 1)

# local = obj.a

# # stest.expect_equal(obj.count, 2)

# obj.ev = None

# # stest.expect_equal(obj.count, 3)

# SIM_continue(100000)

# # stest.expect_equal(obj.count, 4)

# obj.iface.signal.signal_raise()

# # stest.expect_equal(obj.count, 5)

# SIM_notify(obj, SIM_notifier_type("exported-entry"))

# # stest.expect_equal(obj.count, 6)

# SIM_notify(obj, SIM_notifier_type("statically-exported-entry"))

# # stest.expect_equal(obj.count, 7)

# obj.immediate_after = None

# # stest.expect_equal(obj.count, 8)

# SIM_process_pending_work()

# # stest.expect_equal(obj.count, 9)
