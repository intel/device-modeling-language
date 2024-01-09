# © 2022 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import os
import simics
import stest


class signal_stub:
    cls = simics.confclass('signal-stub')
    cls.attr.level('i', default=0)
    cls.attr.transitions('i', default=0)

    @cls.iface.signal.signal_raise
    def signal_raise(self):
        self.transitions += 1
        self.level += 1

    @cls.iface.signal.signal_lower
    def signal_lower(self):
        self.transitions += 1
        self.level -= 1


# signal level high at init
clock = simics.pre_conf_object('clock', 'clock', freq_mhz=1)
stub = simics.pre_conf_object('stub', 'signal-stub', queue=clock)
obj = simics.pre_conf_object('test', 'test',
                             queue=clock,
                             outsig=stub,
                             outsig_signal_high=True)
simics.SIM_add_configuration([clock, stub, obj], None)
stub = simics.SIM_get_object(stub.name)
obj = simics.SIM_get_object(obj.name)
clock = simics.SIM_get_object(clock.name)
stest.expect_equal(stub.level, 1)

# loading a checkpoint doesn't call raise
cpfile = os.path.join(scratchdir, "checkpoint")
simics.SIM_write_configuration_to_file(cpfile, 0)
simics.SIM_delete_objects([clock, clock.cell, stub, obj])
simics.SIM_read_configuration(cpfile)

stub = simics.SIM_get_object('stub')
obj = simics.SIM_get_object('test')
stest.expect_equal(stub.level, 1)
stest.expect_equal(obj.outsig_signal_high, True)

# unplugging and plugging after instantiation
obj.outsig = None
stest.expect_equal(stub.level, 0)
stest.expect_equal(obj.outsig_signal_high, True)
obj.outsig = stub
stest.expect_equal(stub.level, 1)

# Reverse execution is removed in Simics 7
# TODO: re-enable test using the new snapshot primitives, SIMICS-21789
if 'rev-execution' in simics.SIM_get_all_classes():
    # reverse execution doesn't cause signal pulsing
    transitions = stub.transitions
    simics.SIM_run_command('enable-reverse-execution')
    simics.SIM_run_command('continue 1000')
    simics.SIM_run_command('reverse')
    stest.expect_equal(stub.transitions, transitions)

obj.test_outsig = False
stest.expect_equal(stub.level, 0)
stest.expect_equal(obj.outsig_signal_high, False)

simics.SIM_run_command('log-level 2')
obj.port.insig.iface.signal.signal_raise()
stest.expect_equal(obj.port.insig.signal_high, True)
with stest.expect_log_mgr(log_type="spec-viol", obj=obj.port.insig,
                          regex="already high"):
    obj.port.insig.iface.signal.signal_raise()

obj.port.insig.iface.signal.signal_lower()
stest.expect_equal(obj.port.insig.signal_high, False)
with stest.expect_log_mgr(log_type="spec-viol", obj=obj.port.insig,
                          regex="already low"):
    obj.port.insig.iface.signal.signal_lower()

obj.outsig = obj.port.insig
stest.expect_equal(obj.outsig_signal_high, False)
obj.test_outsig = True
stest.expect_equal(obj.port.insig.signal_high, True)
stest.expect_equal(obj.outsig_signal_high, True)
obj.test_outsig = True  # no error log

obj.test_outsig = False
stest.expect_equal(obj.port.insig.signal_high, False)
stest.expect_equal(obj.outsig_signal_high, False)
obj.test_outsig = False  # no error log

obj.outsig = None
obj.outsig_signal_high = True
obj.outsig = obj.port.insig  # should raise the signal
stest.expect_equal(obj.port.insig.signal_high, True)
