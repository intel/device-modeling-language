# © 2024 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest
import simics

class signal_stub:
    cls = simics.confclass('signal-stub')

    @cls.iface.signal.signal_raise
    def signal_raise(self):
        return obj.iface.signal.signal_raise()

cpu = simics.SIM_create_object("clock", "clock", [["freq_mhz", 1]])

destroyed_map = {}
def destroyed(name):
    destroyed_map[name] = destroyed_map.get(name, 0) + 1

obj = simics.SIM_create_object('test', 'obj', queue=cpu, post_inc_attr=None)
# objects_finalized will execute immediate afters posted in init(), post_init(),
# objects_finalized(), and attribute configuration
stest.expect_equal(obj.count, 4)

obj.count = 0
simics.SIM_process_pending_work()
stest.expect_equal(obj.count, 0)

obj.count = 0
obj.simple_attr = None
stest.expect_equal(obj.count, 0)
simics.SIM_process_pending_work()
stest.expect_equal(obj.count, 1)

obj.count = 0
obj.iface.signal.signal_raise()
stest.expect_equal(obj.count, 0)
simics.SIM_process_pending_work()
stest.expect_equal(obj.count, 1)

obj.count = 0
obj.recursive_entry_attr = None
stest.expect_equal(obj.count, 0)
simics.SIM_process_pending_work()
stest.expect_equal(obj.count, 1)

obj.count = 0
simics.SIM_notify(obj, simics.SIM_notifier_type("static-export"))
stest.expect_equal(obj.count, 0)
simics.SIM_process_pending_work()
stest.expect_equal(obj.count, 1)

obj.count = 0
simics.SIM_notify(obj, simics.SIM_notifier_type("extern-export"))
stest.expect_equal(obj.count, 0)
simics.SIM_process_pending_work()
stest.expect_equal(obj.count, 1)

obj.count = 0
simics.SIM_continue(99999)
stest.expect_equal(obj.count, 0)
simics.SIM_continue(1)
stest.expect_equal(obj.count, 1)
simics.SIM_continue(99999)
stest.expect_equal(obj.count, 1)
simics.SIM_continue(1)
stest.expect_equal(obj.count, 2)

obj.post_never_called = None
# The immediate after posted by the post_never_called write will be warned
# about and cancelled
with stest.expect_log_mgr(obj, 'warning', regex='immediate after'):
    simics.SIM_delete_object(obj)

stest.expect_equal(destroyed_map.get('dev', 0), 1)
stest.expect_equal(destroyed_map.get('event', 0), 1)

# Make sure that the delayed deallocation of immediate after state doesn't
# error or segfault
simics.SIM_process_pending_work()

# Check that the immediate afters posted during init, post_init, and attribute
# configuration are cancelled without warning when simics.SIM_add_configuration fails
# late and has to rollback object creation
simics.conf.sim.warnings_as_errors = True
with stest.expect_exception_mgr(simics.SimExc_General):
    simics.SIM_add_configuration(
        [simics.pre_conf_object('obj1', 'test', queue=cpu, post_never_called=None),
         simics.pre_conf_object('obj2', 'test', queue=cpu, error_on_post_init=True)],
    None)

stest.expect_equal(destroyed_map.get('dev', 0), 3)
stest.expect_equal(destroyed_map.get('event', 0), 5)

simics.SIM_process_pending_work()
