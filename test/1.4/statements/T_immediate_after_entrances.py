# © 2024 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest
import dev_util
import simics
from simics import confclass

class signal_stub:
    cls = confclass('signal-stub')

    @cls.iface.signal.signal_raise
    def signal_raise(self):
        return obj.iface.signal.signal_raise()

cpu = SIM_create_object("clock", "clock", [["freq_mhz", 1]])

destroyed_map = {}
def destroyed(name):
    destroyed_map[name] = destroyed_map.get(name, 0) + 1

obj = simics.SIM_create_object('test', 'obj', queue=cpu, post_inc_attr=None)
# objects_finalized will execute immediate afters posted in init, post_init,
# and attribute configuration
stest.expect_equal(obj.count, 3)
SIM_process_pending_work()
stest.expect_equal(obj.count, 3)

obj.simple_attr = None
stest.expect_equal(obj.count, 3)
SIM_process_pending_work()
stest.expect_equal(obj.count, 4)

obj.iface.signal.signal_raise()
stest.expect_equal(obj.count, 4)
SIM_process_pending_work()
stest.expect_equal(obj.count, 5)

obj.recursive_entry_attr = None
stest.expect_equal(obj.count, 5)
SIM_process_pending_work()
stest.expect_equal(obj.count, 6)

SIM_notify(obj, SIM_notifier_type("static-export"))
stest.expect_equal(obj.count, 6)
SIM_process_pending_work()
stest.expect_equal(obj.count, 7)

SIM_notify(obj, SIM_notifier_type("extern-export"))
stest.expect_equal(obj.count, 7)
SIM_process_pending_work()
stest.expect_equal(obj.count, 8)

SIM_continue(99999)
stest.expect_equal(obj.count, 8)
SIM_continue(1)
stest.expect_equal(obj.count, 9)
SIM_continue(99999)
stest.expect_equal(obj.count, 9)
SIM_continue(1)
stest.expect_equal(obj.count, 10)

obj.post_never_called = None
# The immediate after posted by the post_never_called write will be warned
# about and cancelled
with stest.expect_log_mgr(obj, 'warning', regex='immediate after'):
    SIM_delete_object(obj)

stest.expect_equal(destroyed_map.get('dev', 0), 1)
stest.expect_equal(destroyed_map.get('event', 0), 1)

# Make sure that the delayed deallocation of immediate after state doesn't
# error or segfault
SIM_process_pending_work()

# Check that the immediate afters posted during init, post_init, and attribute
# configuration are cancelled without warning when SIM_add_configuration fails
# late and has to rollback object creation
conf.sim.warnings_as_errors = True
with stest.expect_exception_mgr(SimExc_General):
    simics.SIM_add_configuration(
        [pre_conf_object('obj1', 'test', queue=cpu, post_never_called=None),
         pre_conf_object('obj2', 'test', queue=cpu, error_on_post_init=True)],
    None)

stest.expect_equal(destroyed_map.get('dev', 0), 3)
stest.expect_equal(destroyed_map.get('event', 0), 5)

SIM_process_pending_work()
