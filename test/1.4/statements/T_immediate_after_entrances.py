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

obj = simics.SIM_create_object('test', 'obj', queue=cpu)
stest.expect_equal(obj.count, 0)
SIM_process_pending_work()
stest.expect_equal(obj.count, 2)

obj.simple_attr = None
stest.expect_equal(obj.count, 2)
SIM_process_pending_work()
stest.expect_equal(obj.count, 3)

obj.iface.signal.signal_raise()
stest.expect_equal(obj.count, 3)
SIM_process_pending_work()
stest.expect_equal(obj.count, 4)

obj.recursive_entry_attr = None
stest.expect_equal(obj.count, 4)
SIM_process_pending_work()
stest.expect_equal(obj.count, 5)

SIM_notify(obj, SIM_notifier_type("static-export"))
stest.expect_equal(obj.count, 5)
SIM_process_pending_work()
stest.expect_equal(obj.count, 6)

SIM_notify(obj, SIM_notifier_type("extern-export"))
stest.expect_equal(obj.count, 6)
SIM_process_pending_work()
stest.expect_equal(obj.count, 7)

SIM_continue(99999)
stest.expect_equal(obj.count, 7)
SIM_continue(1)
stest.expect_equal(obj.count, 8)
SIM_continue(99999)
stest.expect_equal(obj.count, 8)
SIM_continue(1)
stest.expect_equal(obj.count, 9)
obj.simple_attr = None
stest.expect_equal(obj.count, 9)
# destroy() checks that the count has increased to 10
SIM_delete_object(obj)
stest.expect_equal(destroyed_map.get('dev', 0), 1)
stest.expect_equal(destroyed_map.get('event', 0), 1)
# Make sure that the delayed deallocation of immediate after state doesn't
# error or segfault
SIM_process_pending_work()
