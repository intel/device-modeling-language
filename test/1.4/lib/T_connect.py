# © 2021-2022 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import pyobj
import stest
import dev_util
import simics

calls = []
class signal_stub(pyobj.ConfObject): pass
signal_stub.register()

# instantiation fails if an interface is missing in the init_as_subobj
# port object
conf.sim.fail_on_warnings = False
with stest.expect_log_mgr(None, 'critical'):
    bad = simics.SIM_create_object('test', 'bad', [])
stest.expect_equal(bad.sub_renamed, [None, None])
conf.sim.fail_on_warnings = True

SIM_register_interface('signal_stub', 'signal', signal_interface_t(
    signal_raise=lambda obj: calls.append((obj, 'signal_raise'))))

obj = simics.SIM_create_object('test', 'obj', [])

stest.expect_equal(obj.sub.classname, 'index-map')
stest.expect_equal(obj.sub[1].classname, 'namespace')
stest.expect_equal(obj.sub[1].renamed.classname, 'signal_stub')
stest.expect_equal(obj.attr.sub_renamed, [obj.sub[0].renamed,
                                          obj.sub[1].renamed])

stest.expect_true(SIM_object_descendant(obj, "a2[1].bank.b2[2].c2[4].d[6]"))
stest.expect_true(SIM_object_descendant(obj, "a2[1].e.f"))
stest.expect_true(SIM_object_descendant(obj, "a2[1].port.p.q"))

# by default, init_as_subobj connects have configuration=none
stest.expect_false(SIM_class_has_attribute('test', 'noconf'))

with stest.expect_exception_mgr(SimExc_General):
    obj.validate = [conf.sim, "foo"]
with stest.expect_exception_mgr(SimExc_General):
    obj.validate = conf.sim
with stest.expect_exception_mgr(SimExc_General):
    obj.validate = [obj, "bar"]
obj.validate = [obj, "foo"]
obj.validate = obj

class only_common(pyobj.ConfObject):
    class ethernet_common(pyobj.Interface):
        def frame(self, frame, crc):
            pass
class only_cable(pyobj.ConfObject):
    class ethernet_cable(pyobj.Interface):
        def link_status(self, link_up):
            calls.append((self._up.obj, 'link_status', link_up))
class both(only_common, only_cable):
    pass
class three(both):
    class signal(pyobj.Interface):
        def signal_raise(self):
            calls.append((self._up.obj, 'signal_raise'))
        def signal_lower(self): pass
only_common.register()
only_cable.register()
both.register()
three.register()
[only_common, only_cable, both, three] = [
    SIM_create_object(name, name, []) for name in [
        'only_common', 'only_cable', 'both', 'three']]

with stest.expect_exception_mgr(SimExc_General):
    obj.ifaces = only_common
with stest.expect_exception_mgr(SimExc_General):
    obj.ifaces = only_cable

obj.ifaces = both
obj.invoke = None
stest.expect_equal(calls, [(both, 'link_status', True),
                           (obj.sub[1].renamed, 'signal_raise')])
del calls[:]

obj.ifaces = three
obj.invoke = None
stest.expect_equal(calls, [(three, 'link_status', True),
                           (three, 'signal_raise'),
                           (obj.sub[1].renamed, 'signal_raise')])

# if init_as_subobj has configuration != none, then the automatic assignment
# has lower precedence than an explicit initialization
override = simics.SIM_create_object('test', 'obj2', [['sub_renamed',
                                                      [None, None]]])
stest.expect_equal(override.sub_renamed, [None, None])
