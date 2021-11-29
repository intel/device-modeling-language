# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest
stest.expect_equal(obj.on_raise_called, False)
stest.expect_equal(obj.on_lower_called, False)
stest.expect_equal(obj.port.both.signal_high, False)

obj.port.both.iface.signal.signal_raise()
stest.expect_equal(obj.port.both.signal_high, True)
stest.expect_equal(obj.on_raise_called, True)
stest.expect_equal(obj.on_lower_called, False)

obj.on_raise_called = False
obj.on_lower_called = False

with stest.expect_log_mgr(obj=obj.port.both, regex="already high"):
    obj.port.both.iface.signal.signal_raise()
stest.expect_equal(obj.port.both.signal_high, True)
stest.expect_equal(obj.on_raise_called, True)

obj.port.both.iface.signal.signal_lower()
stest.expect_equal(obj.port.both.signal_high, False)
stest.expect_equal(obj.on_lower_called, True)

obj.on_raise_called = False
obj.on_lower_called = False

with stest.expect_log_mgr(obj=obj.port.both, regex="already low"):
    obj.port.both.iface.signal.signal_lower()
stest.expect_equal(obj.port.both.signal_high, False)
stest.expect_equal(obj.on_lower_called, True)

obj.on_raise_called = False
obj.on_lower_called = False

obj.port.up.iface.signal.signal_raise()
stest.expect_equal(obj.on_raise_called, True)
stest.expect_equal(obj.on_lower_called, False)

obj.on_raise_called = False
obj.on_lower_called = False

obj.port.up.iface.signal.signal_lower()
stest.expect_equal(obj.on_raise_called, False)
stest.expect_equal(obj.on_lower_called, False)


obj.port.down.iface.signal.signal_raise()
stest.expect_equal(obj.on_raise_called, False)
stest.expect_equal(obj.on_lower_called, False)

obj.on_raise_called = False
obj.on_lower_called = False

obj.port.down.iface.signal.signal_lower()
stest.expect_equal(obj.on_raise_called, False)
stest.expect_equal(obj.on_lower_called, True)
