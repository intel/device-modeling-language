# © 2026 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest

class LogCapture(object):
    def __init__(self, kind='error'):
        self.expected_kind = list(conf.sim.log_types).index(kind)
        self.messages = []
        self.filter = sim_commands.logger.filter(self.callback)
    def __enter__(self):
        self.filter.__enter__()
        return self
    def __exit__(self, *args):
        return self.filter.__exit__(*args)
    def callback(self, obj_, kind, msg):
        stest.expect_equal(obj_, obj.bank.b)
        stest.expect_equal(kind, self.expected_kind,
                           'unexpected log type for message %r' % (msg,))
        self.messages.append(msg)

with LogCapture() as capture, stest.allow_log_mgr(obj.bank.b, 'error'):
    obj.trigger_test = None

stest.expect_equal(capture.messages, [
    "b.set() call at 0x0 with invalid size 0",
    "b.set() call at 0x100 with invalid size 9",
    "b.get() call at 0x0 with invalid size 0",
    "b.get() call at 0x100 with invalid size 9",
])
