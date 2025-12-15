# © 2024 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest

class LogCapture(object):
    def __init__(self):
        self.messages = []
        self.filter = sim_commands.logger.filter(self.callback)
    def __enter__(self):
        self.filter.__enter__()
        return self
    def __exit__(self, *args):
        return self.filter.__exit__(*args)
    def callback(self, obj_, kind, msg):
        stest.expect_equal(obj_, obj)
        self.messages.append(msg)

with stest.allow_log_mgr(obj, 'warning'),  \
     stest.allow_log_mgr(obj, 'error'),    \
     stest.allow_log_mgr(obj, 'critical'), \
     LogCapture() as capture:
    obj.good_logs = None
    stest.expect_equal(capture.messages,
                       ['error 1', 'warning 1', 'critical 1',
                        'error 1 then 1', 'warning 1 then 5'])
    capture.messages.clear()
    obj.good_logs = None
    stest.expect_equal(capture.messages,
                       ['error 1', 'warning 1', 'critical 1',
                        'error 1 then 1'])
    capture.messages.clear()

    obj.bad_logs = None
    stest.expect_equal(capture.messages,
                       ['error 2', 'warning 2', 'critical 2', 'error 1 then 2',
                        'warning 1 then 2', 'critical 2 then 3'])
    capture.messages.clear()
    obj.bad_logs = None
    stest.expect_equal(capture.messages,
                       ['error 2', 'warning 2', 'critical 2'])
