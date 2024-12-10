# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import simics
import stest

obj.log_level = 1

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

with LogCapture() as capture:
    obj.test = True
    messages = capture.messages

    stest.expect_equal(len(messages), 6)
    stest.expect_true("regular once 1" in messages[0])
    stest.expect_true("exact once" in messages[1])
    stest.expect_true("regular once 2" in messages[2])
    stest.expect_true("shared method 1 once in g1" in messages[3])
    stest.expect_true("shared method 2 once in g1" in messages[4])
    stest.expect_true("shared method 1 once in g2" in messages[5])

obj.log_level = 4

with LogCapture() as capture:
    obj.test = True
    messages = capture.messages

    stest.expect_equal(len(messages), 10)
    stest.expect_true("regular once 1" in messages[0])
    stest.expect_true("regular once 1" in messages[1])
    stest.expect_true("regular once 2" in messages[2])
    stest.expect_true("regular once 2" in messages[3])
    stest.expect_true("shared method 1 once in g1" in messages[4])
    stest.expect_true("shared method 1 once in g1" in messages[5])
    stest.expect_true("shared method 2 once in g1" in messages[6])
    stest.expect_true("shared method 2 once in g1" in messages[7])
    stest.expect_true("shared method 1 once in g2" in messages[8])
    stest.expect_true("shared method 1 once in g2" in messages[9])

obj.log_level = 1
with LogCapture() as capture:
    obj.test_multi_inheritance = True
    messages = capture.messages
    stest.expect_true(len(messages), 1)
    stest.expect_true("multiply inherited" in messages[0])

with LogCapture() as capture:
    obj.test_arrays = True
    messages = capture.messages
    stest.expect_true(len(messages), 8)
    for mess in messages[0:4]:
        stest.expect_true("shared in array" in mess)
    for mess in messages[4:]:
        stest.expect_true("array method" in mess)

with LogCapture() as capture:
    with stest.expect_log_mgr(obj, 'warning'):
        obj.test_warning_once = True
    with stest.expect_log_mgr(obj, 'error'):
        obj.test_error_once = True
    with stest.expect_log_mgr(obj, 'critical'):
        obj.test_critical_once = True
    stest.expect_equal(capture.messages,
                       ['warning once', 'error once', 'critical once'])

with LogCapture() as capture:
    obj.test_warning_once = True
    obj.test_error_once = True
    obj.test_critical_once = True
    stest.expect_equal(capture.messages, [])
