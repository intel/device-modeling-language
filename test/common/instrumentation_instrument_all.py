# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import dev_util
import stest

raised = False
def callback(connection, access, handle, user_data):
    global raised
    raised = True
    access.set_missed(handle, False)

def read_and_expect_raised(register):
    global raised
    register.read()
    stest.expect_true(raised)
    raised = False

def test(obj, provider):
    # a size and offset of 0 is interpreted as instrumenting the entire bank
    handle = provider.register_after_read(None, 0, 0, callback, None)

    maxuint = 0xffffffffffffffff
    read_and_expect_raised(dev_util.Register_LE(obj.bank.b1, 0, 4))
    read_and_expect_raised(dev_util.Register_LE(obj.bank.b1, maxuint - 7, 4))

    provider.remove_callback(handle)
