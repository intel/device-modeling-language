# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

from dev_util import Register_LE
from stest import expect_true, expect_log_mgr

called = {}

def create_cb(register):
    called[register] = False
    def cb(connection, access, handle, user_data):
        called[register] = True
        access.set_missed(handle, True)
        with expect_log_mgr(register.obj, 'error'):
            access.value(handle)
        access.set_missed(handle, False)
    return cb

def expect_callback(obj, provider, register):
    handle = provider.register_after_read(None, 0, 4, create_cb(register), None)

    register.read()
    expect_true(called[register])

    provider.remove_callback(handle)

def test(obj, provider1, provider2):
    expect_callback(obj, provider1, Register_LE(obj.bank.ba[0], 0x0, 4))
    expect_callback(obj, provider2, Register_LE(obj.bank.ba[1], 0x0, 4))
