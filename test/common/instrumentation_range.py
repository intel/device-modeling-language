# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import dev_util
import stest

def subscribe(provider, offset, size):
    triggered = [False,]
    def callback(connection, access, handle, user_data):
        triggered[0] = True
    provider.register_before_read(None, offset, size, callback, None)
    return triggered

def expect_callback(provider, register, offset, size):
    triggered = subscribe(provider, offset, size)
    register.read()
    stest.expect_true(triggered[0])

def test(obj, provider):
    r1 = dev_util.Register_LE(obj.bank.b1, 0, 4)
    r2 = dev_util.Register_LE(obj.bank.b1, 4, 4)
    r3 = dev_util.Register_LE(obj.bank.b1, 8, 4)

    expect_callback(provider, r1, 0, 4)
    expect_callback(provider, r2, 4, 4)
    expect_callback(provider, r3, 8, 4)

    expect_callback(provider, r1, 0, 1)  # partial instrumentation at start
    expect_callback(provider, r2, 7, 1)  # partial instrumentation at end
    expect_callback(provider, r3, 9, 1)  # partial instrumentation in middle

    # Overlapping instrumentation:
    triggered = subscribe(provider, 0, 0xff)
    def read_and_expect(register, triggered):
        triggered[0] = False
        register.read()
        stest.expect_true(triggered[0])

    read_and_expect(r1, triggered)
    read_and_expect(r2, triggered)
    read_and_expect(r3, triggered)
