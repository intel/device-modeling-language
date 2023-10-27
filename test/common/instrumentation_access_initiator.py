# © 2023 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import conf
import dev_util
import stest

def test(obj, provider):
    calls = []
    handles = [
        provider.register_before_read(
            None, 0, 4,
            lambda _, access, handle, _2: calls.append(
                ('br', access.initiator(handle))),
            None),
        provider.register_after_read(
            None, 0, 4,
            lambda _, access, handle, _2: calls.append(
            ('ar', access.initiator(handle))),
            None),
        provider.register_before_write(
            None, 0, 4,
            lambda _, access, handle, _2: calls.append(
                ('bw', access.initiator(handle))),
            None),
        provider.register_after_write(
            None, 0, 4,
            lambda _, access, handle, _2: calls.append(
                ('aw', access.initiator(handle))),
            None)]

    for (reg, ini) in [
            (dev_util.Register_LE(obj.bank.b2, 0, 4), None),
            (dev_util.Register_LE(obj.bank.b2, 0, 4, initiator=conf.sim),
             conf.sim)]:
        stest.expect_equal(reg.read(), 4)
        stest.expect_equal(calls, [('br', ini), ('ar', ini)])
        del calls[:]
        reg.write(4)
        stest.expect_equal(calls, [('bw', ini), ('aw', ini)])
        del calls[:]

    for handle in handles:
        provider.remove_callback(handle)
