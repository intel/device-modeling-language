# © 2021-2024 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import test_register_view
from stest import expect_equal, expect_true, expect_log_mgr

test_register_view.test(obj)

def test_exc_get(obj, bank):
    with expect_log_mgr(obj, 'error'):
        expect_equal(bank.get_register_value(0), 0)

def test_exc_set(obj, bank):
    with expect_log_mgr(obj, 'error'):
        bank.set_register_value(0, 0xdeadbeef)

test_exc_get(obj, obj.bank.exc.iface.register_view)
test_exc_set(obj, obj.bank.exc.iface.register_view)
