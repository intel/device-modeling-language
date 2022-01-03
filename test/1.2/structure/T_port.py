# © 2021-2022 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest

iface = SIM_get_port_interface(obj, "simple_interrupt", "p")
iface.interrupt(17)
stest.expect_equal(obj.p_last_irq, 17)
port = SIM_object_descendant(obj, 'port.p')
portclass = SIM_object_class(port)
stest.expect_equal(portclass.name, 'test.p')
with stest.expect_log_mgr(log_type='error'):
    with stest.expect_exception_mgr(SimExc_General):
        SIM_create_object(portclass, obj.name + '.port.q', [])
iface = SIM_get_interface(port, 'simple_interrupt')
iface.interrupt(43)
stest.expect_equal(port.last_irq, 43)
