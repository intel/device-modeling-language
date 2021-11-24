# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest

arr_iface = SIM_get_port_interface(obj, "signal", "prt[0]")
arr_iface.signal_raise()
arr_iface = SIM_get_port_interface(obj, "signal", "prt[1]")
arr_iface.signal_raise()
stest.expect_equal(obj.prt_raised, [True, True, False, False])

prtarr_iface = obj.port.prtarr[0][0].iface.signal
prtarr_iface.signal_raise()

prtarr_iface = obj.port.prtarr[1][1].iface.signal
prtarr_iface.signal_raise()

expected_raise_values = [[True, False, False],
                         [False, True, False],
                         [False, False, False],
                         [False, False, False]]

for i, sublist in enumerate(expected_raise_values):
    for j, val in enumerate(sublist):
        stest.expect_equal(obj.port.prtarr[i][j].raised, val)

port = SIM_object_descendant(obj, 'port.prtarr[3][0]')
portclass = SIM_object_class(port)
stest.expect_equal(portclass.name, 'test.prtarr')

stest.expect_equal(port.raised, False)
port.iface.signal.signal_raise()
stest.expect_equal(port.raised, True)
