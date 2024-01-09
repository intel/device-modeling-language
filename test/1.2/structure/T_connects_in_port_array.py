# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest
import dev_util

for pa1 in obj.port.p:
    for pa2 in pa1:
        stest.expect_equal(pa2.c, [None, None])

class Signal(dev_util.Iface):
    iface = "signal"
    def __init__(self):
        self.count = 0
    def signal_raise(self, obj):
        self.count += 1

dev = dev_util.Dev([Signal])

for i in range(2):
    for j in range(2):
        for k in range(2):
            obj.i = i
            obj.j = j
            obj.k = k
            obj.port.p[i][j].c = [dev.obj if (k) == (kk) else None
                                  for kk in range(2)]
            obj.bank.b[i][j].c = [dev.obj if (k) == (kk) else None
                                  for kk in range(2)]
            dev.signal.count = 0
            obj.iface.signal.signal_raise()
            stest.expect_equal(dev.signal.count, 2)
            obj.port.p[i][j].c = [None, None]
            obj.bank.b[i][j].c = [None, None]
