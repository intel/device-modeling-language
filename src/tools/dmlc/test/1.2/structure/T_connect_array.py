# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest
import dev_util

stest.expect_equal(obj.c, [[None, None, None], [None, None, None]])

class Signal(dev_util.Iface):
    iface = "signal"
    def __init__(self):
        self.count = 0
    def signal_raise(self, obj):
        self.count += 1

dev = dev_util.Dev([Signal])

for i in range(2):
    for j in range(3):
        obj.i = i
        obj.j = j
        obj.c = [[dev.obj if (i, j) == (ii, jj) else None
                    for jj in range(3)]
                   for ii in range(2)]
        dev.signal.count = 0
        obj.iface.signal.signal_raise()
        stest.expect_equal(dev.signal.count, 1)

obj.c = [[dev.obj]]
stest.expect_equal(obj.c, [[dev.obj, None, None], [None, None, None]])
