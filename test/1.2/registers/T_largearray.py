# © 2021-2023 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import dev_util

a = dev_util.Register_LE(obj.bank.b,  50000)
b = dev_util.Register_LE(obj.bank.b, 90000)
c = dev_util.Register_LE(obj.bank.b, 150000)

a.read()
b.read()
c.read()
