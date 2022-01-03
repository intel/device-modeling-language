# © 2021-2022 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import dev_util
import stest
for name in ['r1', 'r2', 'r3', 'r4']:
    setattr(obj, 'b_' + name, 4)
    # set adds 1, get adds 2
    stest.expect_equal(getattr(obj, 'b_' + name), 7)
