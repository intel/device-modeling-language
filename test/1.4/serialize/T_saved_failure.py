# © 2022-2023 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest
import simics

with stest.expect_exception_mgr(simics.SimExc_IllegalValue):
    obj.g_saved_objects = [[['g[%u]', [1]], ['dev', []]],
                           [['g[%u]', [0]], ['nonsensical', []]]]

# Checkpoint restoration of individual saved variables inside object arrays
# doesn't atomically fail together; instead, deserialization stops at the first
# element failing to deserialize, and the ones already deserialized get
# updated.
# This is in contrast to how deserialization of array types work, where one
# element failing to deserialize fails deserialization of the entire array.
stest.expect_equal(obj.g_saved_objects,
                   [[['g[%u]', [1]], ['dev', []]],
                    [['dev', []], ['g[%u]', [1]]]])
