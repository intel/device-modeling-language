# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest

# Test initial values
obj.test_initial = None

# Change values through attribute accessors, and check that we read back the
obj.saved_int = 4
stest.expect_equal(obj.saved_int, 4)
obj.saved_int24 = -8388608
stest.expect_equal(obj.saved_int24, -8388608)
obj.saved_bool = False
stest.expect_equal(obj.saved_bool, False)
obj.saved_float = -0.5
stest.expect_equal(obj.saved_float, -0.5)
obj.saved_array = [3, 2, 1, 0]
stest.expect_equal(obj.saved_array, [3, 2, 1, 0])
obj.saved_struct = [[0, [-1, 2], [3], [-2]], 4]
stest.expect_equal(obj.saved_struct, [[0, [-1, 2], [3], [-2]], 4])

matrix = [[1, 5, 6],
          [8, -1, 10],
          [12, 0, 14],
          [0, 4, -8]]

for (i, arr) in enumerate(matrix):
    obj.port.port_array[i].array_saved_int_v = arr

# this one doesnt change
stest.expect_equal([p.v for p in obj.port.port_array], [0, 1, 2, 3])
stest.expect_equal([p.array_saved_int_v for p in obj.port.port_array], matrix)
for (i, arr) in enumerate(matrix):
    obj.bank.b[i].r_f_v = arr
stest.expect_equal([b.r_f_v for b in obj.bank.b], matrix)

obj.saved_int24_le = 0xF00F00 #-1044736 in dml
stest.expect_equal(obj.saved_int24_le, -1044736)
obj.saved_uint48_be = 0xF00F00F00
stest.expect_equal(obj.saved_uint48_be, 0xF00F00F00)
obj.saved_bitfields = 0xF0F0FF
stest.expect_equal(obj.saved_bitfields, 0xF0F0FF)
obj.saved_layout = [-5, [0xFF0FF00F0], 0xFF00F0]
stest.expect_equal(obj.saved_layout, [-5, [0xFF0FF00F0], 0xFF00F0])
obj.port.p_saved1.v = -5
stest.expect_equal(obj.port.p_saved1.v, -5)
obj.port.p_saved2.v = 5
stest.expect_equal(obj.port.p_saved2.v, 5)

# Test set values within dml
obj.test_later = None
