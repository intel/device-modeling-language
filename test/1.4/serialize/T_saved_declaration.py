# © 2021-2023 Intel Corporation
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
# uint8 arrays are serialized as data instead of list, and can be
# deserialized through either
obj.saved_byte_array_via_data = (3, 2, 1, 0)
stest.expect_equal(obj.saved_byte_array_via_data, (3, 2, 1, 0))
obj.saved_byte_array_via_list = [3, 2, 1, 0]
stest.expect_equal(obj.saved_byte_array_via_list, (3, 2, 1, 0))
# int8 arrays are serialized as list, but can be deserialized
# either through data or list
obj.saved_signed_byte_array_via_data = tuple(x & 0xff for x in [1, 0, -1, -2])
stest.expect_equal(obj.saved_signed_byte_array_via_data, [1, 0, -1, -2])
obj.saved_signed_byte_array_via_list = [1, 0, -1, -2]
stest.expect_equal(obj.saved_signed_byte_array_via_list, [1, 0, -1, -2])

obj.g.s.port.p.saved_deep = 4;
stest.expect_equal(obj.g.s.port.p.saved_deep, 4)

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
obj.saved_nested_array = matrix
stest.expect_equal(obj.saved_nested_array, matrix)

# Only the final dimension of a nested uint8 array is serialized as data,
# and may be deserialized as data.
byte_matrix = [tuple(byte & 0xff for byte in row) for row in matrix]
obj.saved_nested_byte_array_via_data = byte_matrix
stest.expect_equal(obj.saved_nested_byte_array_via_data, byte_matrix)
obj.saved_nested_byte_array_via_list = matrix
stest.expect_equal(obj.saved_nested_byte_array_via_list, byte_matrix)

# Only the final dimension of a nested int8 array may be deserialized as data.
obj.saved_nested_signed_byte_array_via_data = byte_matrix
stest.expect_equal(obj.saved_nested_signed_byte_array_via_data, matrix)
obj.saved_nested_signed_byte_array_via_list = matrix
stest.expect_equal(obj.saved_nested_signed_byte_array_via_list, matrix)

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

g1_id = ['g1', []]
obj.saved_identity_g1 = g1_id
stest.expect_equal(obj.saved_identity_g1, g1_id)

g2_2_id = ['g2[%u]', [2]]
obj.saved_identity_g2_2 = g2_2_id
stest.expect_equal(obj.saved_identity_g2_2, g2_2_id)

g2_0_child_id = ['g2[%u].child', [0]]
obj.saved_identity_g2_0_child = g2_0_child_id
stest.expect_equal(obj.saved_identity_g2_0_child, g2_0_child_id)

g2_1_children_2_4_id = ['g2[%u].children[%u][%u]', [1, 2, 4]]
obj.saved_identity_g2_1_children_2_4 = g2_1_children_2_4_id
stest.expect_equal(obj.saved_identity_g2_1_children_2_4, g2_1_children_2_4_id)

stest.expect_equal(obj.saved_identity_device, ['dev', []])
stest.expect_equal(obj.saved_identity_test, ['test', []])

obj.saved_identity_device = ['dev', []]
obj.saved_identity_test = ['test', []]

obj.saved_traitref_g1 = g1_id
stest.expect_equal(obj.saved_traitref_g1, g1_id)

obj.saved_traitref_g2_2 = g2_2_id
stest.expect_equal(obj.saved_traitref_g2_2, g2_2_id)

obj.saved_traitref_g2_0_child = g2_0_child_id
stest.expect_equal(obj.saved_traitref_g2_0_child, g2_0_child_id)

obj.saved_traitref_g2_1_children_2_4 = g2_1_children_2_4_id
stest.expect_equal(obj.saved_traitref_g2_1_children_2_4, g2_1_children_2_4_id)

stest.expect_equal(obj.saved_traitref_invalid, ['', []])
obj.saved_traitref_invalid = ['', []]
stest.expect_equal(obj.saved_traitref_invalid, ['', []])

# Test set values within dml
obj.test_later = None
