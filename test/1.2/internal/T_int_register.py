# © 2021-2024 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

RN = 17
RS = '_r0x0'                    # anonymized name

get_number = obj.ports.b.int_register.get_number
get_name = obj.ports.b.int_register.get_name

assert get_number(get_name(RN)) == RN
assert get_name(get_number(RS)) == RS
