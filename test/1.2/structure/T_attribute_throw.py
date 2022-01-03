# © 2021-2022 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest

try:
    obj.a = None
except SimExc_IllegalValue as e:
    pass
else:
    stest.fail('expected exception')

try:
    _ = obj.a
except SimExc_Attribute as e:
    pass
else:
    stest.fail('expected exception')

try:
    obj.b = None
except SimExc_IllegalValue as e:
    pass
else:
    stest.fail('expected exception')

try:
    _ = obj.b
except SimExc_Attribute as e:
    pass
else:
    stest.fail('expected exception')
