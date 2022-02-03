# © 2021-2022 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

from pyobj import ConfObject
from simics import SIM_create_object

class MockObject(ConfObject): pass

def mock_object():
    mock_object.num += 1
    return SIM_create_object('MockObject', 'connection%s' % mock_object.num, [])
mock_object.num = -1
