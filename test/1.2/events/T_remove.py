# © 2021-2023 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest

cpu = SIM_create_object("clock", "clock", [["freq_mhz", 1]])
obj.queue = cpu

# event happens if you don't remove it. Should be caught by other test.
obj.last_ev = -1
obj.last_destroy = 0
obj.post = [5, 42]
SIM_continue(5)
assert obj.last_ev == 42
assert obj.last_destroy == 0

# event doesn't happen if you remove it.
obj.last_ev = 0
obj.last_destroy = 0
obj.post = [5, 42]
SIM_continue(4)
obj.remove = 42
stest.expect_equal(obj.last_destroy, 42)
SIM_continue(1)
stest.expect_equal(obj.last_ev, 0)

# event happens if you remove it with mismatching event data.
obj.last_ev = 0
obj.last_destroy = 0
obj.post = [5, 42]
SIM_continue(4)
obj.remove = 43
SIM_continue(1)
stest.expect_equal(obj.last_destroy, 0)
stest.expect_equal(obj.last_ev, 42)

### Array Version

def get_pattr_array(attrname):
    return [[getattr(y, attrname) for y in x] for x in obj.port.a]
def set_pattr_array(attrname, valarray):
    for i, x in enumerate(obj.port.a):
        for j, y in enumerate(x):
            setattr(y, attrname, valarray[i][j])

# event happens if you don't remove it. Should be caught by other test.

set_pattr_array('last_ev', [[-1, -1], [-1, -1]])
set_pattr_array('last_destroy', [[0, 0], [0, 0]])
set_pattr_array('post', [[[5, 42], [5, 42]], [[5, 42], [5, 42]]])
SIM_continue(5)
stest.expect_equal(get_pattr_array('last_ev'), [[42, 42], [42, 42]])
stest.expect_equal(get_pattr_array('last_destroy'), [[0, 0], [0, 0]])

# event doesn't happen if you remove it.
set_pattr_array('last_ev', [[0, 0], [0, 0]])
set_pattr_array('last_destroy', [[0, 0], [0, 0]])
set_pattr_array('post', [[[5, 42], [5, 42]], [[5, 42], [5, 42]]])
SIM_continue(4)
set_pattr_array('remove', [[42, 42], [42, 42]])
stest.expect_equal(get_pattr_array('last_destroy'), [[42, 42], [42, 42]])
SIM_continue(1)
set_pattr_array('last_ev', [[0, 0], [0, 0]])

# event happens if you remove it with mismatching event data.
set_pattr_array('last_ev', [[0, 0], [0, 0]])
set_pattr_array('last_destroy', [[0, 0], [0, 0]])
set_pattr_array('post', [[[5, 42], [5, 42]], [[5, 42], [5, 42]]])
SIM_continue(4)
set_pattr_array('remove', [[43, 43], [43, 43]])
SIM_continue(1)
stest.expect_equal(get_pattr_array('last_ev'), [[42, 42], [42, 42]])
stest.expect_equal(get_pattr_array('last_destroy'), [[0, 0], [0, 0]])

# destroy is called when posting the event fails
obj.last_destroy = 0
obj.queue = None
with stest.expect_log_mgr(obj, 'error'):
    obj.post = [5, 44]
stest.expect_equal(obj.last_destroy, 44)
