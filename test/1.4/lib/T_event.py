# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest

cpu = SIM_create_object("clock", "clock", [["freq_mhz", 1]])
obj.queue = cpu

def get_posted_attrs(obj):
    return {
        name: getattr(obj, name + "_posted")
        for name in ["simple_cycle", "p_simple_time", "int_cycle", "int_time"]}

def get_next_attrs(obj):
    return {
        name: getattr(obj, name + "_next")
        for name in ["simple_cycle", "p_simple_time", "int_cycle", "int_time"]}

stest.expect_equal(set(get_posted_attrs(obj).values()), {False})
stest.expect_true(all(time < 0 for time in get_next_attrs(obj).values()))

obj.post_all = None

stest.expect_equal(set(get_posted_attrs(obj).values()), {True})

# no duplicate events
stest.expect_equal(len([0 for [o, _, _, _] in cpu.iface.cycle.events()
                        if o is obj]), 6)

cycles = {cycle: name
          for [o, name, cycle, _] in cpu.iface.cycle.events() if o is obj}
stest.expect_equal(cycles, {
    2: 'simple_cycle', 4: 'int_cycle', 6: 'custom_cycle',
    1000000: 'p.simple_time', 2000000: 'int_time', 3000000: 'custom_time'})

# event.next() returns an accurate value
for (name, left) in get_next_attrs(obj).items():
    stest.expect_equal(
        [left if 'cycle' in name else left * 1000000],
        [c for c in cycles if cycles[c].replace('.', '_') == name])

qnames = set(cycles.values())

# test that event descriptions are fetched correctly from desc or
# qname
descs = {name: desc
         for [o, name, _, desc] in cpu.iface.cycle.events() if o is obj}
stest.expect_equal(descs, {
    'simple_cycle': 'a cycle event with no data',
    'int_cycle': 'a cycle event with integer data: 4711',
    'custom_cycle': 'a cycle event with custom data',
    'p.simple_time': 'p.simple_time',
    'int_time': 'int_time: 1685',
    'custom_time': 'custom_time'})

# event data got right
infos = {
    name: value for [o, name, value, _, _] in cpu.time_queue if o is obj}
stest.expect_equal(infos, {
    'simple_cycle': None,
    'int_cycle': 4711,
    'custom_cycle': -1,
    'p.simple_time': None,
    'int_time': 1685,
    'custom_time': 1750})

# Manipulate event data: subtract 1 where applicable
# Note: setting time_queue is not really a good idea since it discards
# events which are not checkpointed.
cpu.time_queue = [
    [o, name, value - 1 if o == obj and value is not None else value,
     slot, cycle]
    for [o, name, value, slot, cycle] in cpu.time_queue]

# destructor is run for previous values
stest.expect_equal((obj.custom_cycle_destroyed, obj.custom_time_destroyed),
                   (-1, 1750))
(obj.custom_cycle_destroyed, obj.custom_time_destroyed) = (0, 0)

# Now, next() and posted() have mismatching event data in int events,
# so they are not found
stest.expect_equal(get_posted_attrs(obj), {
    'simple_cycle': True, 'p_simple_time': True,
    'int_cycle': False, 'int_time': False})
stest.expect_equal(get_next_attrs(obj), {
    'simple_cycle': 2, 'p_simple_time': 1.0,
    'int_cycle': -1, 'int_time': -1.0})

def read_happened_attrs(obj):
    names = [name.replace('.', '_') + '_happened' for name in qnames]
    return {name: getattr(obj, name) for name in names}
now = 0
for cycle in sorted(cycles):
    left = cycle - now
    assert left > 1
    SIM_continue(left - 1)
    stest.expect_equal(set(read_happened_attrs(obj).values()), {0},
                       'cycle %d' % (cycle - 1,))
    SIM_continue(1)
    now = cycle
    happened_attrs = read_happened_attrs(obj)
    # side-effect of a single event() call
    happened = {name: happened_attrs[name] for name in happened_attrs
                if happened_attrs[name]}
    changed_attr = cycles[cycle].replace('.', '_') + '_happened'
    stest.expect_equal(
        happened, {
            changed_attr:
            # Simple events set attribute to 1, other use the posted
            # value (which was previously decremented by 1)
            1 if 'simple' in changed_attr else infos[cycles[cycle]] - 1},
        'cycle %d' % (cycle,))
    setattr(obj, changed_attr, 0)
# event info is not automatically destroyed after event() (see bug 17765)
stest.expect_equal((obj.custom_cycle_destroyed, obj.custom_time_destroyed),
                   (0, 0))

# Re-post events, to test cancellation
obj.post_all = None
stest.expect_equal(len([0 for [o, _, _, _] in cpu.iface.cycle.events()
                  if o == obj]), 6)
expected_names = set(qnames)
stest.expect_equal(
    {name for [o, name, _, _] in cpu.iface.cycle.events() if o == obj},
    expected_names)

# Remove simple events
obj.remove = 5
expected_names -= {'simple_cycle', 'p.simple_time'}
stest.expect_equal(
    {name for [o, name, _, _] in cpu.iface.cycle.events() if o == obj},
    expected_names)

# Remove int events
obj.remove = 4711
expected_names.remove('int_cycle')
stest.expect_equal(
    {name for [o, name, _, _] in cpu.iface.cycle.events() if o == obj},
    expected_names)
obj.remove = 1685
expected_names.remove('int_time')
stest.expect_equal(
    {name for [o, name, _, _] in cpu.iface.cycle.events() if o == obj},
    expected_names)

# Custom destroy callbacks are triggered by object deletion
msgs = []
def callback(o, kind, msg):
    msgs.append(msg)
with sim_commands.logger.filter(callback):
    SIM_delete_object(obj)
stest.expect_equal(len(msgs), 2)
stest.expect_equal(set(msgs), {'DESTROY CYCLE -1', 'DESTROY TIME 1750'})
