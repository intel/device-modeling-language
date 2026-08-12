# © 2025 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

clock = SIM_create_object('clock', 'clock', freq_mhz=1)
# implementer of direct_memory_update must have a clock
obj.ram.queue = clock
obj.trigger_test = True

b=simics.buffer_t(100)
obj.ram.ram.iface.ram.read(None, 0, b, 0)
print(bytes(b))
