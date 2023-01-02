# © 2021-2023 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import instrumentation_access_inquire
import instrumentation_access_set_missed
import instrumentation_access_set_offset
import instrumentation_access_set_value
import instrumentation_access_suppress
import instrumentation_access_value_at_miss
import instrumentation_bank_array
import instrumentation_callback_args
import instrumentation_callback_inquiry
import instrumentation_callback_order
import instrumentation_connection_order
import instrumentation_disable_connection
import instrumentation_endianness
import instrumentation_endianness_overlapping
import instrumentation_instrument_all
import instrumentation_instrument_edge
import instrumentation_overlapping_order
import instrumentation_range
import instrumentation_remove_callback
import instrumentation_remove_connection_callbacks
import instrumentation_subscribe_multiple

subscribe_b1 = SIM_get_port_interface(
    obj, 'bank_instrumentation_subscribe', 'b1')
subscribe_b2 = SIM_get_port_interface(
    obj, 'bank_instrumentation_subscribe', 'b2')
order_b1 = SIM_get_port_interface(obj, 'instrumentation_order', 'b1')

subscribe_ba = [
    SIM_get_port_interface(obj, 'bank_instrumentation_subscribe', 'ba[0]'),
    SIM_get_port_interface(obj, 'bank_instrumentation_subscribe', 'ba[1]')]

# The tests don't clean up created connections, so run the connection
# order test first
instrumentation_connection_order.test(obj, subscribe_b1, order_b1)

instrumentation_access_inquire.test(obj, subscribe_b2)
instrumentation_access_set_missed.test(obj, subscribe_b1, obj.bank.b1)
instrumentation_access_set_offset.test(obj, subscribe_b1)
instrumentation_access_set_value.test(obj, subscribe_b1)
instrumentation_access_suppress.test(obj, subscribe_b1)
instrumentation_access_value_at_miss.test(obj, subscribe_b1, obj.bank.b1)
instrumentation_bank_array.test(obj, subscribe_ba[0], subscribe_ba[1])
instrumentation_callback_args.test(obj, subscribe_b1)
instrumentation_callback_inquiry.test(obj, subscribe_b1)
instrumentation_callback_order.test(obj, subscribe_b1)
instrumentation_disable_connection.test(obj, subscribe_b1)
instrumentation_endianness.test(obj.bank.be)
instrumentation_endianness.test(obj.bank.le)
instrumentation_endianness_overlapping.test(obj.bank.be, 0x010203040A0B0C0D)
instrumentation_endianness_overlapping.test(obj.bank.le, 0x0A0B0C0D01020304)
instrumentation_instrument_all.test(obj, subscribe_b1)
instrumentation_instrument_edge.test(obj, subscribe_b1, obj.bank.b1)
instrumentation_overlapping_order.test(obj, subscribe_b1)
instrumentation_range.test(obj, subscribe_b1)
instrumentation_subscribe_multiple.test(obj, subscribe_b1, subscribe_b2)
instrumentation_remove_callback.test(obj, subscribe_b1)
instrumentation_remove_connection_callbacks.test(obj, subscribe_b1)
