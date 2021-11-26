# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import stest

stest.expect_equal(set(obj.log_groups), {
    'Default_Log_Group', 'Register_Read', 'Register_Write', 'abc_4711'})

stest.trap_log('info')

mask = 1 << obj.log_groups.index('abc_4711')
obj.log_group_mask = ~mask
obj.log_info = None
obj.log_group_mask = mask
with stest.expect_log_mgr(obj, 'info'):
    obj.log_info = None
