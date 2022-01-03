# © 2021-2022 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

# Global variables

# name -> Template instance. An object declaration in a file are
# represented implicitly as a template, named @filename.dml.
templates = {}
# in 1.2, set of names of templates that are instantiated but not
# defined.
missing_templates = set()

traits = None

log_groups = []
device = None

# A string, such as "4.4" or "internal"
api_version = None

# This is a tuple (major, minor)
dml_version = None

# Array of all declared composite objects
objects = []

# 1.4 style integer operations in 1.2, --strict-dml12-int
strict_int_flag = None
def compat_dml12_int(site):
    # if site is None, guess DML 1.4
    return not strict_int_flag and site and site.dml_version() == (1, 2)

# True if compiling DML 1.2 without --strict. Set after parsing.
compat_dml12 = None

debuggable = False

illegal_attributes = {
    'access_count',
    'attributes',
    'build_id',
    'class_desc',
    'classname',
    'component',
    'component_slot',
    'iface',
    'log_buffer',
    'log_buffer_size',
    'log_group_mask',
    'log_groups',
    'log_level',
    'log_type_mask',
    'name',
    'ports',
    'object_id',
    'queue'}

build_confidentiality = 0

linemarks = False
