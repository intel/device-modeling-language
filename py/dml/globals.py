# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

# Global variables

from . import compat

# name -> Template instance. An object declaration in a file are
# represented implicitly as a template, named @filename.dml.
templates = {}
# in 1.2, set of names of templates that are instantiated but not
# defined.
missing_templates = set()

traits = None

# A mapping from each trait to the set of objects instantiating that trait.
# {Trait: [CompositeObject]}
trait_instances = {}


log_groups = []
device = None

# A string, such as "4.4" or "internal"
api_version = None

# This is a tuple (major, minor)
dml_version = None


# Array of all session/saved variables declared in methods, represented by
# pairs (Symbol, init) where init is str or None
static_vars = []

# A reference to the 'object' trait
object_trait = None

# Set of template types which get serialized
# serialize.SerializedTraits
serialized_traits = None

# Array of all codegen.AfterOnHookInfo:s
after_on_hook_infos = []

# A mapping from any after delay key to its after delay info
# object -> codegen.AfterDelayInfo
after_delay_infos = {}

# A mapping from any immediate after key to its immediate after info
# object -> codegen.ImmediateAfterInfo
immediate_after_infos = {}

# A mapping from a unique type sequence to its TypeSequenceInfo
# types.TypeSequence -> codegen.TypeSequenceInfo
type_sequence_infos = {}

enabled_compat = set()

# 1.4 style integer operations in 1.2
def compat_dml12_int(site):
    # if site is None, guess DML 1.4
    return (compat.dml12_int in enabled_compat
            and site and site.dml_version() == (1, 2))

debuggable = False

coverity = False

coverity_pragmas = {}

# all warnings are disabled by the --dep flag
ignore_all_warnings = False

# Enable features that are not supported in any capacity, for testing purposes.
# This is reserved for features that are fully implemented, but whose design
# cannot be finalized until later in the future.
enable_testing_features = False

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

# Signifies if --nolines was passed to DMLC; if not, this will be set to True
linemarks_enabled = False

# Relevant during C emission: indicates if linemarks should currently be generated
linemarks = False
