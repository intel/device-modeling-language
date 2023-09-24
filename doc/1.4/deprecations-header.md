<!--
  © 2023 Intel Corporation
  SPDX-License-Identifier: MPL-2.0
-->

# Managing deprecated language features

As the DML language evolves, we sometimes need to change the language in
incompatible ways, which requires DML users to migrate their code. This
appendix describes the mechanisms we provide to make this migration process
smooth for users with large DML code bases.

In DML, deprecations can come in many forms. Deprecations in the form of
removed or renamed symbols in libraries are rather easy to manage, since they
give clear compile errors that often are straightforward to fix. A slightly
harder type of deprecation is when some language construct or API function
adjusts its semantics; this can make the model behave differently without
signalling error messages. A third kind of deprecation is when DML changes how
compiled models appear in Simics, typically to adjust changes in the Simics
API. Such changes add another dimension because they typically affect the
end-users of the DML models, rather than the authors of the models. Thus, as an
author of a model you may need to synchronize your migration of such features
with your end-users, to ease their transition to a new major version.

## Deprecation mechanisms

The simplest deprecation mechanism is Simics API versions: Each deprecated DML
feature is associated with a Simics API version, and each Simics version
supports a number of such API versions. Features reach end-of-life when moving
to a new Simics major version, the features belonging to a previous Simics API
version are dropped. Since Simics is currently the primary distribution channel
for DML, this deprecation scheme is used for DML features as well.

This scheme allows users with a large code base to smoothly migrate from one
Simics major version, N, to the next, N+1:
* First, while still using version N, make sure all Simics modules are updated
  to use API version N. Modules can be migrated one by one.
* Second, update the Simics version to N+1. This should normally have no
  effect on DML, but may come with other challenges.
* Third, update modules to API N+1, one by one. Simics version N+1 will always
  offers full support for API N, so there is no rush to update, but changing
  the API version early makes sure deprecated features are not introduced in
  new code.

In addition to the API version, DML offers some compiler flags for selectively
disabling deprecated features that are normally part of the used API. This has
some uses, in particular:
* During migration to a new API version, disabling one deprecated feature at a
  time can allow a smoother, more gradual, migration.
* If a legacy feature is still fully supported in the latest API version, then
  it cannot be disabled by selecting an API version, so selectively disabling
  it is the only way to turn it off. There are reasons to do this, e.g.:
  * Disabling a feature before it is deprecated guarantees that it is not
    relied upon in new code, which eases later migration.
  * Avoiding a feature that has a newer replacement makes the code base
    cleaner and more consistent.
  * Some legacy features can also bloat models, by exposing features in a
    redundant manner. This can also have a negative impact on performance.

## Controlling deprecation on the DML command-line
DMLC provides a command-line flag `--api-version` to specify the API version to
be used for a model. When building with the standard Simics build system, this
is controlled by the `SIMICS_API_VERSION` variable in `make`, or the
`SIMICS_API`/`MODULE_SIMICS_API` variable in `CMake`.

DMLC also provides the <code>--no-compat=_tag_</code> flag, which disables the
feature represented by _`tag`_. The available tags are listed in the next
section. The tag also controls the name of a global boolean parameter that the
DML program may use to check whether the feature is available. The parameter's
name is the tag name preceded by `_compat_`.

## List of deprecated features
