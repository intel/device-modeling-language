<!--
  © 2024 Intel Corporation
  SPDX-License-Identifier: MPL-2.0
-->

# Managing deprecated language features

As the DML language evolves, we sometimes need to change the language in
incompatible ways, which requires DML users to migrate their code. This
appendix describes the mechanisms we provide to make this migration process
smooth for users with large DML code bases.

In DML, breaking changes can come in many forms. Breaking changes in the form of
removed or renamed symbols in libraries are rather easy to manage, since they
give clear compile errors that often are straightforward to fix. A slightly
harder type of breaking change is when some language construct or API function
adjusts its semantics; this can make the model behave differently without
signalling error messages. A third kind of change is when DML changes how
compiled models appear in Simics, typically to adjust changes in the Simics
API. Such changes add another dimension because they typically affect the
end-users of the DML models, rather than the authors of the models. Thus, as an
author of a model you may need to synchronize your migration of such features
with your end-users, to ease their transition to a new major version.

## Simics API versions

The simplest deprecation mechanism is Simics API versions: Each breaking change
is associated with a Simics API version, and each Simics version
supports a number of such API versions. When moving
to a new Simics major version, support for the oldest API version is dropped
which means the corresponding changes become mandatory.
Since Simics is currently the primary distribution channel
for DML, this scheme is used for DML features as well.

This scheme allows users with a large code base to smoothly migrate from one
Simics major version, N, to the next, N+1:
* First, while still using version N, make sure all Simics modules are updated
  to use API version N. Modules can be migrated one by one.
* Second, update the Simics version to N+1. This should normally have no
  effect on DML, but may come with other challenges.
* Third, update modules to API N+1, one by one. Simics version N+1 will always
  offer full support for API N, so there is no rush to update, but changing
  the API version early is a way to make sure deprecated features are not
  introduced in new code.

## Breaking changes

DML supports a more fine-grained mechanism for managing breaking changes, where
each individual change in a new API version can be enabled individually,
without enabling the full API version. This has the following uses:

* During migration to a new API version, enabling one breaking change at a time
  across the whole system makes it easier to analyze errors, because you only know that
  all errors come from the same change.
* When a breaking change is first introduced, it will be disabled by default in
  the latest API version, and only be enabled by default when the next API
  version is introduced.  Thus, until the next Simics major release, the
  breaking change cannot be activated without explicitly enabling it. There are
  reasons to adopt changes early, e.g.:
  * It guarantees that newly written code does not rely on the old behaviour,
    which eases later migration.
  * Avoiding a legacy construct that has a newer replacement makes the code base
    cleaner and more consistent.
  * Some breaking changes remove redundant representations from models, thereby
    reducing bloat both in terms of usability and performance.

## Controlling deprecation on the DML command-line
DMLC provides a command-line flag `--api-version` to specify the API version to
be used for a model. When building with the CMake based build system in Simics,
this is controlled by the `SIMICS_API` parameter of the `simics_add_module`
function.

DMLC also provides a flag <code>-\-breaking-change=_TAG_</code>, which enables
the breaking change represented by _`TAG`_. The valid tags are listed in the
next section.

## List of breaking changes

Each breaking change has an associated tag, which is passed to the
`--breaking-change` flag. Each tag also comes with a top-level DML parameter,
which DML code can use to check if a change is enabled or not. A section with
title `foo-bar` corresponds to the DMLC flag `--breaking-change=foo-bar`, and
when that flag is passed, the global parameter `_breaking_change_foo_bar` to
`true`.

Each breaking change is also implicitly enabled when compiling with a Simics
API version above a certain threshold. The breaking changes under the section for API
_n_ are useful when migrating to API _n+1_.
