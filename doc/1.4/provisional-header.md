<!--
  © 2024 Intel Corporation
  SPDX-License-Identifier: MPL-2.0
-->

# Provisional language features

Sometimes, we may choose to extend the DML compiler with a feature before it is
ready to be fully incorporated into the language. This can happen for different
reasons, e.g. if the design is not fully evaluated or if the feature is
backward incompatible. Currently, all provisional features are enabled on a
per-file basis, by adding <code>provisional <em>feature_name</em>,
<em>other_feature_name</em>;</code> just after the `dml 1.4;` statement.

Provisional features can come in two flavours:

* _Stable_ provisional features have a proven design and are
  expected to remain pretty stable over time. Details in semantics may
  still change between versions, but if we decide to make a
  significant incompatible change to a supported provisional, then we
  will create a second version of the provisional, under a new name,
  and keep both versions in parallel for some time. It can make sense
  to use supported provisional features in production code.

* _Unstable_ provisional features are expected to undergo significant
  incompatible changes over time, and are generally exposed to allow a
  dedicated team of modelers to evaluate an early design. It can be used
  to play around with, but should not be used in production code without
  first communicating with the DML team.
