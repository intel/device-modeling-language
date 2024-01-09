<!--
  © 2021 Intel Corporation
  SPDX-License-Identifier: MPL-2.0
-->

# Known Limitations

The following are known limitations of the `dmlc` compiler at
the time of this writing:

<dl><dt>

Conflicting C names
</dt><dd>

Because of the way that C identifiers are generated from DML,
there is a possibility that the C compiler will complain about
conflicting identifiers.  The reason is that when generating
C functions from DML methods, the fully qualified is used, with
double underscores substituted for the separating periods. This means that
the following two declarations will create a conflict:

```
bank foo {
    method export bar { ... }
}
method export foo__bar { ... }
```

  The qualified names the methods are `foo.bar` and
 `foo__bar`, respectively, and these both get mangled to the
 same C identifier `foo__bar`.

 To avoid this problem, never use double underscores in names in DML.
</dd></dl>

