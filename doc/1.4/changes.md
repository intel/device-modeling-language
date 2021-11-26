<!--
  © 2021 Intel Corporation
  SPDX-License-Identifier: MPL-2.0
-->

# Changes from DML 1.2 to DML 1.4

DML 1.4 is in general very similar to DML 1.2 in syntax and semantics.
However, some key differences do exist which we will highlight here.

## Toy DML 1.2 device
```
dml 1.2;

device example;

method foo(a) -> (int b, bool c) nothrow {
    b = a;
    c = false;
    if (a == 0) {
        c = true;
    }
}

method bar(int a) {
    if (a == 0) {
        throw;
    }
}

// Bank
bank b {
    register r[20] size 4 @ 0x0000 + $i * 4 is read {
        method after_read(memop) {
            local int b;
            inline $foo(2) -> (b, $c);
        }

        field f [7:0];

        data bool c;
    }
}

template t1 {
    parameter p default 1;
}

template t2 {
    parameter p = 2;
}

attribute a is (t1, t2) {
    parameter allocate_type = "int32";

    method after_set() {
        call $bar($this);
        $this = $p;
    }
}
```

## The corresponding DML 1.4 device
```
dml 1.4;

device example;

inline method foo(inline a) -> (int, bool) /* b, c */ {
    local int b;
    local bool c;
    b = a;
    c = false;
    #if (a == 0) {
       c = true;
    }
    return (b, c);
}

method bar(int a) throws {
    if (a == 0) {
        throw;
    }
}

// Bank
bank b {
    register r[i < 20] size 4 @ 0x0000 + i * 4 is read_register {
        method read_register(uint64 enabled_bytes, void *aux) -> (uint64) {
            local uint64 value = default(enabled_bytes, aux);
            local int b;
            (b, c) = foo(2);
            return value;
        }

        field f @ [7:0];

        session bool c;
    }
}

template t1 {
    param p default 1;
}

template t2 is t1 {
    param p = 2;
}

attribute a is (int64_attr, t1, t2) {

    method set(attr_value_t value) throws {
        default(value);
        this.val = p;
    }
}
```

## Key Differences

```
dml 1.4;
```

In very DML 1.4 file, this must be the first declaration.

```
inline method foo(inline a) ...
```

The syntax for [inlining a method](language.html#inline-methods)
call has changed. It is now strictly an attribute of the method being
_called_, declaring itself to be inline and which arguments are inline.

```
... -> (int, bool) /* b, c */
```

Return values are no longer named in
[method declarations](language.html#methods-detailed), rather only their types
need to be declared. This also means they are not
inherently available as variables in the method body scope.

```
local int b;
local bool c;
```

Previously, these variables were declared as return values. They are now
declared as [locals](language.html#local-statements) instead.

```
#if (a == 0) {
```

The [`#if`](language.html#if-else-statements) syntax is useful to do
compile-time evaluation of constants, in DML 1.2 this would be done with
regular `if`.

```
return (b, c);
```

In DML 1.4 you must explicitly return the return values.

```
method bar(int a) throws
```

A method that might throw must be annotated with the
[`throws`](language.html#input-parameters-and-return-values) keyword.

```
register r[i < 20] ...
```

The syntax for [object arrays](language.html#object-declarations)
has changed. The index name can no longer be implicit, and the
range syntax is now `[index < size]`.

```
.., @ 0x0000 + i * 4 ...
```

The `$` prefix on variables has been removed. See
[Backward Incompatible changes](changes-manual.html#dollar_changes) for the
variable scope implications of this.

```
... is read_register {
   method read_register(uint64 enabled_bytes, void *aux) -> (uint64) {
            local uint64 value = default(enabled_bytes, aux);
            ...
   }
```

The API for the [builtin library](dml-builtins.html) has changed. In this case
the `after_read` method has been replaced, and instead the `read_register`
method must be overridden in order to call its `default` implementation and then
implement the after read logic.

```
field f  @ [7:0];
```

The syntax for [field declarations](language.html#field-declarations) has
changed, you must now specify an `@` before the bit range of the field.

```
session bool c;
```

Data declarations have been replaced with
[session](language.html#session-variables) declarations.

```
template t1 {
    param p default 1;
}

template t2 is t1 {
    param p = 2;
}

attribute a is (int64_attr, t1, t2) {
```

The parameter override behaviour has been made stricter. In the case where
multiple declarations of a parameter conflict DML will not pick a particular
declaration if it is the only non-default one, rather it will check the
[template hierarchy](language.html#templates) to determine which declarations
override which.

```
method set(attr_value_t value) throws {
        default(value);
```

Similar to above for registers, the `after_set` method of attributes is no
more. Instead the set method must be overridden and `default` called.

```
this.val = p;
```

Registers, fields, and attributes are no longer proper values in DML 1.4,
their session variable `.val` must be accessed, or a `get`, or `set` method
be called.
