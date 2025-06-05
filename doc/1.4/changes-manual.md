<!--
  © 2021 Intel Corporation
  SPDX-License-Identifier: MPL-2.0
-->

# Backward incompatible changes, not automatically converted

A number of changes are not taken care of by the conversion script.
Many incompatibilities can be captured as errors already in DML 1.2,
by passing the `--strict-dml12` argument. This can be used to
split the conversion process into smaller steps: by running with
this flag *before* running `port-dml.py`, one can
apply manual fixes one by one and validate each fix in isolation;
this means that the result of `port-dml.py` will contain
fewer errors.

The following incompatible changes are caught as errors or warnings
by the `--strict-dml12` flag:

* In classic DML 1.2, an unused template is allowed to instantiate
  a non-existing template. This is not permitted in DML 1.4.

* <a id="dollar_changes"/> In DML 1.4, `$` is no longer used
  for object references, and top-level object scope is therefore merged
  with the global scope. This means that you cannot both declare a bank X
  (referenced as $X in 1.2, as X in 1.4) and a global constant X (referenced
  as X in both 1.2 and 1.4).

* When inlining a method with a constant value as an argument,
  the argument will no longer be considered constant in the method body
  if the argument was declared with a type. In classic DML 1.2, a constant
  is always inlined as a constant, whose type might not match with the
  declared type.

* Anonymous banks (syntax: `bank { ... }`) are no longer
  allowed.

* DML 1.2 permitted certain C keywords, e.g. `signed` and
  `register`, to be used as names of local variables. This is no longer
  permitted.

* Integer arithmetic works differently in DML 1.4. In short, all operands to
  arithmetic operands are promoted to 64 bits before performing the
  operation, much like 16- and 8-bit operands are promoted to 32 bits in C.

  Also, many semantic irregularities and bugs in 1.2 semantics have been
  corrected in 1.4.

* It is no longer permitted to use a register or field object as operand
  to the `!` operator. In classic DML 1.2, there was a loophole that
  permitted this.

* In DML 1.2, you can reference a method in an `interface` object
  by casting it to a function pointer type. That is no longer permitted
  in DML 1.4.

* The `goto` statement has been removed from DML 1.3. We
  plan to re-introduce a restricted variant of the goto statement
  which is only allowed to jump forward, and to an enclosing scope.

* It is no longer allowed to iterate over the undocumented `vect`
  type in a `select` statement. In classic DML 1.2, this was
  permitted but had unexpected behaviour.

* If a method argument has type `const char *`, it is no
  longer permitted to pass a string literal.

* The `!` operator is stricter in 1.3; in particular, it no longer
  accepts a constant integer as operand. Other condition expressions, like
  the `if` condition or `&&` operands, no
  longer accept the 0 or 1 literal; use `false` or `true`
  instead.

* In DML 1.4, the `sizeof` and `typeof` operators
  require that the operand is an lvalue.

* A reference to an `implement` object can no longer be used as
  a value.

* In classic DML 1.2, one could write `extern X;` to declare that
  some symbol X is externally defined as a C symbol, without providing a hint
  on the type of X. This is not permitted in DML 1.4.

* The undocumented `c_name` parameter no longer has an effect
  on `interface` objects. This may affect old C + DML integrations.

* A method's input parameters now belong to the method's top
  scope, instead of a separate scope. Thus, it is illegal to declare a
  variable in method's top scope which shadows a method
  parameter.

* In classic DML 1.2, the `loggroup X;` statement would expose the
  log group as an identifier `X` in generated C code. This is no longer
  done, which may affect old C + DML integrations.

The following incompatible changes are *not* caught
by `--strict-dml12`, and must be adjusted manually:

* The [API for initialization and reset](utility.html#templates-for-reset)
  has been rewritten. The most important differences:
  * When the device is initialized, all registers are reset to the
    new `init_val` parameter, defaulting to zero. In 1.2,
    similar results were achieved by performing a hard reset.

  * The `hard_reset` and `soft_reset` methods in
    banks and devices, typically called to trigger a reset, are no
    longer available by default. The methods can be created by
    instantiating templates from the `utility.dml`
    library (the templates are named `hreset`
    and `sreset`, respectively).

  * The `hard_reset` and `soft_reset` methods in
    registers and fields also do not exist by default. They are also
    created when instantiating `hreset` and `sreset`
    templates.

  * The `hard_reset_value` and `soft_reset_value`
    parameters in registers and fields are no longer
    recognized. Instead, the default implementations of
    the `hard_reset` and `soft_reset` methods
    invoke the `init` method, effectively replacing
    the `hard_reset_value` and `soft_reset_value`
    parameters with `init_val`. You must now override
    the `soft_reset` method explicitly in order to provide
    different reset values for hard and soft reset.

* The API for declaring event objects has changed. Just like in
  DML 1.2, the event callback is defined by a
  method `event`, and the event is posted by a
  method `post`. A number of things have changed since 1.2:

  * Instead of defining a parameter `timebase` in each
    event object, you must now instantiate one of six predefined
    templates in each event object:

    <dl><dt>

    `simple_time_event`, `simple_cycle_event`
    </dt><dd>

    Event without data. The `event` method takes no
    arguments, and the `post` method only takes a single
    argument, the delay.

    </dd><dt>

    `uint64_time_event`, `uint64_cycle_event`
    </dt><dd>

    Event with integer data. The `post` method takes
    an argument of type `uint64` in addition to the
    delay argument. This argument is later passed on to
    the `event` method, which takes a
    single `uint64` argument.

    </dd><dt>

    `custom_time_event`, `custom_cycle_event`
    </dt><dd>

    Event with custom data. The `post` method takes
    an argument of type `void *` in addition to the
    delay argument. This value is later passed on to
    the `event` method, which takes a
    single `void *` argument.

    </dd></dl>

    The difference between the `*_time_event`
    and `*_cycle_event` templates is similar to the
    difference between `timebase = "seconds"`
    and `timebase = "cycles"` in DML 1.2. I.e., when a
    method argument or return value represents time, then the
    variant of the method in a `*_time_event` template
    represents time as a floating-point number, representing
    (simulated) seconds, whereas the variant in
    a `*_cycle_event` template represents time as an
    integer, representing CPU cycles.

  * The method `event` is abstract in all templates. In
    DML 1.2, there was a not-so-useful default implementation.

  * The methods `next`, `posted`
    and `remove` are not available in
    the `custom_*_event` templates. They are available in
    the `simple_*_event` templates without `data`
    arguments, and in `uint64_*_event` templates
    with `data` arguments of type `uint64`.

  * The
    methods `set_event_info`, `get_event_info`
    and `destroy` are only recognized by
    the `custom_*_event` templates, where the methods are
    abstract. In the other templates, serialization and
    deserialization are done automatically

  * The `describe_event` method can no longer be overridden.

  * The `post_on_queue` method is no longer available.

  Example:
  <pre>
  event simple is (simple_time_event) {
    method event() {
      log info: "hello";
    }
  }
  event parameterized is (uint64_cycle_event) {
    method event(uint64 data) {
      log info: "hello %d", data;
    }
  }
  ...
  // no data args needed
  simple.post(1.0);
  assert simple.posted();
  // in uint64_cycle_event, the methods <fun>posted</fun>, <fun>next</fun>
  // and <fun>remove</fun> check that the data argument matches
  parameterized.post(18, 4711);
  assert parameterized.next(4711) == 18;
  assert parameterized.next(42) &lt; 0;
  </pre>

* The parameters `partial` and `overlapping` in banks
  are now `true` by default.

* Some rarely used identifiers from C, in particular many C macros
  on the form `XYZ_INTERFACE`, are no longer automatically
  available in DML. This will cause compile errors on migration, which
  can be resolved by manually adding corresponding symbol definitions
  to the DML file.

* Methods that can throw an exception must be annotated with the
  new `throws` keyword:
  ```
  method m(bool condition) -> (int) throws {
      if (condition)
          throw;
  }
  ```

  The `throws` annotation is required if the method contains
  either a throw statement or an invocation of a method with
  the `throws` annotation, unless all such statements are
  enclosed in `try` blocks.

* Assignment operators (`=` as well
  as `+=`, `*=`, etc) are now separate statements, and
  disallowed inside expressions. These statements are allowed in some
  additional places, to replace common uses of assignment
  expressions:
  * A `=` statement may contain multiple target
    expressions (`a = b = c;`). The targets are assigned
    from right to left, like in C.

  * The pre and post sections of a `for` statement may
    contain any assignment statement. So the following is still valid:

    ```
    for (i = j = 0; i < 10; i += 3, j++) { ... }
    ```

* In DML 1.4, methods are no longer declared `extern`, rather
  they are `exported` using the `export` statement.
  An `exported` method must be declared
  in the top scope, may not throw exceptions, and may have at most one
  return value. The generated C function now has extern linkage, and
  takes a `conf_object_t *` as its first argument.

* The syntax of `switch` statement is now stricter, and now
  requires a conventional switch structure: The switch body must be a
  compound statement starting with a `case` label,
  all `case` and `default` statements must reside
  directly in this statement, and no `case` may appear after
  the `default` statement.

* When declaring a template, a `desc` string may no
  longer be supplied. Thus, the following is now invalid:

  ```
  template t "short description" { ... }
  ```

  This syntax used to create a parameter `desc` in the
  objects where the template is instantiated.

* The `#` operator is no longer supported. It was seldom
  useful in DML 1.2, and its behaviour was in many ways complex and
  undocumented.

* It is no longer allowed to refer to objects using the
  names `signed`, `unsigned` and `this`.

* The expression `undefined` may no longer be passed as argument to
  an inline method

* If the condition of a `while` loop is statically
  evaluated to false, then faulty code in the loop body can cause compile
  errors in DML 1.4. In DML 1.2, the loop body was silently discarded.

* `goto` statements, as well as goto labels, are not allowed in
  DML 1.4.

* The parameter `persistent` in fields is no longer
  supported. It was buggy in 1.2. As a replacement, checkpoint
  fields as separate attributes if persistence differs within a
  register.

* Objects of `register` or `attribute` type with
  `configuration="pseudo"`, that are either read-only or
  write-only, have to declare this specifically by setting the new
  parameter `readable` (or `writable`)
  to `false`. In 1.2, DMLC used some heuristics to
  automatically guess the user's intent.

* The parameter `allocate_type` in `attribute`
  objects has been removed. Three templates are available as a replacement:

  * `uint64_attr` replaces `allocate_type="uint64"`

  * `int64_attr` replaces `allocate_type="int64"`

  * `bool_attr` replaces `allocate_type="bool"`

  * `double_attr` replaces `allocate_type="double"`

  For the `allocate_type`
  values `"uint8"`, `"uint16"`, `"uint32"`,
  `"int8"`, `"int16"`, and `"int32"`, the
  corresponding 64-bit template is recommended. There is no
  replacement for `allocate_type="string"`.

* In objects of type `attribute`, `register`
  and `field`, storage is now allocated in a separate
  member `val` instead of the object itself. For example:
  when assigning a value to a register, you might change
  `bank.reg = 4;` into `bank.reg.val = 4;`. Note that
  the `val` member is not automatically created by DMLC; it
  is merely a naming convention used by many standard templates
  (*`type`_attr* for attributes,
  and `alloc`, `read_write`, `read_only` for
  registers and fields).

* The following standard parameters have been removed:

  <dl><dt>

  Object parameters
  </dt><dd>

  `logobj`, `indexvar`
  </dd><dt>

  `bank` parameters
  </dt><dd>

  `mapped_registers`, `unmapped_registers`,
  `numbered_registers`
  </dd><dt>

  `register` parameters
  </dt><dd>

  `fields`, `signed`
  </dd><dt>

  `connect` parameters
  </dt><dd>

  `interfaces`
  </dd></dl>

* The bank parameters `miss_bank`, `miss_bank_offset`,
  `miss_pattern`, `function` and `log_group`
  have been tentatively removed; we intend to replace them with
  standard templates.

* In `connect` objects, the `validate`
  and `validate_port` methods have been merged into one
  method, named `validate`. This method has the same
  signature and semantics as `validate_port` in DML 1.2.

* In objects of type `attribute`, `register`
  and `connect`, the methods `before_set`
  and `after_set` are no longer recognized. As a
  replacement, override `set` with a method that performs
  intended logic before and after an invocation
  of `default`.

* The templates `signed` and `noalloc` have been
  removed, together with the `signed`
  and `allocate` parameters in registers and
  fields.

* The parameter `offset` in registers can no longer be set
  to `undefined`. Instead, use the `unmapped` template
  to denote an unmapped register.

* The templates `unimplemented`, `silent_unimplemented`,
  `read_unimplemented` and `write_unimplemented` have been
  renamed, to `unimpl`, `silent_unimpl`,
  `read_unimpl` and `write_unimpl`,
  respectively. Also, log levels of logged messages have changed:
  The first message is logged on level 2 by `silent_unimpl`,
  and level 1 by all the other templates, while subsequent messages
  are logged on level 3 in all templates.

* Registers that instantiate the `constant` template should use the `init_val`
  parameter to declare their reset value, changed from the `value` parameter in
  DML 1.2.
