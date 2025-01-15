<!--
  © 2021 Intel Corporation
  SPDX-License-Identifier: MPL-2.0
-->

# Device Modeling Language (DML) 1.4

- `major 6`
- `note 6`  Initial release of DML version 1.4. The new language version is documented in the *DML 1.4 reference manual* document. Some highlights:  
    - **Better compilation performance**   
         2-3 times faster compile for devices with large register banks 
    - **Improved reset support**   
         The reset mechanism in 1.4 is not hard-coded, and can be adapted to different reset flows 
    - **Templates as types**   
         You can pass around references to banks/registers/fields etc in variables 
    - **Multiple levels of overrides**   
         Methods and parameters can be overridden any number of times. In 1.2, this is often prevented by standard library legacy. 
    - **Syntax simplification**   
         Many adjustments to improve readability; for instance, removal of `$` and C-like method syntax 
    - **Standard library API simplification**   
         Clearer API, easier to understand 
    - **Updated language semantics**   
         More consistent and well-specified behaviour of method bodies and expressions, providing more predictable model behaviour 

     
- `release 6 6012`
- `note 6`  Export declarations are now available as replacements for `method extern`. 
- `release 6 6013`
- `note 6`  The dml12-compatibility.dml library file is now available. This file should be included from DML 1.4 files that use or override templates defined in `dml-builtins.dml` and are meant to be backwards-compatible with DML 1.2 devices. 
- `note 6`  Templates can now override `get` or `set` method templates and still be inherited by both registers and fields. 
- `release 6 6018`
- `note 6`  Fixed two problems where devices with a very large number of registers compiles slowly (fixes SIMICS-13802 and SIMICS-13803. 
- `release 6 6019`
- `note 6`  The `import` statement has been changed: If the path starts with `./` or `../`, the path is now interpreted relative to the directory of the importing file (fixes HSD-2209506845). 
- `note 6`  Fixed a problem where parameters defined in global scope in DML 1.4 (typically auto-converted from a DML 1.2 `constant`) had to be accessed with `$` when referenced from a parameter in a DML 1.2 file. 
- `release 6 6024`
- `note 6`  The `read_unmapped_bits` and the `write_unmapped_bits` methods are now available to be overridden for registers. This can be used to customize behaviour of registers with fields that do not cover all register bits. 
- `note 6`  Fixed bug in dml-lib "common.dml" where `pci_system_error` could not be called. 
- `release 6 6037`
- `note 6`  Fixed a bug that sometimes caused a crash when calling the default implementation of a shared method. 
- `note 6`  Fixed a bug that caused unused goto labels in generated C code. 
- `note 6`  Fixed a bug that caused DMLC to ignore compile errors in the right operands of the `&&` and `||` operators. For instance, in `a && b`, and `a` resolves to constant false, and `b` is nowhere defined, then DMLC would previously silently evaluate the expression to false; now it will give a proper error message. Expressions like `cond_is_defined && cond()`, where `cond_is_defined` is a constant parameter, can be rewritten as `pred_is_defined #? pred()
    #: false`. 
- `release 6 6041`
- `note 6`  The `get` and `set` methods are now overrideable in bank objects. 
- `release 6 6042`
- `note 6`  Fixed a bug in how log group names are generated. 
- `release 6 6044`
- `note 6`  In `switch` statements, `case` labels are now permitted inside `#if` blocks (fixes HSD-14011267833). 
- `note 6`  Fixed an issue where the `++` and `--` operators could not be applied to pointers. 
- `note 6`  Added compile warnings when trying to override a library method that was called in DML 1.2 but is ignored in DML 1.4. 
- `release 6 6046`
- `note 6`  Obtaining the address of a member of a layout is now supported (fixes SIMICS-15381). 
- `note 6`  DMLC now gives an explicit error message on name collision between two members of a `layout` or `bitfields` (fixes SIMICS-9088). 
- `release 6 6049`
- `note 6`  Fixed issue with passing endian integers as arguments to shared methods. 
- `release 6 6051`
- `note 6`  The `get` method in `bank` objects is now declared `throws`; a read outside registers causes an exception like in DML 1.2, causing the inquiry access to fail. 
- `note 6`  The parameters `partial` and `overlapping` in banks are now `true` by default. 
- `release 6 6052`
- `note 6`  Field objects now have a `val` member. This is currently a bitslice into the field's bits within the parent register's storage. The .val parameter can be read, and it can be written using assignment (`=`); however, the operators `--` and `++` do not work yet. Also, `.val` cannot be accessed from a shared method. 
- `note 6`  Fix a bug that caused DMLC to crash in declarations of field objects in files with a `bitorder be` declaration (fixes SIMINT-1275). 
- `release 6 6053`
- `note 6`  DMLC will now give you a better error in some cases where a typed template parameter has an initializer that cannot be evaluated in a static context. 
- `note 6`  The experimental `bank_obj` template is now available in the `utility` file, providing a single shared method `bank_obj` which returns the bank configuration object of a bank. 
- `release 6 6056`
- `note 6`  The formatting of the log message output in the default implementation of partial register reads and write has been improved. 
- `release 6 6059`
- `note 6`  You can now access `.len` on lists and sequences to obtain the number of elements they contain. This expression is constant for lists and non-constant for sequences. 
- `note 6`  Reverted a change that allowed DMLC to provide better error messages when typed parameters had non-static initializers since it was overly conservative in some cases. 
- `release 6 6060`
- `note 6`  Removed the syntax for goto labels (`label:`). `goto` statements are already forbidden in DML 1.4, so goto labels are useless. `case` and `default` labels in `switch` blocks are still allowed. 
- `note 6`  If the condition of a `while` loop is statically evaluated to false, then faulty code in the loop body can now cause compile errors. Previously the loop body was silently discarded. 
- `release 6 6061`
- `note 6`  Removed incorrect documentation of the deprecated 'banks' parameter. 
- `release 6 6075`
- `note 6`  `session` variable declarations are no longer permitted within methods marked with `inline`. 
- `note 6`  `saved` variable declarations are now available. These can be used to declare variables that behave similarly to `session` variables, but that are automatically checkpointed (fixes SIMICS-7031). 
- `note 6`  If a `static` variable is declared within a method declared under an object array, it will now result in a separate instance of the variable for each instance of the containing object (fixes SIMICS-13738). 
- `note 6`  Fixed a bug that caused a crash in `print-device-regs` and related commands, when inspecting a function-mapped bank array in a DML 1.4 device (fixes HSD-1508646546).
- `release 6 6079`
- `note 6`  If a method argument has mismatching type in an override, then an error will now be reported (fixes SIMICS-9337).
- `release 6 6080`
- `note 6`  DMLC will now report errors for references to undefined type names in some rare cases that previously were ignored, such as unused typedefs and functions imported from C using `extern` (fixes SIMICS-16187).
- `note 6`  DMLC will now report an error for methods in `implement` blocks, if the method's return type does not match the interface method. Previously, only input arguments were typechecked in interface methods
- `release 6 6081`
- `note 6`  The parameter `connect` now accepts the values `"none"` and `"pseudo"`, just like attributes and registers do.
- `release 6 6082`
- `note 6`  Added a template `init_as_subobj` which can be used on `connect` objects to make it instantiate a subobject automatically (fixes SIMICS-16131).
- `release 6 6084`
- `note 6`  Added `saved` variables declared within templates to the template type.
- `note 6`  Added parameter `unmapped_offset` which makes a register unmapped.
- `release 6 6095`
- `note 6` Fixed typo in documentation of `miss_pattern_bank` template.
- `release 6 6102`
- `note 6`  Corrected documentation in reference manual regarding `read_bits` and `write_bits` methods. 
- `note 6`  An illegal arithmetic operation, such as a division by zero or negative shift, no longer crashes the simulation. Instead it is reported as a critical error, and evaluates to zero (fixes SIMICS-16639).
- `release 6 6104`
- `note 6`  You can now use the syntax `X then Y` for the level in log statements to have the first log happen on log level `X` and subsequent logs happen on level `Y` (fixes SIMICS-13513).
- `note 6`  Fixed an issue where multiple registers or fields instantiating `read_only` or similar templates from `utility.dml` would switch log-level at the same time (fixes SIMICS-7025).
- `release 6 6119`
- `note 6`  You can now access `.len` on constant-sized object or value arrays to obtain the number of elements they contain. These expressions are constant (fixes SIMICS-13114).
- `release 6 6123`
- `note 6`  `after` statements now support cycles as a unit of time (fixes SIMICS-8798).
- `release 6 6127`
- `note 6`  The compiler now explicitly forbids using the `export` statement on a method with multiple output parameters.
- `note 6`  Various improvements to the reference manual has been made. 
- `release 6 6129`
- `note 6`  It is no longer an error to instantiate a non-existing template from within a dead branch of an `#if` block. 
- `release 6 6131`
- `note 6`  Two values of the same template type can now be compared for equality, and are equal when they reference the same object (fixes SIMICS-6998).
    
    The internal representation of values of template types has been changed. It is no longer allowed to cast such values to integer or pointer types. 
- `note 6` Attributes that do not define *either of* the `documentation` parameter or the `desc` parameter are now considered internal by default. Register-attributes are internal by default, regardless if they define `documentation` or `desc`. This can be changed by explicitly overriding the `internal` parameter of the register or attribute. 
- `note 6`  Fixed an issue where a log statement could fail to switch log levels when used within a shared method and called through trait references. 
- `note 6`  Fixed an issue where a log statement could incorrectly switch log levels when used within a method declared within an object array. 
- `note 6`  Writes outside fields in a register will now by default log one `spec-violation` message on log level 1 on first such write, and no logging on further such writes. 
- `release 6 6133`
- `note 6`  Added a template `map_target` which can be used on `connect` objects to expose methods for reading and writing data from/to the connected object.
- `note 6`  The device info XML file is no longer needed by DML 1.4 devices. A dummy XML file is still created to avoid packaging problems.
- `note 6`  Optimized the DML-provided implementation of the `register_info` and the `number_of_registers` methods of the `register_view` interface (fixes SIMICS-18427). 
- `release 6 6135`
- `note 6` Banks that use the transaction interface (the parameter `use_io_memory` is `false`) now handle accesses that are bigger than 8 bytes.
- `release 6 6137`
- `note 6`  Added simple C99-style designated initializers, which may be used together with `struct`-like types (fixes SIMICS-17252). For example, the following is now supported: 
    ```
          local size_t size = get_size();
          local buffer_t buf = {.len = size, .data = new uint8[size]};
          
    ```
    
- `release 6 6139`
- `note 6` Added an optimization that reduces the compile time for devices with huge register arrays (fixes SIMICS-7038).
- `note 6`  Added support for cancelling events posted via `after` statement through the use of the `cancel_after()` method, provided as part of the `object` template (fixes SIMICS-17930). 
- `release 6 6143`
- `note 6`  `break` may now be used within `foreach` statements (fixes SIMICS-18719). 
- `release 6 6145`
- `note 6`  When declaring an object array, any dimension size specification may now be omitted if already defined through a different declaration of the same object array (fixes HSD-22014423596). Omission is done by specifying `...` instead of the dimension size; for example, the following is now supported: 
    ```
          group g[i < 4][j < ...] { }
          group g[i < ...][j < 7] { }
          
    ```
    
- `release 6 6147`
- `note 6` The `issue` method in the `map_target` template can now be overridden.
- `release 6 6148`
- `note 6`  Objects of type `port` and `bank` can now be placed inside `group` objects.
- `note 6`  Added a new object type `subdevice`. This object behaves similar to a `port`, with two differences: It may contain `port` and `bank` objects, and the corresponding Simics object does not add a `port.` prefix. For instance, the object for `subdevice x;` is named `dev.x` instead of `dev.port.x`.
- `note 6`  Fixed a bug that caused incorrect Simics object names when the `init_as_subobj` template is instantiated on a `connect` object inside a `bank` or `port` object.
- `release 6 6149`
- `note 6`  Multiple variables may now be declared simultaneously using tuple syntax (parentheses), and tuple syntax can now be used for multiple simultaneous assignment or initialization (fixes SIMICS-7027).
- `note 6`  Calls to throwing methods or methods with multiple return values may now be used as the argument to a return statement. Compound initializer syntax can now be used to return values of `struct`-like types, e.g.: 
    ```
          method construct_buffer_t(size_t siz) -> (buffer_t) {
              return { .len = siz, .data = new uint8[siz] };
          }
          
    ```
    
- `release 6 6150`
- `note 6` Added new templates `signal_port` and `signal_connect` which help maintain correct handling of the signal interface, i.e. do not raise signal when already high, and do not lower it when already low.
- `release 6 6153`
- `note 6`  Added independent methods (fixes SIMICS-6181), independent startup methods (fixes SIMICS-17632), and independent startup memoized methods. 
- `note 6`  Invalid definitions of typed parameters now emit compile errors (fixes SIMICS-6182). 
- `note 6`  Method references can now be converted into function pointers using `&`. This can only be used with methods that that may also be exported using the `export` statement (fixes SIMICS-14921). 
- `release 6 6154`
- `note 6`  Overlapping field ranges within registers are no longer allowed (fixes SIMICS-19194). 
- `note 6`  Fixed regression introduced in Simics 6.0.133, where methods or variable named `independent`, `startup` or `memoized` would give a parse error.
- `release 6 6157`
- `note 6`  Fixed a bug where independent methods were exported incorrectly.
- `release 6 6161`
- `note 6` The methods `signal_raise` and `signal_lower` are now overridable in HRESET port defined in template `hreset`
- `release 6 6171`
- `note 6`  When a transaction misses in a bank, the error message is now logged by the `unmapped_read` and `unmapped_write` methods of the bank, instead of `io_memory_access` or `transaction_access`. This makes it easier to override the default behaviour; however, if a model suppresses logging by providing a local alternative implementation of the `*_access` methods, and that implementation still calls `unmapped_read` or `unmapped_write`, then that model will no longer suppress logging.
- `release 6 6173`
- `note 6` Removed the generation of some broken `#line` directives that caused problems in code coverage reports (fixes HSD-18024044100).
- `note 6` When using designated initializers, partial initialization is now possible through trailing `...` syntax (fixes SIMICS-18705). Members not explicitly initialized are zero-initialized. For example, the following is now supported: 
    ```
          local struct { int value; bool valid; } x = {.valid = false, ...}
          
    ```
    
- `release 6 6177`
- `note 6` Template types are now considered serializable (fixes SIMICS-18507).
- `note 6` Casts of constant object references to template types are now considered constant expressions, allowing for their use as initializers for `session`/`saved` variables: 
    ```
          template t { }
          group g is t;
          saved t s = cast(g, t);
          
    ```
    
- `release 6 6178`
- `note 6`  Fixed a bug for ports/banks inside groups, that inhibited the creation of attributes for `saved` variables (fixes SIMICS-19422)
- `release 6 6183`
- `note 6` Fixed a bug where `session`/`saved` variables initialized to values of template types could cause internal compiler errors or invalid generated C (fixes SIMICS-20145).
- `release 6 6184`
- `note 6` Fixed a bug where models containing any parameter defined to be a value of a template type resulted in broken generated debug files when compiling with `-g` (fixes SIMICS-20200).
- `note 6` The `set` method in the `map_target` template can now be overridden, to add extra behavior before and after the attribute is set.
- `release 6 6187`
- `note 6` Any value of a template type can now be cast to the template type `object`, even if `object` is not an ancestor of the template (fixes SIMICS-19950).
- `release 6 6190`
- `note 6` A new parameter `init_val` has been introduced to the attribute templates `bool_attr`, `uint64_attr`, `int64_attr` and `double_attr`. These templates now also provide a default `init()` implementation that leverages `init_val` in order to initialize the `val` variable provided by the template (fixes SIMICS-20108). 
- `note 6` `NULL` is now considered a constant expression, and equalities between `NULL` and other constant expressions of pointer type (in particular, string literals) are now also considered constant.
- `note 6` Attribute registration for `connect`, `attribute`, and `register` objects has been greatly optimized, reducing the compilation times of models that feature large numbers of such objects (fixes SIMICS-6271). A consequence of this change is that errors in the `get_attribute()`, `set_attribute()`, `set()`, and `get()` methods of such objects are always reported by the compiler, even if the `configuration` parameter is defined to be `"none"`. 
- `note 6` Fixed a bug where leveraging serialization of a template type could cause an internal compiler error if there is some object in the model instantiating that template which never gets referenced in a value of that template type (fixes SIMICS-20301). 
- `note 6` The `register_info` interface for banks now provide unique names for each field in an array (fixes SIMICS-18244).
- `note 6` Fixed a bug where `.len` on template type sequences could only be used with direct `each`-`in` expressions (fixes SIMICS-20278).
- `release 6 6191`
- `note 6` Fixed Windows-specific gcc compile error on DMLC-generated code, caused by name clashes with the `interface` macro, defined by `windows.h` (fixes HSD-15012582368).
- `release 6 6195`
- `note 6` Fixed a bug in `saved` variables that caused stack overflow during checkpoint restore for huge (multi-megabyte) struct types (fixes HSD-18026246959).
- `release 6 6200`
- `note 6`  `--coverity` has been added as an option to DMLC. When used, DMLC will generate Synopsys® Coverity® analysis annotations to suppress common false positives in generated C code created from DML 1.4 devices.
- `note 6`  Fixed an ICE caused by constant inlined method parameters being used in constant equalities (fixes HSD-16019548195).
- `note 6`  Fixed an issue with debuggable compilation (`-g`) that caused `inline` method calls to inline constant arguments even for parameters not declared `inline` (fixes HSD-16019548195).
- `release 6 6205`
- `note 6`  Compound initializer syntax can now be used to construct arguments of a method or function call, e.g: 
    ```
          method callee(buffer_t buf, bool b) {
              ...
          }
    
          method caller() {
              callee({ .len = 4, .data = new uint8[4] }, true);
          }
          
    ```
     This syntax can't be used for variadic arguments or any argument corresponding to a method parameter declared `inline` (fixes SIMICS-20473).
- `release 6 6213`
- `note 6` Fixed a bug where a top-level object named the same as the device object could result in invalid generated C when both objects are targets of `each
      .. in ..` expressions (fixes SIMICS-21044).
- `release 6 6218`
- `note 6` `--coverity` now suppresses a false positive that the `COPY_PASTE_ERROR` checker reports in `dml-builtins.dml`.
- `note 6` It's now allowed to cast an expression of a struct type to that same struct type.
- `note 6` Fixed an error where const method parameters would cause an internal error if that method is used as the callback of an `after` statement.
- `note 6` Fixed an issue where having a const-qualified layout type could lead to internal compiler errors in certain scenarios.
- `release 6 6223`
- `note 6`  Added `hook` declarations; a `hook` is a named object member to which suspended computations may be attached for execution at a later point. Computations suspended on a hook are detached and executed through the `send_now()` operation of the hook, which can also provide data for use by the resumed computations. Currently, the only form of computations that can be suspended on hooks are single method calls. This is done via the `after` statement, which has been extended to permit this.
- `release 6 6231`
- `note 6` Fixed a bug where partial inquiry writes to a bank would incorrectly clear register bits (fixes SIMICS-21382).
- `note 6` Fixed a bug where trying to serialize values of `long` or `size_t` type (via `saved` variables or arguments of `after` statements) would lead to an internal compiler error (fixes SIMICS-21424).
- `note 6`  Added a new form of the `after` statement, called *immediate `after`*. Syntax is `after: callback(1, false)`. An immediate after will delay the call to the specified method until all current entries into devices on the call stack have been completed, and the simulation of the current processor would otherwise be ready to move onto the next cycle (fixes SIMICS-20379).
- `note 6`  Fixed a bug where an attribute declared within a port, bank or subdevice that is itself declared within an object array would lead to the type of that attribute being incorrectly registered (fixes SIMICS-21474).
- `release 6 6244`
- `note 6`  Fixed a bug where log statements within `shared` methods would always use the device's configuration object as the log object instead of the configuration object of the nearest enclosing `device`, `port`, `bank` or `subdevice` (fixes SIMICS-11346). This fix is only enabled by default with Simics API version 7 or above. With version 6 or below it must be explicitly enabled by passing `--no-compat=shared_logs_on_device` to DMLC.
- `major 7`
- `release 6 6252`
- `note 6`  Improved the run-time performance of accesses to registers with many fields.
- `release 6 6262`
- `release 7 7006`
- `note 6`  Fixed a bug where overlapping registers would not be rejected if they were part of a bank within a group or subdevice.
- `release 6 6287`
- `release 7 7012`
- `note 6`  Fixed a bug where executed callbacks posted via immediate after statements would not trigger the device state change notifier.
- `release 6 6295`
- `release 7 7016`
- `note 6`  Added support for `uint64_t` and `int64_t` types from C. These types are needed for C interoperability, e.g., calling a function that takes an argument of type `uint64_t *`.
- `note 6`  The `miss_pattern_bank` template now allows for its implementations of `unmapped_read`, `unmapped_write`, and `unmapped_get` to be overridden. 
- `release 6 6297`
- `release 7 7017`
- `note 6`  It's now allowed to instantiate multiple unrelated templates that offer different `shared` implementations of the same method, as long as that method is declared in a common ancestor template, and the conflict is resolved through an implementation that overrides all the unrelated implementations (fixes SIMICS-20431). This mirrors the pre-existing semantics for non-`shared` methods.
- `note 6`  Added the `templates` member for all objects and values of template types, which allows for making calls to any particular implementation of a method as provided by a specified template (fixes SIMICS-22264). Example syntax: 
    ```
          // Call the `write_register()` implementation as provided by the
          // `register` template on the `r` register of the bank `b`.
          b.r.templates.register.write_register(...);
          
    ```
     Calls of this form are called *Template-Qualified Method Implementation Calls*, and are primarily meant to make it easier to resolve conflicts in multiple inheritance: it allows any method that overrides multiple implementations from unrelated templates to individually reference and call the implementations it overrides.
- `release 7 7020`
- `note 6`  Template `function_io_memory` now searches for the bank in the same `subdevice`/`device` scope as the `implement`. 
- `release 6 6309`
- `release 7 7021`
- `note 6` Added a system for *provisional language features*: language features that are disabled by default but can be explicitly enabled per DML file. Each provisional feature is named and documented in an appendix of the reference manual; to enable a feature named `foo_bar` in a DML file, write `provisional
      foo_bar;` in the top of the file, right after `dml
      1.4;`.
- `note 6` Added a provisional feature `explicit_param_decls`, which can be enabled per file by a statement `provisional explicit_param_decls;`. This feature adds a syntax `param NAME := VALUE;`, which signifies that the parameter is *not* intended as an override. The existing syntax `param NAME = VALUE;` is re-purposed to signify that the parameter *is* intended as an override. Inconsistent usage will trigger compile errors. This is useful to catch misspelled overrides (fixes SIMICS-21451).
- `note 6` Addressed a bug where DMLC did not report errors when a non-existing template is instantiated within an `#if` block. E.g., the following does not give an error: 
    ```
          #if (true) { group g { is nonexisting_template; }}
          
    ```
     The bugfix is opt-in, because an immediate bugfix would risk breaking existing builds; the error will only be reported when the flag `--no-compat=broken_conditional_is` is passed to DMLC. This flag will be automatically enabled in Simics 8 (fixes SIMICS-22403).
- `note 6`  When two conflicting definitions of the same parameter are declared within the same scope, then this will now be reported as an error also if they are overridden by a third declaration. In the following example, this gives a new compile error on the `p` declarations within the `t` template: 
    ```
            template t {
              param p default 1;
              param p default 1;
            }
            group g is t {
              param p = 2;
            }
          
    ```
     Previously, DMLC would only report an error on `t` if the parameter was not overridden. If this is the desired behaviour, then the idiomatic construct is: 
    ```
            template t {
              param p;
            }
          
    ```
    
- `release 6 6310`
- `release 7 7022`
- `note 6`  Any immediate after callbacks posted during object initialization and configuration are now guaranteed to be executed as part of `objects_finalized` of the device. 
- `note 6`  Any immediate after callbacks of a device that are still pending at the start of the device's deletion will be canceled and give rise to a warning (fixes SIMICS-22268). This can be resolved by leveraging `SIM_process_pending_work()`. Immediate after callbacks posted *during* deletion (e.g. by `destroy()`) will still be executed at the end of device deletion, as before. 
- `release 6 6312`
- `release 7 7023`
- `note 6` Calls to methods defined within object arrays now assert that the specified indices for the object arrays are in bounds (fixes SIMICS-22491). For backwards compatibility, these asserts are not performed with Simics API version 7 or below unless `--no-compat=no_method_index_asserts` is passed to DMLC.
- `release 6 6313`
- `release 7 7026`
- `note 6` The warning message for comparing a value of unsigned type to a negative constant has been improved to also warn for unsigned types shorter than 64 bits. 
- `release 6 6315`
- `release 7 7027`
- `release 7 7029`
- `release 6 6320`
- `release 7 7032`
- `release 6 6321`
- `release 7 7033`
- `release 6 6324`
- `note 6` The `explicit_param_decls` provisional feature is now considered stable. Its use will be kept supported in the same way as any standard DML feature.
- `release 7 7036`
- `note 6` `after` statements now support picoseconds (`ps`) as a unit of time (fixes SIMICS-16019).
- `note 6` Fixed an issue where overriding a `shared` method with another could
  cause an internal compiler error if the method signature involves a template
  type.
- `note 6` Fixed an issue where the validity of referenced types in the
  signatures of unused `shared` methods were not properly checked, leading to
  invalid generated C.
- `release 6 6329`
- `note 6` `continue` can now be used within `foreach` loops (use within
  `#foreach` loops remains unsupported)
- `release 7 7037`
- `release 6 6330`
- `note 6` Added the `destroy` template, the device deletion equivalent of the
  `init` and `post_init` templates. When the device is being deleted,
  `destroy()` will be called of every object that instantiates the `destroy`
  template. The device object always instantiates this template.
- `release 6 6333`
- `release 7 7042`
- `release 7 7041`
- `note 6` Fixed an issue where initializing a `local` variable via a method
  call was not properly type-checked, which could result in invalid code being
  accepted, as well as incorrect semantics, invalid generated C, or undefined
  behaviour (fixes SIMICS-22874).
- `release 6 6356`
- `release 7 7060`
- `note 6` Addressed a bug where DMLC did not report errors when an `extern typedef` referenced a non-existing type. E.g., the following would not give an error if `never_used_t` was never used:
  ```
  extern typedef struct {
      undefined_type_t member;
  } never_used_t;
  ```
  The bugfix is opt-in, because an immediate bugfix would risk breaking existing builds; the error will only be reported when the flag `--no-compat=broken_unused_types` is passed to DMLC. This flag will be automatically enabled in Simics 8.
