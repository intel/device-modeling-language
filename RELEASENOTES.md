<!--
  © 2021 Intel Corporation
  SPDX-License-Identifier: MPL-2.0
-->

# Device Modeling Language (DML)

- `major 6`
- `note 6`  The syntaxes for template instantiation in object declarations and in standalone statements have been unified; you can now write: `register r is read_only { ... }` to instantiate a single template and `is (read_only, write_unimplemented);` to instantiate two templates. Also, templates and traits now permit `is` before the opening brace, just like object declarations do. 
- `release 6 6004`
- `note 6`  The binary `<host>/bin/dmlc` has been replaced with the script `scripts/dmlc.py`. This is considered an internal change; from a user's perspective, the entry point of DMLC is still `bin/dmlc` 
- `release 6 6005`
- `note 6`  The type produced by the `<<` operator is now always `uint64` unless both operands are constant. Previously, the result could sometimes be a signed `int`, which could lead to unexpected sign extension when assigning the result to a `uint64` variable. 
- `release 6 6006`
- `note 6`  Restrictions have been lifted on what values can be assigned to a `bitfields` variable: Previously, only integer values small enough to fit in the bitfield size were permitted; now we permit assignment of any integer value, and truncate upon overflow. This eliminates some compile errors that were introduced by changes in Simics-Base 5.0.180. 
- `release 6 6007`
- `note 6`  In the `io-memory.dml` standard library file, the `operation` method is now marked `default` (fixes HSD-1507149841). 
- `note 6`  A new parameter `dml_1_2` has been added to device scope. The parameter is true, but will be false in future versions of the language. 
- `note 6`  The semantics of a `select` statement has changed to report errors less often in dead code. In particular, the `else` clause may now contain an `error` statement to get a compile-time check that the `where` expression was satisfied. 
- `release 6 6008`
- `note 6`  Fixed a bug that caused a crash with the deprecated `-M` flag 
- `release 6 6010`
- `note 6`  Upon object instantiation, required attributes inside banks can now be set through the compatibility attribute `bank_attr` in the device upon object creation. Previously, required attributes had to be set directly on the bank object (fixes HSD-2207518293). 
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
- `note 6`  Improved information provided by EATTRDATA error message. 
- `release 6 6015`
- `note 6`  The bank instrumentation framework no longer invokes callbacks on inquiry accesses. 
- `note 6`  Fix issue where the inquiry flag of memory operation was modified when the inquire function of the bank instrumentation framework was invoked. This could cause unnecessary trouble for users that recycle memory operations. 
- `release 6 6022`
- `note 6`  Improved how inline method arguments are handled when porting a device from DML 1.2 to 1.4. 
- `note 6`  It is now permitted to have `connect` objects inside banks. 
- `note 6`  Fixed various bugs that caused crashes and errors for inlined methods, in particular when mixing DML 1.2 and DML 1.4 code. 
- `release 6 6026`
- `note 6`  Multi-dimensional register, attribute, group, connect, event, bank, port, and implement arrays are now available. 
- `note 6`  Add support to `header` sections for including a `.h` file relative to the currently imported DML file (fixes HSD-2209645860). 
- `release 6 6028`
- `note 6`  Fixed a bug where the `DMLDIR_<name>_H` macro was undefined when generating dependency files. 
- `release 6 6030`
- `note 6`  DMLC will now report a syntax error if a DML file contains any invalid UTF-8 characters. Previously, invalid UTF-8 characters were ignored in comments. 
- `note 6`  The parameters `desc` and `documentation` are now required to be correctly encoded as UTF-8. 
- `release 6 6032`
- `note 6`  Adjusted DMLC code generation to silence false positives reported on `dml-builtins.dml` when analysing DMLC-generated C code using the Synopsis® Coverity® static analysis tool. 
- `release 6 6036`
- `note 6`  Improved some error messages (fixes SIMICS-15552).
- `note 6`  Added some rules to permit DML 1.4 methods to call 1.2 methods without a `try`/`catch` block in some common cases. Details are documented with the `EBADFAIL_dml12` error message. 
- `note 6`  When using DML 1.4 attribute templates, such as `bool_attr`, from a 1.2 device through `dml12-compatibility.dml`, the `get` method is no longer marked as `throws`. 
- `release 6 6044`
- `note 6`  Improved compatibility between DML 1.2 and 1.4: The experimental `dml12-compatibility.dml` file now provides default implementations of methods `read_register`, `write_register`, `read_field`, `write_field` and `io_memory_access`. When overriding these methods in a DML 1.4 file, there are templates `dml12_compat_read_register`, `dml12_compat_write_register` etc that will make sure the overrides are invoked if the file is imported from DML 1.2. 
- `note 6`  The `port-dml` script now accepts an argument `--compat` to generate 1.4 code that works better when imported from DML 1.2. The resulting code is less clean, so it should only be used on code that needs to retain 1.2 compatibility for some time. 
- `release 6 6046`
- `note 6`  The endian class of integer types are now available. These look like `[u]intX_(be|le)_t` (where X is a multiple of 8, up to 64) and describe an integer of the specified size with the specified byteorder and natural alignment of 1 byte. These can be transparently used as if they were regular `[u]intX`s in calculations (fixes SIMICS-7603). 
- `release 6 6049`
- `note 6`  Fixed an issue where typedeffed bitfields would get the incorrect size in layouts. 
- `release 6 6052`
- `note 6`  Added a script `bin/port-dml-module` to port all devices in one Simics module to DML 1.4. 
- `note 6`  The types `long`, `ulong`, `size_t` and `ssize_t` now correspond to the correct C types. For instance, on Linux, both the `long` and `int64` types are signed 64-bit integers, yet C considers them incompatible and refuses to assign between pointers of type `long *` and `int64 *`. DML now mirrors this behaviour, which makes it easier to call an externally defined C function that takes a `long *` argument. 
- `note 6`  Bitfield types are now considered compatible with the similarly sized unsigned integer type (e.g. `bitfield 32 {...}` is compatible with `uint32`). 
- `release 6 6053`
- `note 6`  Improved compatibility between DML 1.2 and 1.4: The experimental `dml12-compatibility.dml` file now also provides shared access to typed variables defined in the common templates `name`, `shown_desc`, `miss_pattern_bank`, and `function_mapped_bank`. See the DML 1.4 reference manual for details on which typed parameters each template provides. 
- `note 6`  The experimental `bank_obj` template is available in DML 1.2 through the experimental `dml12-compatibility` file. See DML 1.4 releasenotes for the functionality provided. 
- `release 6 6056`
- `note 6`  Registers and attributes that do not define the `documentation` parameter are now considered internal by default, meaning that they will be excluded from reference documentation by default. This can be changed by explicitly overriding the `internal` parameter of the register or attribute. 
- `release 6 6061`
- `note 6`  Fixed a bug that could cause a segmentation fault when instantiating a device with register arrays following certain patterns (fixes HSD-1508585437).
- `release 6 6075`
- `note 6`  If an array of `connect` objects is declared as `configuration=optional`, then it is now permitted to assign it a shorter list than the array size. For instance, a connect `connect objs[i < 10]` now has the type string `"o|[os]|n{0:10}"` (fixes SIMICS-16132). 
- `note 6`  Improved DML's type system. DMLC will now detect and report many type errors instead of generating broken C code. 
- `note 6`  typedefs of `layout` types may no longer be declared `extern`. It never worked to access members of such types, so they can safely be replaced with an opaque extern struct. E.g., if you have this declaration:
    ```
    extern typedef layout "big-endian" {
      uint24 member;
    } my_type_t;
    ```
     then you can safely replace it with:
    ```
    extern typedef struct { } my_type_t;
    ```
     
- `release 6 6079`
- `note 6`  `group` objects are now allowed anywhere in the device hierarchy. All kinds of objects except `bank`, `port`, `interface` and `implement` may reside inside a group (in DML 1.2, `field` objects are also disallowed inside groups) (fixes SIMICS-16563).
- `note 6`  `attribute`, `saved` and `session` objects are now permitted also inside `implement` objects. 
- `release 6 6082`
- `note 6`  Fixed an issue where interfaces defined as typedeffed aliases of other interfaces would not work correctly in some cases.
- `release 6 6088`
- `note 6`  Fixed a problem with strict aliasing; it is now safe to cast a `uint8` array to a layout pointer, assign to the layout members, and then access the arrays as bytes. This would sometimes give problems when using `-fstrict-aliasing` with new GCC versions (fixes SIMICS-16978). 
- `release 6 6090`
- `note 6`  DML devices compiled with simics-base 6.0.82 or later cannot be loaded with simics-base earlier than 6.0.76
- `release 6 6095`
- `note 6`  `after` statements now support methods with serializable method parameters (fixes SIMICS-7030).
- `release 6 6118`
- `note 6`  DMLC will now emit a warning when comparing an unsigned operand with a negative constant (fixes SIMICS-17096). 
- `release 6 6119`
- `note 6`  Single-bit wide ranges within bitfield declarations can now be specified through `@ [i]`, as with field objects (fixes SIMICS-18156).
- `release 6 6124`
- `note 6`  The DMLC standard library is now licensed under the BSD Zero Clause License.
- `note 6`  References to interface methods and `shared` methods are now allowed as parameter definitions. 
- `note 6`  The file `dmllib.h`, needed to compile DMLC-generated C files, has been moved to `host/bin/dml/include/simics`. Makefiles have been updated to add this directory to the include path. 
- `release 6 6131`
- `note 6`  Fixed a problem with `log error` statements when there are more than 32 log groups (fixes HSD-1305472692). 
- `note 6`  Unicode BiDi control characters are no longer permitted in DML source files, for security reasons. 
- `release 6 6132`
- `note 6`  Addressed code generation efficiency issues when using array types in `saved` variables or parameters to methods called with `after` (fixes SIMICS-18440).
    
    Serialized representation of `uint8[(be|le)_t]` array types has been changed. The old serialized representation is still accepted for deserialization in order to preserve checkpoint compatibility. 
- `release 6 6135`
- `note 6`  Added support for non-constant compound initializers inside methods. For example, the following is now supported: 
    ```
          local size_t size = get_size();
          local buffer_t buf = {size, new uint8[size]};
          
    ```
    
- `release 6 6139`
- `note 6`  `break` may now be used within `foreach` statements in DML 1.2 and `#foreach` statements in DML 1.4 (fixes HSD-1309451301). 
- `release 6 6141`
- `note 6`  (Partially) const-qualified `session` variables no longer result in invalid generated C (fixes SIMICS-9440).
- `note 6`  Implicitly initialized `local` variables of (partially) const-qualified struct type no longer result in invalid generated C (fixes SIMICS-10197).
- `release 6 6143`
- `note 6`  `default` can now be called within `destroy()` without causing an internal compiler error (fixes SIMICS-19598).
- `release 6 6176`
- `note 6`  The DMLC implementation is now shipped as source code, as `.py` files instead of `.pyc`.
- `note 6`  The C code emitted by DMLC is now identical every time, when given the same DML files as input. Previously, the compiler had nondeterministic behaviour that affected things like in what order C functions are output. Deterministic output allows more efficient use of tools like `ccache`.
- `release 6 6200`
- `note 6`  Fixed a bug with how template types were handled by DMLC that could cause the compiler to still exhibit nondeterministic behaviour.
- `release 6 6205`
- `note 6`  Added an error if a device declares more than 63 log groups (including the two built-in log groups.) 
- `release 6 6244`
- `note 6`  Added a warning for if a specified log level of a log statement is likely intended to instead specify the log groups, and/or vice versa (fixes HSD-22018374443). This warning is only enabled by default with Simics API version 7 or above. With version 6 and below it must be explicitly enabled by passing `--warn=WLOGMIXUP` to DMLC.
- `release 6 6247`
- `note 6`  Added a flag `--no-compat` to selectively disable compatibility features. In particular, `--no-compat=port_proxy_ifaces` disables generation of interface trampolines, which can speed up compilation of devices with huge arrays of ports. The option `--help-no-compat` lists all features that can be disabled.
- `release 6 6252`
- `note 6`  Addressed multiple issues with line directive generation which were detrimental to the use of debugging and coverage tools. Generated C lines should now only ever be redirected to DML when the DML line in question can be reasonably considered to describe the operation of the C line. 
- `note 6`  Fixed an issue where attempting to use inlined method parameters in constant equalities could lead to internal compiler errors or invalid generated C, if the corresponding arguments are themselves constant inlined method parameters (i.e. an inline method call propagates an inline parameter to an inline method call of its own.) 
- `major 7`
- `release 6 6255`
- `note 6`  The DMLC behaviour of suppressing the warning `WLOGMIXUP` by default below Simics API version 7 is now controlled by the compatibility feature `suppress_WLOGMIXUP`. This is in order to increase visibility of the API-dependent behaviour, and to provide migration documentation about it. As a consequence, `WLOGMIXUP` can be enabled below Simics API version 7 by passing either `--warn=WLOGMIXUP` (as before) or `--no-compat=suppress_WLOGMIXUP` to DMLC.
- `release 6 6274`
- `release 7 7007`
- `note 6`  Fixed an issue where leveraging `typeof` within a method signature would lead to incorrect compile-time errors when the method is overridden.
- `release 6 6283`
- `release 7 7011`
- `note 6`  Added support for the `log warning` statement.
- `release 6 6297`
- `note 6`  DMLC will now report errors for some uses of function types that are illegal in C. These would previously yield broken C code.
- `note 6` Created a compatibility feature `function_in_extern_struct`. The syntax `void
      m(conf_object_t)` for function pointers is only permitted when this feature is enabled. The feature will be disabled in Simics API versions \> 7.
- `release 6 6298`
- `release 7 7017`
- `note 6` Fixed a bug in the `dead_dml_methods` library, which caused it to never detect methods as dead if they occur directly after a `footer` block.
- `release 6 6310`
- `release 7 7022`
- `note 6` DMLC's use of the legacy Simics API for attribute registration is now controlled by the compatibility feature `legacy_attributes`. This feature can be disabled by passing `--no-compat=legacy_attributes` to DMLC, in which case the modern API will be used instead, which does not support the use of dictionary attribute values (fixes SIMICS-22406). `legacy_attributes` will always be disabled with Simics API version 8 or above.
- `note 6` Typechecking has been reworked to be more strict, and more accurately reject type mismatch that would result in invalid C or GCC warnings (fixes SIMICS-9504). To avoid breakage from novel type errors from the stricter typechecking, which particularly affects pointer types, the legacy lenient typechecking is still used by default with Simics API version 7 or below. The strict typechecking can be enabled by passing `--no-compat=lenient_typechecking` to DMLC.
- `release 7 7024`
- `note 6` Multiple \`extern\` declarations for the same identifier are now allowed as long as they all declare the same type for the identifier (fixes SIMICS-22472).
- `release 6 6313`
- `release 7 7025`
- `release 6 6316`
- `release 7 7027`
- `release 6 6138`
- `release 7 7029`
- `release 6 6320`
- `release 7 7032`
- `release 6 6321`
- `release 7 7033`
- `release 6 6324`
