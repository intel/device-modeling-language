<!--
  © 2021 Intel Corporation
  SPDX-License-Identifier: MPL-2.0
-->


# Device Modeling Language (DML) 1.2

- `major 6`
- `note 6`  Fixed a bug that caused a crash on the expression `1 << 64` (fixes HSD-1306575446).
- `release 6 6012`
- `note 6`  The `read` and `write` templates have been moved from utility.dml to always be included into 1.2 DML files. Additionally they are now inherited by all registers and fields by default. If you now see a name clash on `read` or `write` templates, you will need to rename your user-code defined templates. 
- `release 6 6018`
- `note 6`  The `get` and `set` methods are now shared in attributes inheriting from the DML 1.4 utility templates (`bool_attr` etc.). 
- `release 6 6019`
- `note 6`  The `import` statement has been changed: If the path starts with `./`, the path is now interpreted relative to the directory of the importing file (fixes HSD-2209506845). 
- `note 6`  Fixed a bug that caused a bad warning and incorrect code when the operand to `sizeof` is an identifier that can be resolved both to a value and to a type. 
- `release 6 6024`
- `note 6`  The project build environment is now sensitive to an environment variable `DMLC_PORTING_TAG_FILE`, which should be a filename. When defined, DMLC will create the file if it doesn't already exists, and then append data to the end of the file, containing machine-readable instructions on how to port the device to DML 1.4. The typical use case is to set this variable when building one or more modules, and feed the resulting file as input to the `port-dml.py` script. The -P command-line argument has also been changed: it now takes a filename argument, and can be used without -T. 
- `release 6 6026`
- `note 6`  Changed the order in which registers are being reset upon hard\_reset or soft\_reset, and the ordering of the parameters `mapped_registers` and `unmapped_registers`. The order is still considered undefined, but in practice it now changes less randomly, and is equal on Simics 5 and Simics 6. 
- `note 6`  Fixed a bug in nested `foreach` loops when traversing register arrays inside bank arrays (fixes HSD-1809370911). 
- `release 6 6031`
- `note 6`  It is now permitted to use the `nothrow` annotation on a method override even if the overridden method does not. This is sometimes useful while porting a large code base to DML 1.4, because it allows `get` and `set` methods in common code to be declared as non-throwing, which is required by the DML 1.4 standard library. 
- `note 6`  Fixed a language inconsistency: When DML 1.4 code contains an `if` statement inside a method body, and this code is imported from a DML 1.2 file, then the statement used to be interpreted as a DML 1.2 `if` statement; it is now interpreted as a DML 1.4 `if` statement as expected. The difference is that if the `if` condition is constant, then DML 1.2 would discard the dead branch without checking for errors, while 1.4 evaluates both the live and dead branch. This change can give new compile errors in existing code; in this case, you can normally resolve the problem by changing `if` to `#if`. 
- `release 6 6036`
- `note 6`  Field arrays are now fully supported and documented, and don't give compile warnings. 
- `note 6`  Fixed a DMLC crash when generating porting tags. 
- `note 6`  Add porting rule for uses of 1-bit fields as booleans; converts `if (f)` into `if (f != 0)`. 
- `release 6 6041`
- `note 6`  Fixed a problem with the template `custom_time_event` from `dml12-compatibility.dml`. 
- `release 6 6048`
- `note 6`  Applying bitwise not (`~`) on a boolean value now gives a compile error. Previously it resulted in an unconditionally true value, which is not what you want. 
- `release 6 6049`
- `note 6`  Fixed an issue with using endian integers as arguments in pointer arithmetic. 
- `note 6`  Fixed an issue preventing assignment from const layout-typed pointers. 
- `release 6 6050`
- `note 6`  Taking the address of a bitfield type in a layout will now correctly give you a pointer of the bitfield type. 
- `release 6 6053`
- `note 6` Fixed a crash on methods marked `nothrow` and multiple return arguments, returning a typed template or struct type (fixes HSD-18012036595). 
- `note 6` The file `dml12-compatibility.dml` now adds a parameter `val` to `field` objects. 
- `release 6 6054`
- `note 6` Unmapped registers names are now properly anonymized according to \_confidentiality (fixes HSD-1508117636). 
- `release 6 6059`
- `note 6`  Fixed a regression in use cases where `if` has a constant condition, and the taken branch consists of a variable declaration. I.e., if the code `if (true) local int x;` appears in a scope, then the variable `x` is now added to that scope (fixes HSD-18013028128). 
- `release 6 6066`
- `note 6`  If a `static` variable is declared within a method declared under an object array, it will now result in a separate instance of the variable for each instance of the containing object (fixes SIMICS-13738). 
- `release 6 6079`
- `note 6`  It is no longer possible to override the `loggroup` parameter inside a `group` object. 
- `note 6`  Added documentation of the `nothrow` annotation for methods (fixes HSD-18012035545). 
- `release 6 6082`
- `note 6`  Fixed a compile error when a DML 1.2 device imports a DML 1.4 file that declares a `saved` variable inside a register or field (fixes SIMICS-17201). 
- `release 6 6096`
- `note 6`  Fixed an issue causing assertion error when accessing the register info of a 1.2 multi-dimensional bank array (fixes SIMICS-17308). 
- `release 6 6100`
- `note 6`  Added the `--state-change-dml12` flag, allowing the use of an incomplete version of state change notifiers in DML 1.2 devices (fixes SIMICS-17952).
- `release 6 6122`
- `note 6`  Fixed an issue when using the `--state-change-dml12` flag together with the `--split-c-file` flag (fixes SIMICS-17975).
- `release 6 6124`
- `note 6`  Fixed a few issues where confidential information could be leaked through non-confidential children of confidential objects. Specifically, these issues occurred with event objects belonging to confidential registers, `after` calls to methods belonging to confidential objects, and accessing `qname` of non-confidential children belonging to confidential objects.
    
    A warning is now emitted when `qname` of a non-confidential child of a confidential object is used outside of `log` statements (fixes SIMICS-18428).
- `release 6 6132`
- `note 6`  Like in DML 1.4, objects of type `port` can now be placed inside `group` objects. However, `bank` objects are still restricted to top level (fixes SIMICS-19009). The new object type `subobject` is not directly accessible in DML 1.2, but it is permitted to import DML 1.4 code that declares `subobject` objects.
- `release 6 6149`
- `note 6`  Add porting rule for uses of `undefined` as register offsets; replaces such uses with `unmapped_offset` (fixes SIMINT-1486).
- `release 6 6158`
- `note 6`  Fixed issue where certain parameters defined in DML 1.4 modules couldn't be referenced within DML 1.2 modules without the use of `$`.
- `release 6 6160`
- `note 6`  Add porting rule to convert the `int1` type into `uint1` in DML 1.4, which better resembles how the type works in 1.2.
- `release 6 6213`
- `note 6`  Improve porting rules for method return values: In some simple cases, avoid storing the method's return value in an intermediate local variable.
- `release 6 6215`
- `note 6`  If a bank declared with `use_io_memory=false` in a DML 1.4 file is imported from a DML 1.2 device, using the `dml12-compatibility.dml` library, then a call to the `transaction_access` method will now trigger a proper bank access. Also, if a bank uses the `dml12_compat_io_memory_access` template, then overrides of the `transaction_access` method will be honored. This simplifies future transition to the Simics 7 API.
- `release 6 6218`
- `major 7`
- `release 6 6316`
- `release 7 7027`
- `release 7 7029`
- `release 6 6320`
- `release 7 7032`
- `release 6 6321`
- `release 7 7033`
- `release 6 6324`
- `note 6` Fixed a regression where e.g. `cast(0x400000000, uint64)` would not
  be considered a constant value (fixes SIMINT-1675).
- `note 6` Added support for the templates `simple_cycle_event`, `simple_time_event`, `uint64_cycle_event` and `uint64_time_event` in the forward compatibility layer provided by `dml12-compatibility.dml`.
