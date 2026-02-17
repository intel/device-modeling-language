# © 2024 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import abc
from . import logging
from . import messages

class ProvisionalFeature(abc.ABC):
    def tag(self) -> str:
        return self.__class__.__name__

    @abc.abstractproperty
    def __doc__(self): pass

    @abc.abstractproperty
    def short(self) -> str: pass

    @abc.abstractproperty
    def stable(self) -> str: pass

    # Whether the feature is included in 1.2 documentation
    dml12 = False

# tag -> feature
features: dict[str, ProvisionalFeature] = {}


def feature(cls: type[ProvisionalFeature]):
    assert issubclass(cls, ProvisionalFeature)
    singleton = cls()
    features[singleton.tag()] = singleton
    return singleton


@feature
class explicit_param_decls(ProvisionalFeature):
    '''<a id="explicit_param_decls"/>
    This feature extends the DML syntax for parameter definitions to
    distinguish between an intent to declare a new parameter, and an intent to
    override an existing parameter (including when providing a definition
    for an abstract parameter). This distinction allows DML to capture
    misspelled parameter overrides as compile errors.

    The following new forms are introduced to mark the intent of
    declaring a new parameter:

    * For typed parameters, `param NAME: TYPE = value;` is essentially a shorthand for

          param NAME: TYPE;
          param NAME = value;

      and similarly, `param NAME: TYPE default value;` is essentially a shorthand for

          param NAME: TYPE;
          param NAME default value;

    * For untyped parameters, `param NAME := value;` is essentially a shorthand for

          param NAME;
          param NAME = value;

      and similarly `param :default value;` is essentially a shorthand for

          param NAME;
          param NAME default value;

    If one of these forms is used for overriding an existing
    parameter, then DMLC will signal an error, because the declaration
    was not intended as an override. DMLC will also signal an error if
    a plain `param NAME = value;` or `param NAME default value;`
    declaration appears that does not override a pre-existing
    parameter.

    In some rare cases, you may need to declare a parameter without
    knowing if it's an override or a new parameter. In this case, one
    can accompany a `param NAME = value;` or `param NAME default
    value;` declaration with a `param NAME;` declaration in the same
    scope/rank. This marks that the parameter assignment may be either
    an override or a new parameter, and no error will be printed.

    Enabling the `explicit_param_decls` feature in a file only affects
    the parameter definitions specified in that file.
    '''
    short = "Require := syntax for defining new params"
    stable = True

@feature
class explicit_method_decls(ProvisionalFeature):
    '''
    This feature extends the DML syntax for methods to distinguish between an
    intent to declare a new method, and an intent to override an existing
    method/provide a definition for an abstract method.
    This distinction allows DML to capture misspelled parameter overrides as
    compile errors.

    This provisional feature introduces new syntax for the following purposes:
    * Abstract method declarations
      ```
      method m(...) [-> (...)] [throws];
      ```

      This declaration establishes a requirement that a method of that name and
      signature is defined in the same object as the abstract method
      declaration. This is similar to the existing abstract `shared` method
      declaration, but unlike abstract `shared` methods have no restrictions
      or semantic implications beyond that (except for the fact that it
      declares the method to exist.) In other words, it's semantically
      analagous to untyped abstract parameter declarations (`param p;`).

    * Simultaneously declaring and defining a new method
      ```
      method m(...) [-> (...)] [throws] :{ ... }
      method m(...) [-> (...)] [throws] :default { ... }
      ```

      DMLC rejects a declaration of this form if the method has already been
      declared, because this form signifies that the declaration was not
      intended as an override.

    `explicit_metod_decls` also changes the meaning of the traditional form
    of method definitions (e.g. `method m() {}` or `method m() default {}`)
    such that DMLC will reject them if the method has not been declared
    previously (either abstractly or with an overridable definition.)

    In some rare cases, you may need to declare a method without
    knowing if it's an override or a new declaration. In this case, one
    can accompany an overriding definition (e.g. `method m() {}`
    or `method() default {}`) with an abstract method declaration (e.g.
    `method m();`) in the same scope/rank. This marks that the method
    definition may either be for a previously declared method or a new method
    entirely, and no error will be printed.

    Enabling the `explicit_method_decls` feature in a file only affects
    the method definitions specified in that file; in other words, it will not
    require other files to use the `:{` syntax in order to declare novel
    methods.
    '''
    short = "Require :{ ... } syntax for defining new methods"
    stable = False


@feature
class simics_util_vect(ProvisionalFeature):
    '''<a id="simics_util_vect"/>
    This feature enables the `vect` type, based on the
    `VECT` macro from the Simics C API (`simics/util/vect.h`).

    This is a simple wrapping that behaves inconsistently in many
    ways, and we plan to eventually introduce a cleaner mechanism for
    vectors; the `simics_util_vect` is supported as an interim solution until we
    have that in place.

    The syntax is `BASETYPE vect`, e.g. `typedef int vect int_vect_t;`
    to define a type for vectors of the `int` type.

    Some caveats:

    * `vect` types typically need to be `typedef`:ed before they are
      used.  This is because `int vect` is blindly expanded into
      `VECT(int)` in C, which in turn expands into a `struct`
      definition, meaning that saying `VECT(int)` twice yields two
      incompatible types. This means, for instance, that `typeof` in
      DML doesn't work properly for `vect` types unless `typedef`:ed

    * Importing `"internal.dml"` exposes various C macros from
      `vect.h` to DML: `VINIT`, `VELEMSIZE`, `VRESIZE`,
      `VRESIZE_FREE`, `VADD`, `VREMOVE`, `VDELETE_ORDER`, `VINSERT`,
      `VSETLAST`, `VLEN`, `VVEC`, `VGROW`, `VSHRINK`, `VFREE`,
      `VTRUNCATE`, `VCLEAR`, and `VCOPY`.

    * DML natively supports indexing syntax, which is translated to
      `VGET` (or `VSET` for assignment). For instance:
      ```
      typedef int vect int_vect_t;
      method first_element(int_vect_t v) -> (int) {
          assert VLEN(v) > 0;
          return v[0];
      }
      ```

    Enabling the `simics_util_vect` feature in a file only affects
    the `vect` declarations in that file.

    When the `simics_util_vect` feature is disabled, usage of `vect` is an
    error if the [`vect_needs_provisional`
    breaking change](deprecations-auto.html#vect_needs_provisional) is enabled.
    '''
    short = "Allow vect syntax based on the VECT macro"
    stable = True
    dml12 = True


@feature
class explicit_object_extensions(ProvisionalFeature):
    '''<a id="explicit_object_extensions"/>

    This feature extends the DML syntax for object declarations to distinguish
    between an intent to introduce a new object to the model structure, and an
    intent to extend the definition of an existing object.

    The following form is introduced to mark the intent to extend an object:
    <pre>
    in <em>object-type</em> <em>name</em> <em>...</em> { <em>...</em> }
    </pre>
    E.g.
    <pre>
    in bank some_bank { ... }
    </pre>

    If this form is used while there is no other non-extension declaration of
    the named object, then DMLC will signal an error because the definition
    was not intended to introduce the object to the model structure.

    DMLC will also signal an error if there is more than one non-extension
    declaration of the object among the files enabling
    `explicit_object_extensions`.
    '''
    short = "Require `in` syntax for additional declarations of an object"
    stable = False

def parse_provisional(
        provs: list[("Site", str)]) -> dict[ProvisionalFeature, "Site"]:
    ret = {}
    for (site, name) in provs:
        if name in features:
            ret[features[name]] = site
        else:
            logging.report(messages.ENOPROV(
                site, name, ', '.join(sorted(features))))
    return ret
