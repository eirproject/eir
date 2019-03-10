# Type system

There are two parallel type systems:
 * Pessimistic types
 * Optimistic types

Pessimistic types are what we can statically infer from the code itself. Using only this type system, the code would behave like it would in normal Erlang. As an example, we know that `erlang:+/2` returns a number, so this type check can be elided in code that follows it. Pessimistic types should only infer details from with the function itself.

Optimistic types are informed by annotations from the user. If the user annotates that a function `foo/1` should only receive a number as an input, we can optimize the code for this case. The compiler is allowed to optionally insert type assertions for supplied optimistic type hints.

## Type tree

It should be noted that everything here is under construction and subject to change. This especially applies to the pseudo-types (`mapclass`, `tupclass` and `contlist`).

* `any`
  * `atom`
    * `boolean`
  * `number`
    * `integer`
      * `smallint`
      * `bigint`
    * `float`
  * `pid`
  * `reference`
  * `map`
  * `list`
  * `tuple`
    * `n-tuple`
  * `nil`
  * `mapclass`
  * `tupclass`
  * `contlist`
  
### Pseudo-types
  
#### The `mapclass` and `tupclass` pseudo-types
  
`mapclass` and `tupclass` are special "pseudo-types". When the compiler infers or gets informed that a map or a tuple will always adhere to a given type signature at a location, it can promote the given `map` or `tuple` into there pseudo-types. When these types leave the locations where their type can be inferred, they get transparently demoted into their normal types.

A normal use-case for `mapclass` would be Elixirs structs, and `tupclass` would be Erlangs records.

From the host language, these look completely identical to their non-pseudo counterparts.

#### The `contlist` pseudo-type

`contlist` stands for continuous list. When a `list` gets promoted into this pseudo-type, it will not be stored as a linked list, but rather as a (possibly chunked and linked?) array.

Not very well thought out yet, but it is to be expected that the cache locality of storing a list in a continuous block of memory could improve performance in some cases. I have a sneaking unconfirmed suspicion that this could be especially useful in places where a list is constructed recursively and then reversed.

### Type modifiers



# Intrinsics
The module that contains all these intrinsics functions is `eir_intrinsics`. All of these functions are directly handled by the compiler. Defining a `eir_intrinsics` module could be done for compatibility with the BEAM, but it will always be overridden in this project.

It should be noted that no intrinsics are dynamically callable.

## Types

 * `type_tag(T)`
   Returns the type tag of the type in the form of an atom.
   * `atom`
   * `float`
   * `smallint`
   * `bigint`
   
 * `annotate_type(Variable, EirTypeSpec)`
   Inserts an annotation that the given variable should be the given type spec.
   The compiler might insert assertions if it decides so, but can also choose not to.
   When annotating types, this should be used most often.
   
   The second argument is required to be known at compile-time.

 * `assert_type(Variable, EirTypeSpec)`
   Same as above, but the compiler is required to insert assertions.
   This can potentially result in a performance impact.

   The second argument is required to be known at compile-time.
