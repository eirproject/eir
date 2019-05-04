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
  * `contlist`
  
### Pseudo-types
  
#### The `mapclass` pseudo-type
  
`mapclass` is a special "pseudo-type". When the compiler infers or gets informed that a map will always adhere to a given type signature at a location, it can promote the given `map` into its pseudo-type. When these types leave the locations where their type can be inferred, they get transparently demoted into their normal types.

A normal use-case for `mapclass` would be Elixirs structs.

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
   This can potentially result in a performance impact over `annotate_type`.

   The second argument is required to be known at compile-time.

## Message passing

### Receiving

TODO: Finish updating

`receive_start` the start of a receive structure, must jump to a 
block containing a single `receive_wait`.
No further `receive_start` or function termination is allowed
before control flow is passed through a `receive_finish` or exited
the structure through the timeout edge.

```

         [eir_intrinsics:receive_start()]
               |
               v
   ----------[eir_intrinsics:receive_wait()]<--------
   v                   |                            |
[Timeout           ]   |                            |
[Other control flow]   |                            |
                       v                            |
        -----[Match logic      ]---------------------
        |           |
        |           ----->[eir_intrinsics:receive_finish()]
        |                 [Other                          ]
        v
   [eir_intrinsics:receive_finish()]
   [Other                          ]

```

```
 #start:
   ...
   %receive_context = ReceiveStart(%timeout, #receive_loop)
 #receive_loop:
   ReceiveWait(%receive_context, #match_body, #timeout_body)
 #match_body:
   %message = ReceiveGetMessage()
   // Jump to #receive_loop if message does not match
   // Jump to #message_1_match if a message matches
   // Jump to #message_2_match if another message matches
 #timeout_body:
   ...
 #message_1_match:
   ReceiveFinish(%receive_context)
   ...
 #message_2_match:
   ReceiveFinish(%receive_context)
   ...
```

ReceiveStart,
 Central node of match loop of a receive structure.
 Must be the only op in its basic block.
 Jumps to edge 0 when a message has been received.
 Jumps to edge 1 when a timeout has occured.
ReceiveWait,
 This must be the first instruction on edge 0 from ReceiveWait.
 Peeks at the message in the mailbox, not removed unless ReceiveFinish
 is passed through on this iteration
ReceiveGetMessage,
 When jumped to from edge 0 from a ReceiveWait, control flow either
 needs to (eventually) return to ReceiveWait or needs to pass
 through ReceiveFinish on its way out. Returning while inside a receive
 structure is a hard error!
 This will actually consume the message from the mailbox.
ReceiveFinish,
