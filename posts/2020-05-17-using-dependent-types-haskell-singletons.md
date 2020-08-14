---
title: Using dependent types in Haskell with singletons
author: Evgeny Poberezkin
tags: haskell, coding
image: singletons1.jpg
---

Haskell has a very advanced type system, but it does not yet have [dependent types](https://en.wikipedia.org/wiki/Dependent_type). Yet, singleton types and [singletons library](https://hackage.haskell.org/package/singletons) provide a very good approximation of dependent types, that is shown on this diagram - the explanations to follow.

[![Singleton types](/images/singletons1.svg "source")](https://github.com/epoberezkin/poberezkin.com/tree/master/dot/singletons1.gv)

Justin Le wrote a fantastic [introduction to singletons library](https://blog.jle.im/entry/introduction-to-singletons-1.html) and dependent type programming with Haskell - if you did not use singletons library before, I highly recommended reading it.

I've written this post to visualise the Haskell "problem" that prevents it from having dependent types, and the "solution" that singleton types offer.


## The problem

Haskell types can only have other types as their parameters, but not values. [DataKinds extension](https://downloads.haskell.org/ghc/8.10.1/docs/html/users_guide/glasgow_exts.html#extension-DataKinds) automatically promotes all types to kinds and all constructors to types, in this way allowing other types depending on something that looks like values of another type. But there is no way to convert actual values to types, as promoted constructors are not connected to corresponding values and fully erased during compilation.

This is best illustrated with a simple example from Justin's post. Suppose we have type `Door` parametrised with the types of the kind `DoorState`:

```haskell
data DoorState = Opened | Closed

data Door (a :: DoorState) :: Type where
  MkDoor :: Door a
```

These two declarations create kinds, types and values that are shown on diagram 2:

[![Types and kinds with DataKinds](/images/singletons2.svg "source")](https://github.com/epoberezkin/poberezkin.com/tree/master/dot/singletons2.gv)

The `DoorState` type is automatically promoted to `DoorState` kind, and `Opened`/`Closed` values belonging to type `DoorState` are automatically promoted to `'Opened`/`'Closed` types belonging to kind `DoorState` - these types have no values in them (other than `undefined`).

As you can see, the values `Opened`/`Closed` are not connected to the associated promoted constructors `'Opened`/`'Closed` - at the moment Haskell provides no way to connect them directly.


## The solution

Singleton types allow creating this missing link between ordinary values and their associated promoted constructors. The idea here is that for each value we need to create a separate type that has only one value in it. In this way, if we connect values in these singleton types to ordinary values (via polymorphic functions), and the singleton types themselves to the promoted constructors (via type inference), we will have managed to link types to values, even though indirectly - so we can use the equivalent of the dependent types in Haskell.

It does sound like a lot of boilerplate to write, but luckily singletons library automates the whole process by using Template Haskell to generate it. To illustrate using the same example as above, but with the singleton types added:

```haskell
$(singletons [|
  data DoorState = Opened | Closed
  |])

data Door (a :: DoorState) :: Type where
  MkDoor :: Sing a -> Door a
```

The `singletons` splice adds two singleton types with the necessary framework, as shown on diagram 1 on top of the post.

Singleton types and values serve as an intermediary to link `Opened`/`Closed` values with `'Opened`/`'Closed` types that are used as parameters for the type `Door`, in this way making `Door` an equivalent of a dependent type.


## The future

Using singleton types for dependent type programming in Haskell is a bit of a workaround, rather than a solution to the problem. It either requires an additional code to write or depends on singletons library templates to generate all the necessary code and on namespace conventions that users have to be aware of.

There is an ongoing work by [Richard Eisenberg](https://richarde.dev/) to bring dependent types support to Haskell - it would be very exciting when it comes out.

Until then, we can use singletons library for type-driven development of complex systems, modelling their state transitions and all the relationships in "dependent" types.
