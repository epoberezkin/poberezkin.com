---
title: "Modelling finite state machines with dependent types in Haskell: Part 1"
author: Evgeny Poberezkin
tags: haskell, coding
github: epoberezkin/finite-state-machine-post
---

Haskell has a very advanced type system, but it does not yet have [dependent types](https://en.wikipedia.org/wiki/Dependent_type). Yet, singleton types (types that have only one value) and [singletons](https://hackage.haskell.org/package/singletons) library allow to have a very good approximation of dependent types.

Justin Le has written a fantastic [introduction to singletons](https://blog.jle.im/entry/introduction-to-singletons-1.html) library and dependent type programming with Haskell - if you did not use singletons library before, it is strongly recommended that you read it before reading the second part of this post.

I will only briefly summarise the problem that Haskell has, preventing it from having dependent types, and the solution that singleton types give to this problem. Haskell types can only have other types as their parameters, but not values. DataKinds extension allow promoting any type to kind, in this way allowing other types depending on something what looks like values. But there is no easy way to convert actual values to types, as types are unrelated to values and removed at compile time.

Singleton types allow creating this missing link, as shown on this diagram:

For example, if we have a type:

```haskell
data DoorState = Opened | Closed
```

then `DoorState` type is also automatically promoted to `DoorState` kind, and `Opened`/`Closed` values belonging to type `DoorState` are automatically promoted to `'Opened`/`'Closed` types (that have no values in them) belonging to kind `DoorState`.

Singleton types and values allow linking `Opened`/`Closed` values with `'Opened`/`'Closed` types that can be used as other type parameters, in this way enabling dependent type programming.
