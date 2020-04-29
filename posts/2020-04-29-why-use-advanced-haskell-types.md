---
title: Why use advanced Haskell types?
author: Evgeny Poberezkin
tags: haskell, coding
---

Haskell type system has dramatically evolved, both with the language extensions and libraries. It can be a challenge to navigate this space. So why anything beyond basic types is needed?

Types in Haskell provide a way not only to type-check the code you write, but to design the whole system in types, before any code is written, and then use the types to guide the development. It is worth reading the book "[Type-driven development in Idris](https://www.manning.com/books/type-driven-development-with-idris)" by Edwin Brady about this approach.

Let's try to design types for some service accounts that can represent a user or an organisation.

The source code is available in [advanced-haskell-types](https://github.com/epoberezkin/advanced-haskell-types) repo.


## Approach #1 - basic types

While users and organisations are quite different, they may have many similarities (for example, have a look at [GitHub API](https://developer.github.com/v3/users/#get-a-single-user) that returns both users and orgs). 

Our users and orgs share some functionality and we would want to store them in one list. So we will make a single type to hold these accounts:

```haskell
-- shared information for both users and organisations
data AInfo = AInfo
              { name :: Text
              , displayName :: Text }

data Account = User AInfo | Org AInfo Members
type Members = [Account]
```

We've already met the first problem with this approach - members of the organisation should be users, but `Members` type allows both users and organisations - we will have to manage it in code.

Here are some functions for these accounts:

```haskell
-- all shared functions should work with both users and orgs
accountName :: Account -> Text
accountName (User info) = name info
accountName (Org info _) = name info

-- but some functions may only work with orgs,
-- so we will have to use Maybe type to return results
orgMembers :: Account -> Maybe Members
orgMembers (User _) = Nothing
orgMembers (Org _ ms) = Just ms
```

It's easy to put accounts in one list and process this list, as they have the same type, but we will have to check the result when we use a function intended only for organisations:

```haskell
main :: IO ()
main = do
  let user = User (AInfo "john" "John Doe")
      org = Org (AInfo "team" "John's team") [user]
      accounts = [user, org]
  print $ map accountName accounts
  case orgMembers org of
    Just ms -> print $ map accountName ms
    Nothing -> return ()
```

Could we design better types for our scenario to avoid the need to check the result of functions for orgs? It would also be good to prevent organisations being added as members at a type level, but without losing the ability to process both users and organisation in a single list.


## Approach #2 - existential quantification

Let's try to create two different types to store users and organisations, to avoid the problems we had:

```haskell
data User = User AInfo -- we could have made it newtype
data Org = Org AInfo Members
type Members = [User]
```

This is better, organisation members can be only users now.

We cannot have one function working on two different types, but we can define a type class and make `User` and `Org` types its instances:

```haskell
class Acc a where
  accountName :: a -> Text
instance Acc User where
  accountName (User info) = name info
instance Acc Org where
  accountName (Org info _) = name info
```

And we can also have a function that works only with organisations, without using Maybe:

```haskell
orgMembers :: Org -> Members
orgMembers (Org _ ms) = ms
```

The problem that we now have is that `User` and `Org` are two different types, and we cannot put them into one list.

Haskell GHC compiler (since v6.8.1 released in 2007) has the extension [ExistentialQuantification](https://downloads.haskell.org/ghc/8.8.3/docs/html/users_guide/glasgow_exts.html#extension-ExistentialQuantification) that allows to create a type that can wrap values of multiple types, and the members of this wrapper type can be put in the list:

```haskell
data A = forall a. Acc a => A a
type Accounts = [A]
```

In our case we limit the allowed types to the instances of `Acc` type class, so we can use the list elements with our type class functions, but it is not the only shared criteria the types can have and still be useful - see another example in ExistentialQuantification docs.

Now we can put wrapped users and orgs into the same list and process them:

```haskell
main :: IO ()
main = do
  let user = User (AInfo "john" "John Doe")
      org = Org (AInfo "team" "John's team") [user]
      accounts = [A user, A org] -- we need to wrap users and orgs

  -- we do not need to check type of the org now
  print . map accountName $ orgMembers org

  -- the only way to unwrap an existential wrapper is with pattern matching
  print $ map (\(A acc) -> accountName acc) accounts
```

There are two downsides of this approach:

1. We cannot limit which types can be instances of `Acc` type class. While in some more general cases this unlimited extensibility can be helpful, if we want to control which types can be used as `Acc` we need some other approach.
2. We have to write some boiler plate code - we really just wanted one type with some additional flexbility in it, and not two different types and type class to represent it.

Let's try to solve these problems.


<h2>Approach #3 - data families and data kinds</h2>

We will try to limit the types that can be instances of `Acc` type class. Types in Haskell have kinds, and in most cases the kind of a type is determined by the number of type parameters.

From v7.4.1 released in 2012 Haskell makes all your types also kinds using [DataKinds](https://downloads.haskell.org/ghc/8.8.3/docs/html/users_guide/glasgow_exts.html#extension-DataKinds) extension - we will use it to limit the types that can be used as `Acc`. We will also use extensions [TypeFamilies](https://downloads.haskell.org/ghc/8.8.3/docs/html/users_guide/glasgow_exts.html#extension-TypeFamilies) and ExistentialQuantification we already used to have types of user and organisation related to each other and to put them into the same list.

Let's define a simple type that has the list of allowed account types:

```haskell
data AType = AUser | AOrg
```

With DataKinds extension each *type* (in this case `AType`) automatically becomes a *kind* that can be used to define and restrict other types. We will use this kind to create `Account` data family:

```haskell
data family Account (a :: AType)
data instance Account 'AUser = User AInfo
data instance Account 'AOrg = Org AInfo Members

type Members = [Account 'AUser] -- organisation members can be only users
```

`'AUser` and `'AOrg` is a special syntax that allows to use *constructors of type* `AType` as *types of kind* `AType`. `User` and `Org` are just normal constructors of types `Account 'AUser` and `Account 'AOrg`.

To define shared functionality we would still have to use a type class, because while `Account 'AUser` and `Account 'AOrg` are now related (as family members), they are still two separate types:

```haskell
class Acc (a :: AType) where
  accountName :: Account a -> Text
instance Acc AUser where
  accountName (User info) = name info
instance Acc AOrg where
  accountName (Org info _) = name info

orgMembers :: Account 'AOrg -> Members
orgMembers (Org _ ms) = ms
```

Please note that in this case we made types belonging to our custom-made kind `AType` instances of typeclass `Acc`, rather than account types. We can neither extend data family `Account a` nor type class `Acc` without extending our kind `AType`.

We still need to create an existential wrapper type to put users and orgs in the same list:

```haskell
data A = forall a. Acc a => A (Account a)
type Accounts = [A]
```

We can use exactly the same code to process users and orgs as in approach #2. 

We have managed to restrict the types of accounts by defining a kind, but do we really need 2 different types and a typeclass, or is there a way to create just one, a more advanced type?


## Approach #4 - GADTs

We can achive the same flexibility using a generalised algebraic data type - support for such types is enabled with [GADTs](https://downloads.haskell.org/ghc/8.8.3/docs/html/users_guide/glasgow_exts.html#extension-GADTs) extension.

```haskell
data AType = AUser | AOrg -- we still need DataKinds extension

-- (a :: AType) here requires KindSignatures extension
data Account (a :: AType) where
  User :: AInfo -> Account 'AUser
  Org :: AInfo -> Members -> Account 'AOrg

type Members = [Account 'AUser] -- organisation members can be only users
```

Now that we have one parametrised type, we can define functions on this general type without a type class, using just pattern matching:

```haskell
accountName :: Account a -> Text
accountName (User info) = name info
accountName (Org info _) = name info
```

We can also define functions on specific types:

```
orgMembers :: Account 'AOrg -> Members
orgMembers (Org _ ms) = ms
```

We still need an existential wrapper to put a general type in a list, but it is a bit simpler now, as we do not need a type class:

```haskell
data A = forall a. A (Account a)
type Accounts = [A]
```

GADTs extension implies ExistentialQuantification, so we do not need to enable it separately.

The same code as above can be used to process the list of users and orgs.


## Summary

Beyond basic types, we looked at three options that allow to define different entities with shared behaviours and to manage them in the same data structure:

1. Type classes - the most extensible option, that allows to define the behavior independently of its implementation. The classic scenario for type classes is some kind of widgets/shapes/etc.
2. Data families restricted with data kinds. The advantage of such data families is that you can define its members in different parts of your code, but you have control of the list of allowed members in a single location using a custom data kind.
3. GADTs - they provide a much bigger flexibility in defining your types, in many cases without the need for type classes. They allow different constructors of one parametrised type (of a higher kind) to return specific types (of basic kind).

Haskell offers many different approaches to design your whole system, not just its data, in types. This post is just a small sample of what is possible with advanced Haskell types.
