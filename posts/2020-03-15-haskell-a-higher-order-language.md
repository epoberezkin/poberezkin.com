---
title: Haskell — a higher order language
author: Evgeny Poberezkin
tags: haskell, executable, coding
ref: https://medium.com/@epoberezkin/haskell-a-higher-order-language-ade461d453c7
---

<img src="/images/haskell.png" width="40%" style="float: left; margin: 20px 20px 10px 0;" />

The thesis here is that Haskell is not just one of many functional programming languages — it is a different, more advanced programming paradigm.

Haskell is indeed a functional language, but calling Haskell “a functional language” is like calling a skyscraper “a dwelling” — while technically correct, it does not describe how the latter is much more than just a place to live.

What defines a programming language? From the point of view of category theory, there are two major components of each programming language: data types and transformations between them — in category theory terminology, “objects” and “morphisms”.

**The first claim** here (that must be challenged) is that all programming languages but Haskell (and more recent Idris) are based on “morphisms” — code, procedures or functions that transform and manipulate the data. Data types in these languages play the secondary role — to ensure validity and to improve predictability of the “morphisms”.

Haskell, being a functional language, counterintuitively, is not based on functions — it is based on types, or “objects” in category theory terminology. Types ensuring the validity of transformations almost seems secondary in Haskell (however useful), while the primary purpose of types is to formally describe the system model and the relationship between the elements of the system (including functions that also have types).

But a bigger distinction between Haskell and other languages is in the nature of the language semantics. **The second claim** (that also must be challenged) is that while other languages have semantics tightly coupled with the syntax — the meaning of the code is defined by its grammar, Haskell semantics is defined by the combination of code and context (e.g., created by the types that belong to Monad class). In this way, Haskell is much closer than other programming languages to the natural human languages that also have semantics defined by the combination of grammar and context (see interpretive and generative semantics of human languages).

For example, a simple `sequence` function that is defined as:

```haskell-ignore
sequence :: Monad m => [m a] -> m [a]
sequence ms = foldr k (return []) ms
            where
              k m m' = do { x <- m; xs <- m'; return (x:xs) }
```

can mean different things depending on the context that is defined by `m`.

Applied to IO it can mean performing IO actions in sequence:

```haskell-ignore
sequence [getLine, getLine]
```

returns a single IO action that resolves into the list of 2 strings.

Applied to the list of instances of `Maybe` type, it would check that all of them contain some value and either return `Just` list of these value or Nothing if any of them is `Nothing`:

```haskell-ignore
sequence [Just 1, Just 2, Just 3] = Just [1, 2, 3]
sequence [Just 1, Just 2, Nothing] = Nothing
```

Applied to the list of 2 lists, it will perform indeterminate computation and return all possible permutations of list items where the first item comes from the first list, and the second — from the second list:

```haskell-ignore
sequence [[1,2],[3,4]] = [[1,3],[1,4],[2,3],[2,4]]
sequence [[1,2],[3,4], []] = [] -- [] is "undefined" in this context
```

Context-dependent semantics of Haskell code makes Haskell more difficult to learn. As the same syntax can mean many different things, achieving fluency requires more effort than with other languages. But it also makes Haskell infinitely more expressive than any other language — you can implement any new semantics you want by adding the new context to the same code. Therefore, while Haskell requires more investment from you than other programming languages, the return on this investment is infinitely higher.

Some Haskell books (e.g. [LYAH](http://learnyouahaskell.com/)) and lectures (e.g [Penn course](https://www.seas.upenn.edu/~cis194/fall16/index.html)) do not capture this fundamental distinction well enough. Instead, they focus on the functional nature of Haskell, and present Monad as almost some work-around to allow using pure functions for context-aware computations (IO, State, indeterminism, etc.). Unfortunately, it creates a barrier to entry for the new developers, because when people are asked to make a larger than usual investment to learn yet one more functional programming language with quirky syntax, this investment is difficult to justify without understanding first that Haskell is a more powerful programming paradigm. How many people abandoned Haskell before grasping its power?

A good book that explains how Haskell is a higher order language is [Haskell](https://en.wikibooks.org/wiki/Haskell) in wiki-books. Once you get over “[Understanding monads](https://en.wikibooks.org/wiki/Haskell/Understanding_monads)” section, the Haskell advantage should become apparent.

If you want to see some relatively simple magic you can do with Haskell, watch the talk by [Paweł Szulc](https://github.com/EncodePanda) at Lambda World’19, particularly where he [talks about Servant](https://www.youtube.com/watch?v=idU7GdlfP9Q&feature=youtu.be&t=625) — the library to create REST APIs in Haskell. Before you write a single line of implementation code, you can get the whole API definition from a single type definition (I am replacing alpacas from Paweł’s farm with users here):

```haskell
{-# LANGUAGE DataKinds, DeriveAnyClass, DeriveGeneric, TypeOperators #-}
import Servant
import Servant.Server
import Network.Wai.Handler.Warp (run)
import Data.Aeson
import GHC.Generics
import qualified Data.Map as M

data User = User {
  name  :: String
} deriving (Generic, ToJSON, FromJSON)

type UserAPI =
       "user" :> Get '[JSON] (M.Map Int User)
  :<|> "user" :> Capture "userId" Int
              :> Get '[JSON] User
  :<|> "user" :> Capture "userId" Int
              :> ReqBody '[JSON] User
              :> PutCreated '[JSON] NoContent

userApi :: Proxy UserAPI
userApi = Proxy

-- UserAPI type defines this API:
-- GET /user   - Response: {"1":{"name":"jane"}}, 200
-- GET /user/1 - Response: {"name":"jane"}, 200
-- PUT /user/2 Body: {"name":"John D."} - Response: NoContent, 201
```

And before you even start implementing this API you can get client functions to call this API with a few lines of code:

```haskell-ignore
-- import Servant.Client

getAll :<|> getUser :<|> putUser = client userApi

-- client functions types:
getAll :: ClientM (M.Map Int User)
getUser :: Int -> ClientM User
putUser :: Int -> User -> ClientM User
```

With just a few annotations you can generate API docs from UserAPI type:

```haskell-ignore
instance ToCapture (Capture "userId" Int) where
  toCapture _ =
    DocCapture "userId"
               "Id that uniquely identifies a user in the system"

instance ToSample (User) where
  toSamples _ = singleSample $ User "Jane"

instance ToSample (M.Map Int User) where
  toSamples _ = singleSample $ M.singleton 1 (User "Jane")

apiDocs :: API
apiDocs = docs userApi

main :: IO ()
main = (writeFile "docs.md" . markdown) apiDocs
```

To run this server you just need to implement it, the mock implementation is very simple, but the Haskell type system ensures that the type of implementation is correct (`Server UserAPI` that is based on `UserAPI` type):

```haskell
dummy = User "Jane"

fetchAll :: Monad m => m (M.Map Int User)
fetchAll = pure $ M.singleton 1 dummy

fetch :: Monad m => Int -> m User
fetch id = pure dummy

insert :: Monad m => Int -> User -> m NoContent
insert id user = pure NoContent

server :: Server UserAPI
server = fetchAll :<|> fetch :<|> insert

app :: Application
app = serve userApi server

main :: IO ()
main = do
  putStrLn "http://localhost:8080/user"
  run 8080 app
```

The above does feel like magic! You can run this server right from this post with `stack run users-api`.

Morphism-based programming languages (i.e., all other languages) force programmers to model the whole system outside of the code — using SQL schema, JSON schema, diagrams, etc. Type-based languages (Haskell and Idris) allow for type-driven development, when the whole system can be modelled top-down with algebraic data types, rather than bottom-up with functions as in other languages.

Haskell being type-based language with context-dependent semantics is a higher order language that is almost one of a kind — there seems to be no other mature programming language that allows the same level of expressiveness as Haskell does.
