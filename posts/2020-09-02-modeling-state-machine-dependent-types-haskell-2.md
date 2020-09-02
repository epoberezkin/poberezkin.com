---
title: "Dependent types to code are what static types to data"
author: Evgeny Poberezkin
tags: haskell, executable, coding
github: epoberezkin/elevator-state-machine
---

## Modeling state machines with dependent types in Haskell: Part 2

This post is "literate" haskell (thanks to [markdown-unlit](https://github.com/sol/markdown-unlit)), it can be run on GHC 8.8.3 with `stack run atm`.

You may want to read first "Modeling state machines with dependent types in Haskell: Part 1", - this post builds upon it and answers the questions raised there.

If you used the singletons library and aware of the state of dependent types support in Haskell (that is, "no support", but good workarounds allowing to emulate them), it shoould be ok to read independently.

## Why?

In the first part I wrote the motivation to use dependent types, but having read a post by Alexis King "Parse don't validate" about static type systems, I had an anology to add to the motivation that is in this post title:

dependent types to code are what static types to data

In the same way as static type systems ensure data validity by construction and prevent writing the code that expects or creates invalid data, dependent types ensure code validity on the code flow and logic level - you can restrict the valid state transitions and empose any other restrictions on the code flow.

## Questions to cover

The part 1 left open these questions:

1. How can we write code with dependently typed actions in a more conventional way with `do` notation.
2. How to make actions return results and to have state changes depend on these results.

The ATM (cash machine) example for the second question is borrowed from the excellent book by Edwin Brady: "Type driven development with Idris". Even if you are not interested in Idris, I highly recommend this book, as it it likely to give you some ideas for writing code in any language - particularly in Haskell, as Idris is syntactically very similar, but with the types as first class citizens and full dependent types support.

# Dependent types ergonomics

Let's deal with the first question first - how to write dependently typed code so it looks, well, like code.

Let's first consider a super simple not-so-abstract example of some state transitions:

```haskell ignore
$(singletons [d|
  data State = On | Off
    deriving (Show, Read, Eq)
  |])

data Command (s: State) (s': State) a :: Type where
  TurnOn :: Command Off On a
  TurnOff :: Command On Off a
  (:>>=) :: Command s1 s2 a -> (a -> Command s2 s3 b)  -> Command s1 s3 b
```

It defines Command type with two constructors parameterized on the result type that change the type-level state fron On to Off and back. The last constructor allows to chain them. It very much looks like monadic bind, but it has arguments that are parameterized on the state.

Because of these parameterization, we can define functor instance of this type, but we can neither define Applicative nor Monad. Relatively well covered concept that allows to abstract what is needed here is called parameterized monads - I will not go the into details here, please review it in several other places:

- [Parameterized monads by Oleg Kiselev](http://okmij.org/ftp/Computation/monads.html#param-monad)
- [Type driven]() T. Maguire?
- Edward Kmett library for indexed monads

Further, rather than defining the instances of indexed applicative and monad by hand, it is possible to use indexed freer monads - an evolution of Oleg's [freer monads](http://okmij.org/ftp/Haskell/extensible/more.pdf) generalized to indexed monads. It has also been covered [here](http://okmij.org/ftp/Haskell/extensible/index.html#extext)

These are all fascinating subjects that probably deserve a separate tutorial-style post, please let me know if you have seen it or if you wrote it. I may write about it one day, but no promises here.

Long story short, using indexed freer monads you can get functor, indexed applicative (XApplicative) and indexed monad (XMonad) instances of a derived type with just a few lines of code without the need to define any monadic operations:

```haskell ignore
import Control.XFreer

data Command' (s: State) (s': State) res :: Type where
  TurnOn :: a -> Command' Off On b
  TurnOff :: Command' On Off a

type Cmd = XFree Command'

turnOn :: a -> Cmd Off On b
turnOn = xfree . TurnOn

turnOff :: a -> Cmd On Off a
turnOff = xfree TurnOff
```

XApplicative and XMonad classes and XFree data type are defined in [freer-indexed]() library.

A modified Command' type does not need to define its own bind constructor, instead it can use XMonad and XApplicative instances of `XFree Command'` type. Cmd type created with XFree is the easiest to use with functions `turnOn` and `turnOff`

Now, having briefly covered the required abstractions, to syntax. To use these indexed monads with `do` GHC has an extension RebindableSyntax that, regardless its bad reputation as it allows redefining too many things, is quite useful here - it allows to redefine `>>=` and `>>` to be used with indexed monads, and once it is done they can be used with `do`. The downside is that you cannot combine indexed and normal monads in the same module. It should become better with the new extension QualifiedDo that is expected ... when?

With all these great things we can now write:

```
toggle :: Int -> Cmd Off Off Int
toggle n = do
  m <- turnOn n
  turnOff m
```

and it won't compile if state transitions don't match.

## Dependent types for real

Onwards and upwards from here. Having all the necessary indexed monadic machinery in place, we can now get to the main issue at hand: how to make state changes expressed on the type level dependent on the data-level results of the previous actions. Without being able to do so we do not really have dependent types.

As promised, the ATM example from the book by Edwin Brady will be implemented here in Haskell. You can find the original Idris code here - you will see how similar Haskell code is, so even though dependent types require some workarounds, their ergonomics in Haskell is quite decent.

The possible state transitions of our ATM are shown on the diagram below - it is reproduced from the book with the kind permission of the publisher - Manning Publications.

...

To have the state of the ATM available both in types, and also at run-time, we will use singletons library and we will need to enable some GHC extensions:

```haskell
{-# LANGUAGE DataKinds, FlexibleInstances, GADTs, InstanceSigs,
  LambdaCase, PolyKinds, ScopedTypeVariables, TemplateHaskell,
  TypeApplications, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -Werror=incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

import Control.Monad (void, forever)
import Control.XFreer
import Control.XMonad
import Data.Kind
import Data.Singletons ()
import Data.Singletons.TH
```

We will define the state of the ATM in types, and also create singleton types for them:

```haskell
$( singletons
     [d|
       data ATMState = Ready | CardInserted | Session
         deriving (Show)
       |]
 )

type family HasCard (s :: ATMState) :: Constraint where
  HasCard CardInserted = ()
  HasCard Session = ()
```

In addition to the ATM state, the snippet above defines a constraint type family to simplify type level-check whether the card is inserted - it will be needed when we define the type for ATM commands.

We will also need the types and singletons to define command result on which our state transition depends and the dependency as well.

```haskell
$( singletons
     [d|
       data PINCheck = CorrectPIN | WrongPIN
         deriving (Show)

       pinCheckToState :: PINCheck -> ATMState
       pinCheckToState = \case
         CorrectPIN -> Session
         WrongPIN -> CardInserted
       |]
 )
```

That expresses the state transition dependency that we need - if the correct PIN provided, the state can change to `Session` that allows dispensing money. But if the PIN is incorrect, the ATM will remain in `CardInserted` state and getting money from it will be prohibited on the type level.

Same as in the example in part 1, the function `pinCheckToState` above creates type family and function on singletons in addition to the defined function itself - it is explained in more details in part 1.

Now we can define the type for all allowed ATM commands:

```haskell
type PIN = String

type family HasCard (s :: ATMState) :: Constraint where
  HasCard CardInserted = ()
  HasCard Session = ()

data ATMCommand (s :: ATMState) (s' :: ATMState) a :: Type where
  InsertCard :: ATMCommand Ready CardInserted ()
  EjectCard :: HasCard s => ATMCommand s Ready ()
  GetPIN :: ATMCommand CardInserted CardInserted PIN
  CheckPIN :: PIN -> ATMCommand CardInserted CardInserted PINCheck
  StartSession :: SPINCheck p -> ATMCommand CardInserted (PinCheckToState p) ()
  GetAmount :: HasCard s => ATMCommand s s Int
  Dispense :: Int -> ATMCommand Session Session ()
  Message :: String -> ATMCommand s s ()
```

You can check with the state transition diagram that this type correctly expresses only allowed state transitions and for `StartSession` command the resulting state depends on whether the PIN was correct or not.

Unlike we did with the elevator from part 1 we will not write ATM code with these constructors, neither we defined a constructor to bind these commands in sequence. Instead we will use indexed free monad briefly covered above to create our building blocks for ATM programm:

```haskell
type ATMCmd = XFree ATMCommand

insertCard :: ATMCmd Ready CardInserted ()
insertCard = xfree InsertCard

ejectCard :: HasCard s => ATMCmd s Ready ()
ejectCard = xfree EjectCard

getPIN :: ATMCmd CardInserted CardInserted PIN
getPIN = xfree GetPIN

checkPIN :: PIN -> ATMCmd CardInserted CardInserted PINCheck
checkPIN = xfree . CheckPIN

startSession :: SPINCheck p -> ATMCmd CardInserted (PinCheckToState p) ()
startSession = xfree . StartSession

getAmount :: HasCard s => ATMCmd s s Int
getAmount = xfree GetAmount

dispense :: Int -> ATMCmd Session Session ()
dispense = xfree . Dispense

message :: String -> ATMCmd s s ()
message = xfree . Message
```

That's a bit of the boilerplate - you would have to do the same with any algebraic effect system (or use template haskell in case of Polysemy).

Now, using RebindableSyntax, we can write code using `do`.

```haskell ignore
atm :: ATMCmd Ready Ready ()
atm = do
  insertCard
  message "Hello"
  pin <- getPIN
  pinOK <- checkPIN pin
  case pinOK of
    FromSing ok -> do
      startSession ok
      case ok of
        SCorrectPIN -> do
          amount <- getAmount
          dispense amount -- this would fail to compile in SWrongPIN branch
          ejectCard
          message "Remove card and cash"
        SWrongPIN -> do
          message "Incorrect PIN"
          ejectCard
```

Even though what we have above is a data structure yet to be interpreted, it very much looks like code. The type-level state transition depends on whether the PIN is correct, and if you try to trick our ATM into giving you 100 in case you don't have PIN by adding `dispense 100` into SWrongPIN branch, the code will not compile - exactly as we wanted.

The main difference with Idris code from the book, is that in case of Haskell the transition between PIN check result on the data level (`pinOK`) to the PIN check result on the type level (`ok`) has to be explicit and we need a separate command to perform the state transition based on PIN check result.

Here we cheated a little bit. The above is not the actual code that runs if you execute this post - as I wrote, RebindableSyntax does not allow combining indexed and normal monads in the same file, and we will need an interpreter for this code that will use normal monads.

Without `do` the code above can be written in this way:

```haskell
atm' :: ATMCmd Ready Ready ()
atm' =
  insertCard
    >>: message "Hello"
    >>: getPIN
    >>=: checkPIN
    >>=: \(FromSing ok) ->
      startSession ok
        >>: case ok of
          SCorrectPIN ->
            getAmount
              >>=: dispense
              >>: ejectCard
              >>: message "Remove card and cash"
          SWrongPIN ->
            message "Incorrect PIN"
              >>: ejectCard
```

`>>=:` and `>>:` operations are provided by [freer-indexed]() package - it provides all other monadic operations similar to those defined in Control.Monad, but for indexed monads, e.g. `>=>:`, `xjoin` and others - just add a semicolon to a monadic operation or prefix a monadic function from `Control.Monad` with `x`.

To execute ATM program we now need an interpreter. In our case it will be just a console demo:

```haskell
runATMCmd :: ATMCommand s s' a -> IO a
runATMCmd InsertCard = putStrLn "Insert card (press enter)" >> void getLine
runATMCmd EjectCard = putStrLn "Card ejected"
runATMCmd GetPIN = putStrLn "Enter pin:" >> getLine
runATMCmd (CheckPIN pin) =
  if pin == "1234"
    then return CorrectPIN
    else return WrongPIN
runATMCmd (StartSession _) = return ()
runATMCmd GetAmount = read <$> (putStrLn "Enter amount:" >> getLine) :: IO Int
runATMCmd (Dispense cash) = putStrLn $ "Here is " ++ show cash
runATMCmd (Message msg) = putStrLn msg
```

We also need to interpret Pure and Bind constructors of `XFree`:

```haskell
runATM :: ATMCmd s s' a -> IO a
runATM (Pure x) = return x
runATM (Bind c f) = runATMCmd c >>= \x -> runATM (f x)
```

## Let's run it!

The code to run our ATM program is very simple:

```haskell
main :: IO ()
main = runATM (forever atm')
```

Why were we able to use `forever` from `Control.Monad` with our ATM program? `atm'` is an indexed Monad, and forever is defined for normal Monads. The reason we could do it is that `freer-indexed` defines normal `Applicative` and `Monad` instances for indexed monads when starting and final states are the same. So even though our ATM program uses indexed monads internally, it can be embedded into a bigger codebase that uses normal monads.

This is very important - dependent types and indexed monads is not all or nothing. You may choose to use dependent types only for some small critical part of your code where you want additional type level safety, and use normal monads in the rest of your code.

This is it! You can run the code right from this post by cloning the [site repo](https://github.com/epoberezkin/poberezkin.com) and executing `stack run atm`. The source code without the text is available [here](https://github.com/epoberezkin/atm-state-machine).
