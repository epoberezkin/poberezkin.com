---
title: "Modeling state machines with dependent types in Haskell: Part 1"
author: Evgeny Poberezkin
tags: haskell, executable, coding
github: epoberezkin/elevator-state-machine
reddit: r/haskell/comments/hkg9q8/modeling_state_machines_with_dependent_types_in/
image: elevator.jpg
---

This post is "literate" haskell (thanks to [markdown-unlit](https://github.com/sol/markdown-unlit)), it can be run using GHC 8.8.3 with `stack run elevator`.

## Why?

The reason to use types to model state transitions is to guarantee the correctness of [state machine](https://en.wikipedia.org/wiki/Finite-state_machine) implementation by the way it is constructed, so that invalid implementations fail to compile.

When we model a state machine we usually create some diagram or use some meta-language to show allowed state transitions. The code implementing the logic of the state machine is disconnected from this diagram, and it can be large enough to make it impossible or very difficult to validate that the implementation only supports allowed state transitions.

We could use some DSL or library to raise the level of abstraction so that allowed state transitions are more visible in code. It mitigates the problem, but it creates additional complexity and dependency on the component (DSL interpreter or state-machine library) that also can have implementation mistakes.

Modeling state transition in types offers a simple alternative to ensure that only allowed state transitions can be implemented - the code that attempts to perform the state transition that is not allowed will not compile.

## How?

Using parametrized types it is possible to express state machine transitions where state can be part of the transition type. Further, using [dependent types](https://en.wikipedia.org/wiki/Dependent_type) it is possible to make transitions depend on some run time states. Haskell does not support dependent types directly, but it is possible to have an equivalent of dependent types with singleton types and [singletons](https://hackage.haskell.org/package/singletons) - see [this post](2020-05-17-using-dependent-types-haskell-singletons.html).

## Modeling elevator (aka lift) states

We will model state transition of a simplified [elevator](https://en.wikipedia.org/wiki/Elevator) state changes.

The state of the elevator has 3 dimensions: its door can be opened or closed, it can be stopped or going up or down, and it can be on different floors. Not all combinations are allowed though - the elevator must not move with the opened door and it must not open the door while moving. Elevator states and allowed actions and states are shown on the diagram.

[![Elevator states](/images/elevator.svg "source")](https://github.com/epoberezkin/poberezkin.com/tree/master/dot/elevator.gv)

In addition to that elevator cannot go below than the ground floor (type-level natural number `0` will be used for it).

To have the state of the elevator available both in types, and also at run-time we will use singletons library and we will need to enable some GHC extensions:

```haskell
{-# LANGUAGE ConstraintKinds, DataKinds, DeriveAnyClass, EmptyCase,
  FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs,
  InstanceSigs, LambdaCase, PartialTypeSignatures, PolyKinds, RankNTypes,
  ScopedTypeVariables, StandaloneDeriving, TemplateHaskell,
  TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

import Data.Kind
import Data.Singletons
import Data.Singletons.Prelude
import Data.Singletons.TH
import Data.Singletons.TypeLits
import System.IO
import System.IO.Interact
```

We will define the state of the elevator door and movement in types, and also create singleton types for them:

```haskell
$(singletons [d|
  data DoorState = Opened | Closed
    deriving (Show, Read, Eq)

  data MoveState = Stopped | Up | Down
    deriving (Show, Read, Eq)
  |])
```

[This post](2020-05-17-using-dependent-types-haskell-singletons.html) shows what singletons library does when you wrap type definitions like that. If you did not use singletons before, you can read an [introduction to singletons library](https://blog.jle.im/entry/introduction-to-singletons-1.html) by Justin Le.

We also need to define dependent state changes both in types and at run-time. For that we will need type families and functions on singletons that can also be created with singletons library:

```haskell
$(singletonsOnly [d|
  nextFloor :: MoveState -> Nat -> Nat
  nextFloor Stopped f = f
  nextFloor Up f = f + 1
  nextFloor Down f =
    if f > 0
      then f - 1
      else 0

  nextMoveState :: MoveState -> Nat -> MoveState
  nextMoveState Stopped _ = Stopped
  nextMoveState Up _ = Up
  nextMoveState Down f =
    if f <= 1
      then Stopped
      else Down
  |])
```

The two functions above define that the floor should increase when the elevator is going up, and decrease when it is going down, but not below `0` (`nextFloor`) and that when the elevator reaches the ground floor it should stop (`nextMoveState`).

Singletons library generates code that adds type families `NextFloor` and `NextMoveState` to the above declaration and equivalent functions on singletons (`sNextFloor` and `sNextMoveState`). They are defined similarly to the below (but you do not need to add this code - it is added automatically):

```haskell-ignore
type family NextFloor (m :: MoveState) (f :: Nat) :: Nat where
  NextFloor Stopped f = f
  NextFloor Up f = f + 1
  NextFloor Down f =
    If (f > 1) (f - 1) 0

sNextMoveState ::
  Sing (m :: MoveState) -> Sing (f :: Nat) -> Sing (NextFloor m f)
```

etc. The actual generated code is a bit more complex.

Now we can define the type for allowed elevator actions:

```haskell
infixr 2 :>>

type Elevator = (DoorState, MoveState, Nat)

type family Moving (m :: MoveState) :: Constraint where
  Moving Up = ()
  Moving Down = ()

data Action (s :: Elevator) (s' :: Elevator) :: Type where
  Open ::
    Action
      '(Closed, Stopped, f)
      '(Opened, Stopped, f)
  Close ::
    Action
      '(Opened, Stopped, f)
      '(Closed, Stopped, f)
  Move ::
    Moving m =>
    Sing (m :: MoveState) ->
    Action
      '(Closed, Stopped, f)
      '(Closed, NextMoveState m f, NextFloor m f)
  Stop ::
    Action
      '(Closed, m, f)
      '(Closed, Stopped, f)
  Wait ::
    Action
      '(d, m, f)
      '(d, NextMoveState m f, NextFloor m f)
  (:>>) :: Action s1 s2 -> Action s2 s3 -> Action s1 s3
```

`Action` type is defined as generalized algebraic data type and it includes elevator state before and after the action as type-level tuples.
Another alternative would be to parametrize `Action` on 6 separate parameters, but it would be more difficult to manage.

`Move` constructor takes parameter that should be `SUp` or `SDown` (singletons for `Up` and `Down`) and type family `Moving` ensures that `SStoped` cannot be used here.

As actions can only be created using one of the available constructors, only allowed actions can be created. For example, the door can only be opened if it is closed and if the elevator is stopped. And the elevator can only start moving only if the door is closed.

`Wait` action lets the elevator to go up and down, and it will also stop the elevator when it reaches the ground floor.

`:>>` constructor allows to sequence actions, but only in such way that the final state of the first action is the same as the initial state of the second action. It also defines that the resulting compound action starts from the initial state of the first action and ends with the final state of the second action, in this way ensuring the continuity of state transitions.

With this type definition we expressed the original requirements in types without any executable code. The code using these types will not need tests to ensure the validity of state transitions, because we can only create allowed actions and we can only chain actions in a way that state changes are sequential, otherwise the code will not compile.

We can create a small "program" for the elevator to perform a sequence of actions:

```haskell
type ElevatorProgram f f' =
  Action '(Opened, Stopped, f) '(Opened, Stopped, f')

program :: ElevatorProgram 0 0
program =
  Close
  :>> Move SUp
  :>> Wait
  :>> Wait
  :>> Stop
  :>> Open
  :>> Close
  :>> Move SDown
  :>> Wait
  :>> Wait
  :>> Stop
  :>> Open
```

This sequence of actions has type `Action '(Opened, Stopped, 0) '(Opened, Stopped, 0)` - the elevator starts and ends on the ground floor, with the opened door and stopped. If you try to write a program that performs the sequence of actions that is not allowed, it will not compile:

```haskell-ignore
badElevator :: ElevatorProgram 0 1
badElevator =
  Close
  :>> Move SUp
  -- Stop
  :>> Open
```

The error message will be:

```
    • Couldn't match type ‘'Up’ with ‘'Stopped’
      Expected type: Action
                       '( 'Closed, 'Stopped, 0) '( 'Closed, 'Stopped, 1)
        Actual type: Action
                       '( 'Closed, 'Stopped, 0)
                       '( 'Closed, NextMoveState 'Up 0, NextFloor 'Up 0)
    • In the first argument of ‘(:>>)’, namely ‘Move SUp’
      In the second argument of ‘(:>>)’, namely ‘Move SUp :>> Open’
      In the expression: Close :>> Move SUp :>> Open
    |
... |     :>> Move SUp
```

It almost literally says that "to open door, the elevator must be stopped, it cannot be moving up"! So the correctness of state transitions is ensured by the way the type is defined.

To "fix" the program you just need to add `Stop` before `Open`.

## What's the point?

We have the program, but what can we do with it? It is not code, so we cannot just run it.

We can interpret this program in different contexts - for that we need to write interpreter. For example, one of the interpreters can be just printing this program to console:

```haskell
printElevator :: Action s s' -> IO ()
printElevator Open = putStrLn "open"
printElevator Close = putStrLn "close"
printElevator (Move m) = case m of
  SUp -> putStrLn "up"
  _ -> putStrLn "down"
printElevator Stop = putStrLn "stop"
printElevator Wait = putStrLn "wait"
printElevator (a :>> prog) = printElevator a >> printElevator prog
```

The same approach can be used in real code - there could be an interpreter to control the real elevator. But it is now safe, as by defining allowed operations on the type level we ensured that we cannot perform unsafe actions - opening the door while the elevator is moving, or starting to move without closing the door.

## How to make it interactive?

In some situations this can be useful and real programs can be written in this way - we achieved the possibility to restrict allowed operations in type system.

But how can we make this elevator interactive, so it can respond to the passengers' commands? These commands are just data, so we need a way to move between data and types.

We will use singletons for the elevator state - they have type `Sing (s :: Elevator)`. We defined `Elevator` as a type synonym for the tuple of type `(DoorState, MoveState, Nat)` and we do not need anything special to use singletons for this type - as we have created singletons for our types `DoorState` and `MoveState`, singletons library defines singletons for all standard derived types, including tuples.

Because we want to create elevator actions at run time, from passenger commands, we will also need the existential wrapper for the typed action:

```haskell
data SomeAction where
  SomeAction :: Action s s' -> Sing s -> Sing s' -> SomeAction
```

The action constructor includes the action itself and its initial and final states, so we can pattern match on them.

Now if we have the initial state and the action, we can both check that the action is compatible with it and get its final state:

```haskell
finalState :: SomeSing Elevator -> SomeAction -> Maybe (SomeSing Elevator)
finalState (SomeSing s1) (SomeAction _ s1' s2) =
  case s1 %~ s1' of
    Proved Refl -> Just (SomeSing s2)
    _ -> Nothing
```

`%~` compares types of singletons `s1` and `s1'` and returns `Proved Refl` if they are the same. In this case we could have also converted both singletons to their base types and compare using `fromSing s1 == fromSing s1'` but it will not work in the contexts where type equality should be established, and not only value equality.

The following function creates the elevator action from the passenger's commands. We also need to pass the initial state, both to make sure that the action is allowed and also to create actions with specific types, as all constructors are polymorphic:

```haskell
actionFromString :: String -> SomeSing Elevator -> Maybe SomeAction
actionFromString name (SomeSing st) = action name st
  where
    act :: Action s s' -> Sing s -> Sing s' -> Maybe SomeAction
    act a s1 s2 = Just $ SomeAction a s1 s2
    action :: String -> Sing (s :: Elevator) -> Maybe SomeAction
    action "open" s@(STuple3 SClosed SStopped f) =
      act Open s (STuple3 SOpened SStopped f)
    action "close" s@(STuple3 SOpened SStopped f) =
      act Close s (STuple3 SClosed SStopped f)
    action "up" s@(STuple3 SClosed SStopped f) =
      act (Move SUp) s (STuple3 SClosed SUp (sNextFloor SUp f))
    action "down" s@(STuple3 SClosed SStopped f) =
      act (Move SDown) s
        (STuple3 SClosed (sNextMoveState SDown f) (sNextFloor SDown f))
    action "stop" s@(STuple3 SClosed _ f) =
      act Stop s (STuple3 SClosed SStopped f)
    action "wait" s@(STuple3 d m f) =
      act Wait s (STuple3 d (sNextMoveState m f) (sNextFloor m f))
    action _ _ = Nothing
```

We need to specify both initial and final states of the action here, so at first it may seem that we can violate the type restrictions in the Action type and create disallowed action. But if any mistake is made in the function above that could lead to the creation of disallowed command, this function will not compile - try changing any of the state values above.

## Let's run it!

We will let "the passengers" control the elevator, but if they try to perform the action that is not allowed it will be rejected.

To print the current elevator state we need a function to convert it to String, so it shows as, e.g., `(Opened,Stopped,0)`, and not as `SomeSing (STuple3 SOpened SStopped (SNat @0))`:

```haskell
show' :: SomeSing Elevator -> String
show' (SomeSing s) = show (fromSing s)
```

To create an interactive REPL that modifies and prints the elevator state in a loop we will use [interact](https://hackage.haskell.org/package/interact) library <a id="interact_text"></a>[\*](#interact):

```haskell
main :: IO ()
main = do
  let elevator = toSing (Opened, Stopped, 0)
  putStrLn "Enter: open, close, up, down, wait or stop"
  putStrLn $ show' elevator
  replState runElevator elevator

runElevator :: String -> SomeSing Elevator -> (String, SomeSing Elevator)
runElevator act st =
  case actionFromString act st >>= finalState st of
    Just st' -> (show' st', st')
    Nothing -> ("action " ++ act ++ " not allowed", st)
```

The "elevator" now support actions "open", "close", "up", "down", "wait" and "stop" and prints its current state after each action.

You can run the code right from this post by cloning the [site repo](https://github.com/epoberezkin/poberezkin.com) and executing `stack run elevator`. The source code without the text is available [here](https://github.com/epoberezkin/elevator-state-machine).

## Exercises

You can try these problems with this elevator example (possible solutions are in the [linked repo](https://github.com/epoberezkin/elevator-state-machine/blob/master/src/Problems.hs)):

1. Create an operation that chains "run-time" actions `SomeAction`:

```haskell-ignore
(>>:) :: SomeAction -> SomeAction -> Maybe SomeAction
```

2. Create a function that given initial state and a list of commands returns elevator "program" to perform this sequence of actions, if it is valid:

```haskell-ignore
elevatorProgram :: SomeSing Elevator -> [String] -> Maybe SomeAction
```

3. If you "close" the door on the ground floor and send the elevator "down", it will remain `Stopped`, but the command will not be rejected. How to modify the type definition for `Action` to disallow it? If you do it, `actionFromString` will no longer compile - it has to be modified as well.

4. Create a function that given the current elevator state and the requested floor, returns an action sequence in `SomeAction` to send it to the specific floor, in case it is stopped and the door is opened, and `Nothing` otherwise:

```haskell-ignore
elevatorToFloor :: SomeSing Elevator -> Natural -> Maybe SomeAction
```

## What's next?

You may have already asked some of the following questions:

1. How to define actions that return results?
2. How to define more complex state machines when the same action can change the state differently depending on the results of the previous actions?
3. Also, how to write interactive programs with these actions in a more conventional way using `do` notation?

All these questions will be answered in Part 2.

<a id="interact"></a>[\*](#interact_text) disclaimer - I created it
