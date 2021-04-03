---
title: "What I wish somebody told me when I was learning Haskell"
author: Evgeny Poberezkin
tags: haskell, coding, essay
image: ajv.png
---

Everybody agrees that functional and imperative programming are fundamentally different. Using imperative programming languages we tell computer what to do, step by step, mutating the data in variables as we go. With functional programming we tell computer what our data, functions and the whole program are, assigning them to names. The values assigned to names cannot change, so they are not "variables".

Most programmers who use functional programming for commercial projects see twice higher productivity - but before it is achieved there is often a steep learning curve, that makes many people reluctant to learn and to use it.

Many people using Haskell commercially believe that it is the most advanced programming language that can be used for commercial applications, giving its users the highest possible productivity and the lowest possible cost of ownership among all programming languages. The definition of engineering productivity can be loosely taken as a time to launch for a given set of requirements for a fixed size team, and the cost of ownership as the engineering time required to maintain the application. It is also the most effective in terms of scaling the application design.

Not only it has effective implementation of immutable data structures and the choice between lazy and strict evaluation, it has a very advanced type system, with polymorphism and inheritance for type classes (somewhat similar to interfaces in other languages). I have written previously that Haskell is a higher order language, that allows to effectively create an equivalent of new languages as a library. Even the core of the Haskell itself - associative data structures, Monads, concurrent channels and transactional memory, that may exist as language level primitives in other languages, are just libraries written in Haskell.

It is worth watching [the video by the Aaron Contorer](https://www.youtube.com/watch?v=ybSBCVhVWs8), the chairman of FP Complete, about functional programming and modern devops, where he tells how Haskell allows to achieve productivity, quality and performance without the need to compromise any of them.

### Why so few engineers and companies use it?

If Haskell is such an advanced and effective language why we are not observing much wider commercial adoption? I believe that the limited adoption is caused by how we teach it - we make it 5 times longer than it could have taken if we did it effectively.

Most engineers have about 2 months to get productive and fluent with a new language. It is exactly how much time it takes if you are surrounded by other engineers who use Haskell commercially (both my experience of on-boarding my son to our open-source project and what other people say), but if an engineer tries to learn Haskell on their own, they invariably get stuck.

Most existing tutorials and book try to teach Haskell in the same way as imperative languages are taught, without focussing on fundamental differences about how to read and write Haskell programs. We do agree that functional programming is fundamentally different, yet we teach it in a classic way.

So, what did I wish somebody told me, to save me at least 6-8 months learning Haskell?

## How to read programmes

### Read types, not code

We are very used to say "read code", and we take it for granted that to understand what the program does you have to "read its code". Unfortunately, in most cases, coming from imperative languages, we do not see types as part of the code - we see it as annotation.

This mindset simply does not work in Haskell. Every time you try to understand what the function does by looking at its code, you are wasting time, and in most cases it is simply impossible.

That's the first thing you have to try to unlearn, as quickly as possible, and start reading types to understand what the function "is", not what it "does".

### Read it slowly

The problem for the beginners is that we are used that there is lots of code and it has to be read as prose. Haskell code is very compact, and it has to be read slower, as if it were a formula. When you get used to it you can write and read the same logic much faster - and it is much easier to understand what you code is.

I had a particular example when 70 lines of TypeScript code took 8 one-line functions in Haskell (and all lines were normal length and quite readable), so 16 lines together with type signatures.

Reading each Haskell line usually takes twice longer (once you get a bit used to it), but because of much smaller code size it is twice faster to write and to read than equivalent logic in TypeScript.

### Functions do nothing

In a functional language a function does nothing - it simply defines a transformation between types - but it does not perform this transformation until it is... it is tempting to say "called", but functions are not "called" in functional programming languages - they are evaluated and combined in bigger transformations, until the whole program is constructed. Only when the whole program is executed something happens - but you cannot simply break down program execution to the individual function calls - compiler can produce a much more efficient machine code than if each function were individually called.

### Function evaluation produces no side effects

This is the consequence of the fact that the function does nothing. To have a side effect it would have to do something, but it does not. To my and many people surprise, this statement holds true even for functions that evaluate to the type of IO monad - it is conventional to say that such functions perform IO, are executed step by step and have side effects. But in reality it is not function evaluation that produces side effect - function simply evaluates to IO action. This action can be chained with another IO action or it can be passed as parameter and simply be thrown away based on some other function evaluation. The whole program is one combined IO action that is, when executed, does indeed produce side effects.

Many tutorials separate functions to functions with and without side effects, but it only increases the confusion about how the language works, and confronted with a simple function with the type `Bool -> IO a -> IO b -> IO (Either a b)` most beginners (myself included) would be puzzled by what this function can possibly do.

But the second you remind yourselves, that functions do nothing, you may realize that this function probably evaluates to a single IO action by choosing one of two provided IO actions based on the boolean value (and none of the actions is executed):

```haskell
ifIO: Bool -> IO a -> IO b -> IO (Either a b)
ifIO x a b = if x then Left <$> a else Right <$> b
```

### Polymorphism on steroids

Standard eduction - from examples to abstractions.

You would benefit if you are able to learn to operate on abstractions without worrying about particular examples.

The code above:

```haskell
ifM: Bool -> m a -> m b -> m (Either a b)
ifM x a b = i                       f x then Left <$> a else Right <$> b
```


 But thinking about the function evaluation as about something that product

### Types are NOT annotations, they are the model of your program

Types in Haskell do not serve as annotations - they are the part of the language used to model the whole program to define the shape and constraints on the transformations that the program will perform.

### Always model top down, not bottom up

While it is tempting to model the program bottom up, starting from the elements that define some small parts of your program - this is what we often do with imperative languages, you would arrive to a much more effective program design by modelling it from top to bottom, breaking down the whole program to the small number of components (not more than 3-7), then breaking down each component to smaller parts, and so on until you have the whole program.

What is a component in Haskell code? Usually it is some function that has name and a type - you can always have some stub implementation to have you code compile long before it is written.

### Use Hoogle

Hoogle is a great resource to find functions via their type signatures. As you model your code you are likely to have many cases when you know kind of function you need (that is, its type), and you may reasonable expect that the some library has it, but how do you find it?

In many other languages you are likely to try to think how it is called, and then find it by name. With Haskell all you need is to write down the type of this function and then you can search it. For example if you search for function with type [(a -> b) -> (c -> f a) -> c -> f b](https://hoogle.haskell.org/?hoogle=(b+->+c)+->+(a+->+m+b)+->+(a+->+m+c)&scope=set:stackage), which is a composition inside Functor, you would find two packages that implement it as function `<.>` which looks similar to `<$>` (the latter being application inside functor) - you can either use this function from the library or just look at its implementation and see that `f <.> g` is exactly the same as `fmap f . g` (or `(f <$>) . g`), so you could just use this code.

### Do NOT use algebraic effects

I am talking about 

Well, unless you work at GitHub or some other company that uses them already and you have people to support it.

The problem is that while effects look like

### Do use recommendations from FP Complete

Why bother learning it if it is so difficult to learn
  simplicity
  Haskell = robustness and stability and rigor and velocity (less code + faster modelling) and performance (if needed and extra effort invested)

How Haskell community teaches Haskell to the newcomers does not work

Haskell hacks

- searching type signatures in Hoogle

https://hoogle.haskell.org/?hoogle=(b+->+c)+->+(a+->+m+b)+->+(a+->+m+c)&scope=set:stackage

(<.>) :: Functor f => (a -> b) -> (c -> f a) -> c -> f b
f1 <.> f2 = fmap f1 . f2

### Use functional rather than imperative terminology

It seems insignificant, but using the terminology from the imperative languages makes it more difficult to switch to the functional language mindset.

Functions are not "called" and "executed", they are "used" and "evaluated".

Functions do not "return" values, even when they use `return` function, they are a binding of function code, that is evaluated, to function name.

"Names" are not "variables", and they are not "assigned", they are "bound" to values.

`do` syntax for monads does not define execution steps, it defines the sequence of actions bound via their Monad Typeclass interface.

- what is monad

https://two-wrongs.com/the-what-are-monads-fallacy

https://byorgey.wordpress.com/2009/01/12/abstraction-intuition-and-the-monad-tutorial-fallacy/

Important to remember that monads are functors and applicatives, so you have two more interfaces.

Musical instrument analogy is wrong, because they all have different interfaces. Monads, on another hand, has the same interface.
Some monads have additional interface, but there is a common interface to them all.
But you play them differently nevertheless.


- code



So do get fluent with Haskell you have to start from unlearning how you read code - you should never try to understand what function "does", because it does nothing, you should try to understand what function "is" by looking at its type signature.


- types

Looking at types of functions you should be able to understand what the code could be doing, without even looking at the code itself.

Most imperative programmers see types as restrictions for their data. The reason for that is that the type system is very limited. Haskell has a very advanced type system that allows to use types as the model of your system - it codifies not only your data structures, but also your code structures.


- monads

Some beginners try to understand monads without understanding functors and applicative. But each monad is a functor and an applicative at the same time.

So it is worth learning these classes in exactly this order - Functor -> Applicative -> Monad
These are the most important classes, other are less important, but it is also worth looking at Foldable and Traversable.

Once you get comfortable with these, you can look wider, at typeclassopedia. https://wiki.haskell.org/Typeclassopedia


- class

This term helps confusion. In many language "class" effectively means a hybrid of a record and typeclass in Haskell - class in Haskell is an interface, it does not tell you anything about the data stored in this type.

Each type can be an instance of multiple classes.

- type synonyms vs newtype

- do not worry about glue code
the smaller it is the better. Focus on types.

- functions do not do anything - they define relationship

- every function can be replaced with its code - always.

- you use computer without understanding how it works - do not waste you brain cycles trying to understand every single piece of glue code - focus on meaningful code.

- do NOT yet use effects

- do use Reader monad to pass application environment / configuration as context. If the application is large, you can have polymorphic type of the environment in each application part, that only expects a particular property in the environment - without being able to access the whole environment, and without the need to recompile module when the type of the environment changes.

- it's ok to use Except monad to manage error handling - better that exceptions and Either, but you still have to handle exceptions, as most libraries throw them

- use unliftio library

- use STM

- study Parallel and Concurrent programming in Haskell.

- do NOT read learn you a Haskell - this is very unhelpful book.


I think the future belongs to some [non-existent] language(s) that have a hierarchy of categories that can represent values, types, types of types etc., where entities are of just one kind - whatever they are - values should be able to become types, types should be able to become types of types and they should be able to move in opposite direction. This actually would make things much simpler, as you won’t need to think about types as something that exists at compile time and values as something that exists at run time - it should be a compiler’s decision what needs to be computed when - for the users of the language it should be transparent… The fact that programmers need to worry about compilation specifics is a consequence of a limited and at the same time complex design. Sometimes it is possible and effective to compute the values at compile time. Sometimes it is necessary to have access to types at run time (dependent types being a step in that direction)... The fact that we have to tell the compiler when we need what (or compiler author just making a decision that everything should be computed at run time = duck-typing) are both putting the complexity of managing simplicity and effectiveness on programmer. The language of the future will have just one kind of things organised in conceptual hierarchy of different categories.

Same goes about the separation between data and code - a really advanced future language would have no such separation in its core (in the same way assembler does not have it).

Nobody yet designed a language as a “product" with the ultimate objective of making the value created with it operationalised. If programming is defining an effective and simple language for your [business] domain - effectively and quickly - the languages should somehow be optimised to achieve this end…

I agree about Haskell's cognitive overhead - community still didn’t figure out the best way to manage composition of effects - all the proposed solutions are either complex or limited in some way. The future seems to be in composable algebraic effects systems with dependent types, but we are talking 5-15 more years of language evolution at the pace it is going. To Haskell’s defence no language seems to have figured it out any better than Haskell yet - everybody just writes much more code to express the same logic.

Also, the Haskell education/onboarding is completely broken - there is a way to make people productive in Haskell quite quickly as I am observing as I am transferring my knowledge to my son - but you have to do it very differently to how it is done. What is missing is some basics:
1. Clear instructions on how to read and write “code” - if you come from any other language and nobody tells you that you cannot really understand the code without understanding the type of the code, you will waste lots of time. The overhead happens because you habitually try to read the executable part of the code first and only look at type part of the code if you get stuck (they are even called “annotations”, as if they are something secondary, while in Haskell types are primary) - this is extremely inefficient and everybody who gets productive with Haskell eventually switches to doing it the other way around - your types define and drive your implementations, and that’s what you should read and write first - that’s the source of the terms “type driven” or “algebra driven” development both seem to be coming from Sandy Maguire (or at least that’s where I saw them first).
2. Clear guidance of what works and should be used in production and what is best avoided unless you fancy contributing to these libraries. Everybody eventually just arrives to the conclusion that effect systems with all their attractiveness are neither ergonomic enough yet, nor performant, and lack first class language support yet (which actually should change quite soon thanks to Alexis King), that you should use MTL (from base), Reader monad, STM, and unliftio created by Snoyman/FPComplete, but you still have some hard choices there… And you should not bother using State (in most cases) or Writer that are both quite prominent in many tutorials, but are quite useless for anything other than some narrow tasks and parlour tricks (e.g. implementing imperative algorithms without side effects - in most cases you either need side effects, or there are more efficient equivalent functional algorithms - just a search away)… Snoyman advocated for “boring Haskell” to help onboarding people but it’s another extreme that he himself doesn’t practice.
3. Each library in Haskell defines its own language, literally, - so you get much bigger benefit by adopting a library but while it requires much less investment than learning a new language, it requires much more investment than an average library in any other language.
4. Lots of moaning about libraries not well documented - but it goes back to the people not seeing types as something to read slowly and attentively - they are used to the same information written as prose. I remember the point when I suddenly realised that majority of libraries have more than enough documentation to use them effectively, but you must always read types and occasionally look at the source code which is quite easy to read in most cases - most functions are very short.

So I completely agree about pains of adopting Haskell and the cognitive overhead, but more than half of this pain is self-inflicted by the community and could have been completely avoided by better and more structured communication to the newcomers - just tell them what to expect and where to look, instead of focussing on the language syntax and primitives, as all tutorials are doing. I still stand by the statement that Haskel is a "higher order language" (not sure if you’ve seen this post) but without substantial restructuring of how Haskell community communicates to the newcomers - and it’s not about just being friendlier or more supportive - this IS a very supportive community - it is about clarity of expectations, cost benefit analysis of different paths, honesty about what works and what doesn’t, instructions on how to read the code and the documentation - it is all very different in Haskell, and for a good reason, but without explaining it people just fail and abandon it…. My experience of onboarding my son to Haskell confirms all that - he gets stuck exactly where I was stuck, but the difference is that I can just tell him how to get unstuck, and nobody seem to tell these simple things in Haskell community….

It’s almost as if you were trying to read/learn Japanese in the same way as you read/learn French…

I also do agree with that statement in the post you shared that most problems take less time to implement in Haskell - but it is only if you know what you are doing, otherwise the cognitive overhead of learning to do things differently will destroy any time savings. I remember some test that took 70(!) lines of typescript and 16 lines in Haskell - exactly 8 functions one line of type + one line of code to evaluate each. Partially it was thanks to being able to cheat a bit - I just used dictionaries as keys for another dictionary - it is effective in Haskell, but in typescript I had to compute these hashes manually - performance was on par… It actually goes back to Rick’s point of having associative data structures - in Haskel you can use them as keys (by value), unlike TS (where you can also use them as keys but only by reference). What’s also important that in Haskell all these nice things (associative data structures, Monads, etc.) are not part of the core language - they are just libraries, even such advanced stuff as concurrent channels and STM that exist as language level primitives in other languages, in Haskell are just libraries written in Haskell…

Not sure if you read Stephen’s Diehl’s “What I wish I knew when learning Haskell” - it’s worth reading, but even there he doesn’t talk about simple things above.
