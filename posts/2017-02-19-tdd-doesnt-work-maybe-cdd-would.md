---
title: TDD doesn’t work… Maybe CDD would?
author: Evgeny Poberezkin
tags: tests
ref: https://medium.com/@epoberezkin/tdd-doesnt-work-maybe-cdd-would-1fd756d2ca4b
---

Test Driven Development existed long before the term itself. It’s been “rediscovered” and popularised by Kent Beck, the creator of Extreme Programming. He wrote on Quora:

<img src="/images/tdd1.png" alt="Kent Beck on Quora" title="...How else could you program?">

The complexity of software systems since those ancient times has evolved to a point when it is no longer possible to define the expected application behaviours in as simple terms as “the output tape you expect”.

The software development community is split, with opinions ranging from “[TDD is dead. Long live testing](https://dhh.dk/2014/tdd-is-dead-long-live-testing.html)” (by [David Heinemeier Hansson](https://dhh.dk/), the creator of Ruby on Rails and the founder & CTO of Basecamp):

> *Test-first fundamentalism is like abstinence-only sex ed: An unrealistic, ineffective morality campaign for self-loathing and shaming.*

to “[What’s Wrong with Test-Driven Development (TDD)](https://dzone.com/articles/whats-wrong-test-driven)”:

> *[TDD] is a powerful methodology that helped me combat “analysis paralysis”, create robust, maintainable code and there’s the added benefit of the resulting unit tests which provide a safety-net against regression bugs.*

The latter one says that TDD is nearly perfect, we are simply doing it wrong, being scared to leave our comfort zones and to watch our tests fail… I wish it were as simple as that.

<img src="/images/tdd2.png" alt="If it's so simple, why doesn't everybody doing it?" width="100%">

Really, why?

There is also no consensus about how software should be designed, even among developers who preach and practice TDD. Some moderate believers say that [TDD is NOT "Test Driven Design"](https://vladikk.com/2016/01/22/tdd-what-went-wrong/), that it should be used together with Domain Driven Design and call for reformation into TDD 2.0. Zealots insist that "[TDD is a design methodology - the unit tests are just a by-product of the process](https://dzone.com/articles/whats-wrong-test-driven)".

---

Ok, let's step back from the heat of the battle and ask ourselves what objectives we want to achieve, with or without TDD?

At a high level, software developers in both camps want to have lower costs of creating and using software. It means:

1. Reducing the number of bugs
2. Making changes with minimal regressions
3. Releasing changes with lower risk of failures
4. Maintaining code easily and running tests quickly
5. etc.

We definitely need team-wide processes for the development project success. [Uncle Bob writes](http://blog.cleancoder.com/uncle-bob/2014/06/17/IsTddDeadFinalThoughts.html) that if only a part of the team does TDD and another one doesn't it will lead to divorce. But do we really need an industry-wide standard development process to achieve those objectives?

Software development projects are as diverse as businesses. As the complexity of the businesses was growing they in many cases transitioned from process management to [management by objectives](https://en.wikipedia.org/wiki/Management_by_objectives).

So what metrics in software development should we manage to achieve a higher level objective of reducing the costs?

I can see a strong inverse correlation between number of costly issues (bugs, regressions) and code coverage. That's exactly the reason why a majority of popular open-source projects display this metric. A higher code coverage is usually seen as a higher reliability of the code, both by developers who follow TDD and those who don't.

<img src="/images/tdd3.png" alt="inverse correlation of bugs and code coverage" title="It's only a picture for 'inverse correlation', not the result of some research" width="100%">

---

My development process aims to achieve an effective design and high code coverage. It could be called **Coverage Driven Development** and consists of three phases:


### 1. Plan & Design

I make a list of features and requirements for a both minimal and viable implementation that can be given to the end-users. The plan allows to foresee external dependencies and all requirements that the application should satisfy, and the design should be coordinated with this plan.

At this point there are no (or very few) external constraints to help structure the code. So I quickly iterate code to figure out how basic models, APIs, functions and classes should look. I am not worried if my code even works, and I usually don't write tests during this phase, they only hinder the understanding of what the right design is.

Iterating code allows to avoid unnecessary intermediary abstractions that often plague TDD-written code and achieves a robust foundation of code structure. And it helps to transfer a visual image of the system that only exists in the the sketches or in the mind into the code and see if this picture still makes sense and whether it supports the whole plan. If it does, I go to the next phase, in not - I continue iterating the code until it does. Depending on the complexity of the system this phase can take from a couple of hours to a couple of days, or even weeks in a really big project.


### 2. Develop

Before development continues, I write tests for all the code written in design phase to achieve code coverage of 95%+.

Each feature in the plan (that continues to evolve) requires some design thinking so I continue rapid code iteration and refactoring and in most cases I write tests only after code, getting to 95%+ code coverage before going to the next feature.

Depending on your quality requirements and the code size you can set a higher threshold for code coverage in your project, but the cost or writing and maintaining the tests grows very quickly as you approach 100%.


### 3. Maintain

This phase starts long before MVP is complete. In case when bugs are discovered or some simple changes should be made I almost always write a failing test BEFORE writing any code, exactly as TDD instructs. In this phase it just makes more sense. During the implementation of the fix I often discover some other issues and do some refactoring, but I don't always write tests for them as I fix them - switching context damages focus and speed. Once the implementation is done I write additional tests to maintain code coverage at 95%+ level.

<img src="/images/tdd4.png" alt="Coverage Driven Development phases" width="100%">

---

This is not a linear process, it is a cycle that repeats multiple times, even before the application reaches the end-users. Also these phases are not strictly separated in time, they are more like "modes of operation" that can overlap.

The whole development process is driven by a single objective quality metric - code coverage. Uncle Bob sees TDD as a pre-requisite to achieving high code coverage (see "[Is TDD Dead?](http://blog.cleancoder.com/uncle-bob/2014/06/17/IsTddDeadFinalThoughts.html#a-team-divided)"). But it's not the case, you can maintain high code coverage without practicing TDD all the time.

You just need to follow several simple steps:

1. Implement code coverage measurement as early as possible.
2. Make the measurement run on every build.
3. Make coverage visible in your source code repository.
4. Make coverage change visible in PRs.
5. Only merge PRs if they increase the coverage or if it is already higher than the threshold agreed with the team for the project.

Code coverage as a metric is not perfect, and I often hear the following criticism:

1. "It depends on code style". It is true, but it does not reduce the efficiency of code coverage as quality metric. Adopting consistent code style for the team makes code coverage measurement consistent as well.

2. "100% coverage does not guarantee the absence of bugs". It is also true, but neither does following TDD. No sufficiently complex system is free of bugs. While the absence of bugs is theoretically possible to achieve, and even possible to prove in some cases, it is prohibitively expensive. On the other hand maintaining high code coverage dramatically reduces the number of bugs and the probability of regressions.

---

If you are not using code coverage as part of your development process, it's definitely worth trying. If nothing else, it will give you an increased satisfaction from writing tests. And all developers seem to agree that we should write tests to reduce the cost of software.

<img src="/images/tdd5.png" alt="Acheived 100% code coverage" title="… but the bug is still there" max-width="100%">
