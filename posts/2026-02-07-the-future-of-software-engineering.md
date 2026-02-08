---
title: "The Future of Software Engineering"
author: Evgeny Poberezkin
tags: ai, coding
---

Can LLMs independently engineer software systems? Not yet – and the gap is not where most people think it is.

## Which problems AI (Artificial Intellect) can solve in the future?

Engineering, technical specifications, and operations – everything other than deciding what to build and for whom. Requirements lead to engineering specifications and test code, specifications lead to code and deployment. These derivations are [functors](https://en.wikipedia.org/wiki/Functor) – structure-preserving transformations. Machines can do all of this, as long as someone defines the requirements and overall structure. AI can also assist in gathering requirements - all people have to do is decide what people need.

From the point of view of category theory, the specification of a software system is a type. An implementation is a term that inhabits it. The Curry-Howard correspondence established this equivalence decades ago[^curry-howard]. If we could express what a system must do as a precise type, producing an implementation becomes search – constrained by the type, not requiring creativity or intelligence. Compilers already do this for small types. Artificial Intellect will do it for large ones.

This covers all of engineering: technical specification and design, developing code, creating automatic tests, deployment, and maintaining coherence across layers as requirements change. So all engineers not interested in product work will have to look for another job, as the current one won't last once AI appears.

## What LLMs already do – and why it is not enough

Nobody needs to write code by hand any more. LLMs already can act as probabilistic compilers from English to code, with quality more dependent on the prompts and the process than on the model. Just as civil engineers stopped laying bricks long ago, writing syntax by hand became unnecessary[^coding].

Using LLMs is not optional. Small competitors using LLMs ship much faster[^yc]. Attackers already use LLMs to find vulnerabilities and exploits at scale. Not adopting LLMs for security analysis, code review, and testing is a major risk.

But so called vibecoding[^vibe] is fundamentally broken. An LLM given a loosely described feature will produce code that looks plausible and does something approximately right. It will also silently violate some of the constraints that make software correct: security boundaries, error handling, state invariants, performance budgets. The output compiles and runs. It is not engineered.

This can be improved. Compliance and quality increase a lot when you[^prompts]:

- use imperative language,
- state each requirement exactly once, in positive terms, without duplication or overlap,
- explain *why* each requirement matters,
- enforce adversarial self-review loops that catch violations before they compound,
- instruct the model to adopt a task-specific role and personality.

These are engineering techniques, applied to English instead of code.

So for now, engineering is as necessary as it ever was, but the work shifted from writing code to specifying systems precisely enough that a probabilistic compiler can produce correct output. The hard part was always specification. With LLMs, it is the only part that still requires a person.

## Which problems a future AI can't solve?

AI (Artificial Intellect) is not an intelligence. It can simulate intelligence, consciousness and empathy, but these are qualities of living people that neither current LLMs, nor future-generation models, nor AI can possess – I believe, even theoretically. Some people would disagree, but this question deserves a separate post[^consciousness].

If consciousness is not computation, then a system that only computes cannot be conscious, cannot understand, and cannot empathize – it simply has no intelligence. It can only produce outputs that *look like* understanding.

What this means for software: the problems that require understanding what people actually need, what is good and what is harmful, what trade-offs are acceptable – these problems require human intelligence, not models, and not even future AI. An LLM can generate a privacy policy, but it cannot *care* about privacy.

## Which problems LLMs can't solve now?

Model compliance and solving problems while satisfying multiple constraints is a trillion-dollar question.

Given a prompt with ten constraints, an LLM will reliably violate at least some of them. This is not a failure of any particular model – it is by design. LLMs are statistical next-token predictors optimised for fluency, not for constraint satisfaction. The more constraints you add, the worse compliance you get[^compliance].

This creates a compounding problem: it is hard to make LLMs do effective prompt engineering for other LLMs. When one model generates instructions for another, compliance violations compound at each level of the hierarchy. Some people, especially in enterprise, believe that hierarchical structures of LLMs can achieve what hierarchical structures of people achieve – that you can build an organisation chart out of language models and get organisational intelligence. This is wrong for two reasons.

First, hierarchies of people work because each person has consciousness, empathy, and the ability to exercise judgement about unstated requirements – to push back when instructions are incoherent. LLMs do none of this; they comply or hallucinate.

Second, even setting aside the intelligence that LLMs lack, the compliance mathematics are very bad. If a single LLM follows a ten-constraint prompt correctly 70% of the time, two levels of LLM delegation yield ~50% compliance, three levels - ~35%. Real systems need hundreds of constraints across multiple levels. The numbers do not work. Current "agentic" approaches that chain LLMs together are not solving this problem, they are hiding it behind retries and deterministic orchestration layers programmed by engineers.

## How software could be engineered by an Artificial Intellect

I believe that LLMs are a necessary component of AI, but not a sufficient one – and scaling them will not bridge the gap. How a true AI could be created deserves a separate post too[^ai].

Consider instead what software engineering would look like with a real intellect – one that reliably satisfies hundreds of constraints while knowing why they exist.

The approach would be categorical, in the mathematical sense. A software system is a collection of categories (layers) connected by structure-preserving functors: a purpose (why it exists and for whom), capabilities (what it does), a technical design (how it is organised), executable code, acceptance criteria, and a deployment process. Each layer forms a category: the entities it describes are objects, their relationships are morphisms. Functors between adjacent layers preserve structure – every purpose maps to a capability, every capability to a design component, every design component to a code module[^categorical].

The key property is compositionality. If the hypothesis that a software system can be formalized as a complex type holds, and if each adjacent-layer functor is correct, the end-to-end mapping from purpose to deployment is *automatically* correct – this is a theorem of category theory, not only empirical knowledge about how systems work. The product owner defines what the system should be (the top layers). The AI "compiles" it through the functor chain, and verification checks that structure was preserved at each step.

This requires reliably verifying constraints at each functor boundary – that every entity maps, nothing is orphaned, relationships and security properties are preserved. Current LLMs consistently fail at this. But the future is AI-enabled compilation, where the AI finds the solution that satisfies typed pre- and postconditions and deterministic verification confirms the result.

## Who will still be needed?

Product owners – people who know what others need, who understand what is good and what is harmful, and who care about outcomes in a way that no AI can.

Not managers, and not engineers as we define them today. Any profession whose core function is transforming requirements into artifacts will be automated, once compliance and constraint satisfaction are solved. This is most of what engineers and managers do.

What cannot be automated is the *wisdom* layer: deciding why a system should exist, for whom, and what problems it solves.

Enterprises accelerating the transition to AI will fail if they believe they already have it. What we have is a very helpful assistant, excellent at modelling and language generation. Calling LLMs "AI" is damaging: it conflates LLMs with the intellect, preventing investment into the missing components[^llm-not-ai].

Once actual AI appears, the transformation of the world will be much faster than what we see today. But this future is not here yet.

---

[^curry-howard]: The [Curry-Howard-Lambek correspondence](https://wiki.haskell.org/Curry-Howard-Lambek_correspondence) establishes a structural identity between types (programming), propositions (logic), and objects in categories (mathematics). A specification-as-type is simultaneously a logical proposition and a categorical object; an implementation satisfying it is simultaneously a program, a proof, and a morphism.

[^coding]: ["English is the new Programming Language"](https://deadneurons.substack.com/p/english-is-the-new-programming-language).

[^yc]: YC W25: [25% of startups 95% AI-written, fastest batch ever](https://techcrunch.com/2025/03/06/a-quarter-of-startups-in-ycs-current-cohort-have-codebases-that-are-almost-entirely-ai-generated/).

[^vibe]: Vibecoding is an oxymoron. "Coding" is "codifying requirements and specification in code". People who "vibecode" do not codify anything - they have a dialogue with LLMs and from rather incoherent conversation emerges an incoherent system, that neither vibecoder nor LLM can reason about. Nobody knows what it does beyond a certain threshold.

[^prompts]: These techniques are supported by research: positive framing avoids the [Pink Elephant Problem](https://eval.16x.engineer/blog/the-pink-elephant-negative-instructions-llms-effectiveness-analysis) where negative instructions paradoxically increase unwanted behaviour; self-review loops approximate [Constitutional AI](https://arxiv.org/abs/2212.08073) patterns. See [The Prompt Report](https://arxiv.org/abs/2406.06608) (Schulhoff et al., 2024) for a taxonomy of 58 prompting techniques from 1,565 papers, and [the instruction gap](https://arxiv.org/abs/2601.03269) research on LLM compliance rates.

[^consciousness]: The [computational theory of mind](https://en.wikipedia.org/wiki/Computational_theory_of_mind), once the dominant framework in cognitive science, is breaking down. [Penrose and Hameroff](https://pubmed.ncbi.nlm.nih.gov/24070914/) argue consciousness requires [non-computable quantum processes](https://en.wikipedia.org/wiki/Orchestrated_objective_reduction) in microtubules. [Tononi, Koch et al.](https://arxiv.org/abs/2412.04571) demonstrate using [integrated information theory](https://en.wikipedia.org/wiki/Integrated_information_theory) that digital computers have near-zero integrated information regardless of software. [Seth](https://www.noemamag.com/the-mythology-of-conscious-ai/) argues it is a property of life, not of computation. Etc.

[^compliance]: [IFScale](https://arxiv.org/abs/2507.11538) (Jaroslawicz et al., 2025) tested 20 models across 1–500 constraints. Weaker models show exponential decay; stronger models degrade linearly; the strongest hold near-perfect compliance until a critical threshold, then fall off a cliff. At 500 constraints the best model scores 69%, the worst 7%.

[^ai]: This paper can be relevant to future AI design: Fritz (2020) [A synthetic approach to Markov kernels, conditional independence and theorems on sufficient statistics](https://arxiv.org/abs/1908.07021).

[^categorical]: Bartosz Milewski, [Category Theory for Programmers](https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/).

[^llm-not-ai]: Formal verification, constraint satisfaction and causal reasoning that a true AI requires.

