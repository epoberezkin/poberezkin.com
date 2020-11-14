---
title: "Ajv validator: time to migrate to version 7!"
author: Evgeny Poberezkin
tags: open-source, javascript, json-schema, coding, ajv
github: ajv-validator/ajv
image: ajv.png
---

[<img src="/images/ajv.svg" width="20%" style="min-width: 100px; clear:right; float: right; margin: 0 1.65% 2.5% 5%;">](https://github.com/ajv-validator/ajv)

## Prologue

My relationship with [Ajv](https://github.com/ajv-validator/ajv) changed over time, going full circle:

- the weekend project to enable another project almost nobody knows about (this short [conference talk](./2019-07-10-talk-why-you-should-open-source-for-real.html) can give you some context).
- the project I had a growing excitement about as it was becoming more and more adopted, with some huge number of JavaScript developers using it [all over the world](https://www.google.com/maps/d/u/1/edit?mid=1MGRV8ciFUGIbO1l0EKFWNJGYE7iSkDxP&ll=21.319581133324633%2C-23.649554920324135&z=3), millions of GitHub projects depending on it and over 100 million [npm downloads](https://www.npmjs.com/package/ajv) every month.
- a huge burden I could not allow myself to drop long after I stopped being interested in it.
- realisation that I will develop it further only if I am paid for it and applying for Mozilla's Open Source Support grant - and [having it awarded](./2020-08-14-ajv-json-validator-mozilla-open-source-grant-openjs-foundation.html) to my excitement.
- being as excited about it as I was 5 years ago, having fully re-written Ajv in version 7 at my current level of the problem understanding and competence (I removed lots of embarrassing 5 year old code), thanks to the grant that paid for this work - and there is lots more work to do.

## What's changed in version 7

5 years of adding features and resolving bugs resulted in somewhat tangled mess of [doT templates](https://github.com/olado/doT) (doT engine is still a solid piece of software though) and ES5 code that quite a few people managed to figure out - I am super grateful to [almost 100 contributors](https://github.com/ajv-validator/ajv/graphs/contributors).

Now that Ajv is fully re-written in TypeScript I hope that it can enable much wider contributions from Ajv users.

### Code generation

[doT templates](https://github.com/olado/doT) are replaced with a new code generation API (any suggestions how it can be improved would be super helpful) to generate control flow code using light-weight syntax trees. This tree is optimized, reducing generated code size by 10-15%, compared with version 6.

To generate expressions (and conditions) Ajv now uses tagged template literals that make the code easy to read (unlike string concatenation or doT templates), but at the same time make accidental mistakes impossible - otherwise they could lead to remote code injection (when untrusted schemas are used). If Ajv internal code were to use an unsafe string where a number or variable name is expected, it would be quoted and not executed - so the schema would fail to compile or throw an exception, but not execute untrusted code.

As a side effect it made the code of validation keywords much more concise and expressive. See [code generation](https://github.com/ajv-validator/ajv/blob/master/docs/codegen.md) in Ajv docs. I believe it may have a value as a standalone library - let me know if you have any use cases for it, I am considering splitting it from Ajv.

### Schema compilation

The code that manages schema compilation and reference resolution is radically simplified - the same functionality is achieved with half of the code, with many recursive functions removed or refactored, without losing support of any edge cases. It also allowed to implement support for dynamic recursive references (JSON Schema draft-2019-09) that enable [extension of recursive schemas](https://github.com/ajv-validator/ajv/blob/master/docs/validation.md#extending-recursive-schemas).

### Validation keywords definition

Ajv version 6 has two types of validation keywords - "standard" JSON Schema keywords, hard-wired into Ajv, and "custom keywords" - they have their own API in version 6.

Defining the keywords that are as performant as "standard" keywords required either using string concatenation, which is difficult, or doT templates - that nobody liked. Ajv version 7 removed the difference between bundled and user-defined keywords. While there remains a very small number of hard-wired keywords that are fundamental to how schema compilation and code generation logic works - `type`, `$id` and `$ref` - all other keywords are defined using [the same new API](https://github.com/ajv-validator/ajv/blob/master/docs/keywords.md#define-keyword-with-code-generation-function) that is available to the users to define the additional keywords.

I am really excited about this change, as it should simplify using Ajv to implement any JSON schema language specification, not only JSON Schema with a capital "S", and even wider - to implement any DSL for JSON data processing and transformation. By the time version 7 becomes the main version in December 2020 you will be able to use core Ajv class without any bundled keywords, as the foundation for your own JSON-based DSL compiler.

Users will also be able to use it both to define their own collections of keywords (JSON Schema specification calls them "vocabularies") and also to use a subset of included keywords to reduce the size of the browser bundle.

One other exciting milestone for Ajv coming in 2021 is the support of the new specification for JSON data: [JSON Type Definition](https://jsontypedef.com) - it started from [this conversation](https://github.com/json-schema-org/json-schema-spec/issues/710) in JSON Schema GitHub organization and has been [approved as RFC8927](https://datatracker.ietf.org/doc/rfc8927/) just a week ago.

We finally have a language to define JSON structure that enterprise users will be confident using because of its RFC status. It is super simple and much less error-prone (although with the new Ajv strict mode writing JSON Schemas became less error-prone too).

### Strict mode for JSON Schema

Ajv version 6 has several options to reduce errors when writing schemas: `strictKeywords`, `unknownFormats`, etc. It helped to reduce errors to the users who enabled these options, but all the new users had to learn about these problems from their own mistakes.

Version 7 of Ajv changed this approach to make ["strict mode"](https://github.com/ajv-validator/ajv/blob/master/docs/strict-mode.md) default - it is an opinionated (and optional) set of restrictions about how JSON Schema should be written. For example, a common mistake for the new JSON Schema users is this schema (that is a valid JSON Schema):

```json
{
  "properties": {
    "foo": {"type": "string"}
  }
}
```

A majority of the new users of JSON Schema reasonably expect that this schema requires that your data is an object with a property "foo" that is a string - and it would be actually correct if it were [JSON Type Definition](https://jsontypedef.com) schema. But in JSON Schema specification this schema means the following:

> IF your data is an `object`, AND this object has a property `"foo"`, THEN the type of this property should be a `string`.

So any non-object is valid against this schema, and also any object without a property "foo". Quite a surprise!

In [strict mode](https://github.com/ajv-validator/ajv/blob/master/docs/strict-mode.md) that Ajv has enabled by default this schema would log a warning (or optionally throw an exception with `strictTypes: true` option) unless you add "type" keyword:

```json
{
  "type": "object",
  "properties": {
    "foo": {"type": "string"}
  }
}
```

It still allows objects without property "foo" - you need to use the "required" keyword, but at least it won't allow non-objects now.

If you use TypeScript there is an even stricter option now - you can use a generic type `JSONSchemaType<T>` as a type for the schema itself (assuming the data has type `T`) that would  fail TypeScript compilation unless you add some other keywords - see the [example in readme](https://github.com/ajv-validator/ajv#getting-started).

While strict mode is designed to avoid mistakes, it does not change JSON Schema semantics or validation results - if a schema is correct in "strict mode", it is also correct without it and it produces the same validation results.

### Parse don't validate

Users of static type systems live by [this mantra](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/), and for TypeScript users compiled validation functions are now type guards - validating data narrows its type according to the used schema.

Ajv users who use TypeScript [asked to add it](https://github.com/ajv-validator/ajv/issues/736) for a couple of years - 70 people voted for this feature! Now that TypeScript support is first class, and not an afterthought, both this and another important problem of Ajv is addressed - validation errors now belong to tagged union type.

I am now thinking whether it should be taken it a step further - extending Ajv to generate parsing code that would not just validate the object that was pre-parsed by `JSON.parse()`, but would parse JSON and validate it at the same time, returning data of the expected type if the validation succeeds, and failing as soon as schema violation is encountered when it is possible (some validation keywords would still require parsing the whole JSON). I am not promising it, but it may happen some time next year.

## What was removed

Some people may get sad - I definitely am - that Ajv version 7 no longer supports draft-04 of JSON Schema specification. Draft-04 was the first version of JSON Schema that Ajv supported since 2015 and removing its support is a big decision - it is still used in some old schemas. But the specification has so substantially evolved in draft-06, -07 and -2019-09 that continuing to support draft-04 would further complicate already complex code - removing it brought some simplicity back.

So you will either have to migrate your schemas to draft-06 or later (e.g. using [ajv-cli](https://github.com/ajv-validator/ajv-cli)) or to continue using Ajv version 6 that will be supported until the end of Q1 2021 and will remain stable for a much longer time.

Another breaking change is that support of JSON Schema formats was split to a separate package [ajv-formats](https://github.com/ajv-validator/ajv-formats). The first reason was that JSON Schema specification draft-2019-09 made format validation optional. The second, and more important, reason is that semantic string validation is complex, and it is impossible to find the right balance between correctness, performance and complexity that is acceptable to all users and applications. While you still can easily add the formats provided by ajv-formats package, it is up to you to assess their suitability and, if needed, to replace with other implementations that are appropriate for your use cases.

## Time to migrate to version 7!

Although Ajv version 7 is still in beta, I believe it is stable enough and safe to use. Just make the version fixed until the major version is out of beta - there may be small API changes.

There are several benefits to switch to Ajv [v7.0.0-beta.4](https://github.com/ajv-validator/ajv/releases):

- support of all important JSON Schema draft-2019-09 features:
  - many [new validation keywords](https://github.com/ajv-validator/ajv/blob/master/docs/json-schema.md#json-schema-draft-2019-09), including `unevaluatedProperties` than many users needed.
  - dynamic recursive references to enable [extending recursive schemas](https://github.com/ajv-validator/ajv/blob/master/docs/validation.md#extending-recursive-schemas)
- new, easy to use keyword definition and code generation APIs, both used for included keywords and available to the users - [ajv-keywords](https://github.com/ajv-validator/ajv-keywords/releases/tag/v4.0.0-beta.0) package was also updated to use these APIs.
- smaller bundle size (10% reduction with the all new features added) and reduced generated code size (10-15%) - while it was not an objective, it is a nice side effect. For a moment, I was wondering if the energy savings it might create on a global scale would save one tree...

Have fun migrating to Ajv version 7 - it is available to all of us, thanks to Mozilla's grant and support. I hope these changes help you - if so, share it with other people.

Any feedback and suggestions would be super helpful - thank you!
