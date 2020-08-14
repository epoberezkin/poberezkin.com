---
title: "Ajv JSON Schema validator: Mozilla MOSS grant and OpenJS Foundation"
author: Evgeny Poberezkin
tags: open-source, javascript, json-schema, coding, ajv
github: ajv-validator/ajv
reddit: r/javascript/comments/i9oq86/ajv_json_schema_validator_mozilla_moss_grant_and/
image: mozilla.png
---

[<img src="/images/mozilla.svg" alt="Mozilla" width="33.3%" style="min-width: 165px; float: right; margin: 0 0 5% 5%;">](https://www.mozilla.org/en-US/moss/)

[<img src="/images/openjs.png" width="30%" style="min-width: 150px; clear:right; float: right; margin: 0 1.65% 5% 5%;">](https://openjsf.org/)

[<img src="/images/ajv.svg" width="20%" style="min-width: 100px; clear:right; float: right; margin: 0 1.65% 2.5% 5%;">](https://github.com/ajv-validator/ajv)


I am really excited to share this news: [Ajv](https://github.com/ajv-validator/ajv) has been awarded a grant from Mozilla’s [Open Source Support (MOSS) program](https://www.mozilla.org/en-US/moss/) in the “Foundational Technology” track and it joined the [OpenJS Foundation](https://openjsf.org)!


## What is Ajv?

Ajv is a JavaScript open-source library for data validation using [JSON Schema standard](https://json-schema.org/).

I started developing Ajv 5 years ago, because all validators I’ve tested failed some of the tests I had. At the time I thought that JSON Schema is a relatively simple standard and it should not take more than 3-4 weekends to make a working library. It took 4 months of weekends and evenings, outside of work, and it took another year to stabilize and to fix bugs.

Without the help from contributors and users, Ajv would had never become as widely used as it is – I am very grateful to [almost 100 contributors](https://github.com/ajv-validator/ajv/graphs/contributors) who helped shape Ajv over 5 years.


## Why grant from Mozilla?

Ajv started when JSON Schema standard was at version 4, it now supports version 7. The current version of JSON Schema (version 8 – draft 2019-09) introduced 2 substantial validation paradigm changes. The additional features increased implementation complexity – to consistently implement these features in Ajv will require substantial re-write. Also, a new promising standard for JSON validation appeared – [JSON Type Definition](https://tools.ietf.org/html/draft-ucarion-json-type-definition-04) (JTD).

Ajv users are interested in the support of the latest JSON version, but it was not sustainable for me to implement it in my free time – it is a large amount of work, particularly given how important it is to allow all existing users to migrate without disruption. Support from the users via OpenCollective and GitHub sponsors has confirmed that Ajv needs to be developed further, so I’ve also applied to Mozilla Open Source Support fund. I am very happy that Mozilla awarded the grant.


## OpenJS Foundation

OpenJS Foundation supports many critical JavaScript open-source projects with both infrastructure and organization. As Ajv became a part of millions of web applications, having this support will  ensure the longevity and stability of Ajv for all users.

Working with OpenJS foundation will also help better align Ajv with the JavaScript ecosystem and to grow the number of contributors, and may help with wider enterprise adoption through greater confidence in the project.

There is more information, and many supportive words from OpenJS Foundation in [the announcement](https://openjsf.org/blog/2020/08/14/ajv-joins-openjs-foundation-as-an-incubation-project/).


## What is next?

I’ve already started working on the next major version of Ajv. The plan is to support both the last version of JSON Schema and JTD in Ajv, so that the users can choose the standard that works best for them – and the users will be able to package only the functionality they actually use.

I am looking for the long-term maintainers joining me to share the ownership of Ajv – please reach out if you are interested! I am working with [ReadySet](https://www.thereadyset.co/), also sponsored by Mozilla, to establish guidelines for the role of a "maintainer" and the contribution standards, to adjust our code of conduct to fit our users and contributors community better, and to encourage a wider, more inclusive, contribution from the community.

We believe that it will lower the barrier for the new contributors and maintainers, for all experience levels, and will make expectations clear - how long it usually takes to review and to decide whether the change is necessary, and what are the requirements.

I am looking forward to the next phase of Ajv development that is possible thanks to Mozilla.
