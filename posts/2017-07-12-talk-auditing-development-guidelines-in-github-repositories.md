---
title: "Talk: Auditing development guidelines in GitHub repos"
author: Evgeny Poberezkin
tags: talk, github, coding, open-source
ref: https://skillsmatter.com/skillscasts/10399-auditing-development-guidelines-in-github-repositories
github: mailonline/gh-lint
---

This is a [talk at FullStack London 2017](https://skillsmatter.com/skillscasts/10399-auditing-development-guidelines-in-github-repositories).

<a href="https://skillsmatter.com/skillscasts/10399-auditing-development-guidelines-in-github-repositories">
  <img src="/images/talk2017_2.jpg" alt="Auditing Development Guidelines in GitHub Repositories" width="100%">
</a>

> If your organisation has hundreds of code repositories you probably have some guidelines for them: how they are documented, how branches are protected, whether direct commits to master branch are allowed or only PRs should be used and all PRs should be reviewed, whether tests are run and code coverage is reported to PRs, etc.

> Making sure that those guidelines are followed is a difficult task - even if all team members agree to do so, sometimes we simply forget or don't have time to implement the necessary changes.

> Once we've agreed on our development guidelines, I was looking for a tool to automate such auditing for our team, so that in the same way as eslint can be used for testing code guidelines based on the rules, we could use this tool to audit our repositories in GitHub organisations. I couldn't find one so I created it.

> Meet [gh-lint](https://github.com/MailOnline/gh-lint) - a rule-based command-line utility that audits all your GitHub repositories generating results in TAP (Test Anything Protocol) format that can be consumed by [tap-github-issues](https://github.com/MailOnline/tap-github-issues) reporter that can create, update and close issues in GitHub repositories.