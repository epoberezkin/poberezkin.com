---
title: "AI Can Reopen the Internet"
author: Evgeny Poberezkin
tags: ai, internet
---

## Internet: commerce not allowed

When the internet began, commerce was not allowed on it. The NSFNET backbone policy prohibited its use "for purposes 'not in support of Research and Education'"[^aup]. An act of Congress let the networks be used for other purposes in 1992[^law], and in April 1993 CERN gave up all rights to the web's code[^cern], so that anybody could build on it.

What followed was a market that nobody owned. Anybody could publish and read, and people found services in catalogues[^catalog].

## How we lost it to big tech

Catalogues did not scale: people curated them by hand, and the web grew faster than they could. Search replaced them and became private – a few companies own the indices and fill the top results with paid placements, which people click even when they are more expensive or worse rated[^clicks]. And software was expensive, so only large companies could afford to build the services people used.

The market became a few walled gardens. Their owners decide who gets found, see every buyer and take a cut of every deal.

## AI changes the game

AI reverses all three. Catalogues can scale again, as LLMs can curate them and search them. Search indices can become a public good, because data is now cheap. And creating software costs next to nothing, so anybody can build a service.

Agents also make buying cheaper, safer and more convenient. An agent can compare a thousand times more offers than a person, without an aggregator in the middle that skews the comparison to whoever pays it – LLMs make aggregators unnecessary, as they aggregate offers themselves, cheaply. An agent can protect its owner from manipulation. And everybody can have a concierge.

But AI also breaks trust in the networks we use to reach each other. It can clone a voice and a face, and phone numbers and email addresses are easy to spoof. Anybody who has your number or email can target you, or pretend to be somebody you know – and an agent can be deceived the same way.

## What has to be true

For this opportunity to become an open market again, three things must hold:

1. Sellers can be found and trusted.
2. Buyers can ask without being identified and profiled.
3. Nobody sits in between to watch or tax the deal.

## Proposed solutions

**A public square.** Sruly Rosenblat, Ilan Strauss and Tim O'Reilly propose a public space where agents broadcast their requests, and sellers' agents answer[^square]. But a request tells who wants what, when and for how much – it is the most private part of commerce, and broadcasting makes it public. The authors list privacy loss among the risks themselves. It should be the opposite: offers are public, and requests are private.

**Platforms.** We saw what happens when a platform owns the market, and agents on platforms change nothing: the platform still decides who gets found, sees every buyer and takes its cut. Meta's business agents already serve customers inside WhatsApp and Messenger[^meta], and Shopify's agent API requires an agent profile with every request[^ucp].

**Websites with MCP.** An agent can call a seller's website directly, but every request shows its IP address, and to get a reply later it needs an account – a registration that is hard for an agent, and that identifies the buyer. And it still finds sellers through private search.

## What we are building instead

SimpleX is the only messaging network without any user identifiers – no phone numbers, usernames, account IDs or even random IDs[^simplex]. Nobody can contact you, or pretend to be somebody you know, unless you gave them a link. The app was downloaded more than 3 million times, and over 600,000 people use it every month[^users].

For agents, SimpleX meets all three conditions:

1. Sellers publish a contact address and a channel under SimpleX names that only their owners control[^names], and websites can mark these addresses for agents[^discovery].
2. A buyer's agent reads sellers' channels unseen[^channels], and contacts each seller with a new random profile – a private two-way channel with that seller, effectively a throwaway account. Private message routing hides its IP address from the seller.
3. Nobody sits in between: SimpleX needs no accounts, and its servers relay encrypted messages without knowing who sends them to whom.

Already working: channels, business addresses and the directory; test names, with `.simplex` names on sale from 12 December 2026[^sale]; an MCP server that connects any agent to SimpleX conversations, with its owner approving each connection and message in the app[^mcp]; and open specifications for websites to publish their SimpleX addresses and for agents to call each other's tools over SimpleX[^protocols].

Next: catalogues of sellers on SimpleX names, curated by people and LLMs; agents calling each other's tools with a single request and response, without a conversation; custom interfaces inside chats; and names operated by the SimpleX Network Consortium[^consortium], not by one company.

If another network passes all three tests, I would like to know which.

## Join us, own it

Businesses can publish a channel and a contact on SimpleX today, and agent builders can connect their agents. To own a part of the network where agents will trade, invest in SimpleX Chat's community round on Wefunder[^wefunder].

---

[^aup]: Leiner, Cerf, Clark et al., [A Brief History of the Internet](https://www.internetsociety.org/internet/history-internet/brief-history-internet/), Internet Society, 1997.

[^law]: Public Law 102-476, 23 October 1992: networks "which may be used substantially for purposes in addition to research and education in the sciences and engineering" ([text](https://www.govinfo.gov/content/pkg/STATUTE-106/pdf/STATUTE-106-Pg2297.pdf)). The NSFNET backbone was decommissioned on [30 April 1995](https://www.livinginternet.info/doc/merit.edu/phenom.html).

[^cern]: "CERN relinquishes all intellectual property rights to this code, both source and binary", 30 April 1993: [Licensing the Web](https://home.cern/science/computing/the-birth-of-the-web/licensing-web/), CERN.

[^catalog]: The best known was The Whole Internet User's Guide & Catalog, published by Tim O'Reilly's company in 1992; it sold more than a million copies ([Brian Erwin](https://www.oreilly.com/content/monty-python-and-the-dawn-of-internet-marketing/), O'Reilly, 2016). In 1993 the company launched GNN – in O'Reilly's words, "the first commercial website" ([opening remarks at Bellagio](https://ai-disclosures.org/assets/bellagio/Tim-OReilly-Opening-Remarks-Bellagio-2026.pdf), 28 April 2026).

[^clicks]: Rock, Strauss, O'Reilly and Mazzucato, [Behind the Clicks: Can Amazon allocate user attention as it pleases?](https://www.ucl.ac.uk/bartlett/public-purpose/wp2023-11), UCL IIPP working paper 2023-11.

[^square]: Sruly Rosenblat, Ilan Strauss and Tim O'Reilly, [Agents Need a Public Square](https://asimovaddendum.substack.com/p/agents-need-a-public-square), 5 June 2026.

[^meta]: "more than 1 million businesses using them to talk to their customers or complete sales every week": Meta, [Q2 2026 prepared remarks](https://s21.q4cdn.com/399680738/files/doc_financials/2026/q2/META-Q2-2026-Prepared-Remarks.pdf), 29 July 2026.

[^ucp]: [Storefront MCP](https://shopify.dev/docs/agents/catalog/storefront-mcp), Shopify: every request includes an agent profile (`meta.ucp-agent.profile`).

[^simplex]: [How SimpleX works](https://simplex.chat/docs/simplex.html).

[^users]: [SimpleX Chat on Wefunder](https://simplex.chat/wefunder.md), October 2026.

[^names]: [SimpleX public names](https://simplex.chat/blog/20260722-simplex-public-names.html): "Only you control the name with the key in your wallet".

[^discovery]: [Publishing SimpleX addresses on websites](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/protocol/web-discovery.md).

[^channels]: [SimpleX channels](https://simplex.chat/blog/20260430-simplex-channels-v6-5-consortium-crowdfunding-freedom-of-speech.html): "Anybody can join a public channel via its link and see what is sent, but not who sent it, and not who else is reading."

[^sale]: [simplex.domains](https://simplex.domains/).

[^mcp]: [@simplex-chat/mcp](https://www.npmjs.com/package/@simplex-chat/mcp).

[^protocols]: [Publishing SimpleX addresses on websites](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/protocol/web-discovery.md) and [Agent protocols over SimpleX](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/protocol/agent-protocols.md).

[^consortium]: An agreement between the non-profit SimpleX Network Foundation and SimpleX Chat: [SimpleX channels, the consortium and crowdfunding](https://simplex.chat/blog/20260430-simplex-channels-v6-5-consortium-crowdfunding-freedom-of-speech.html).

[^wefunder]: [wefunder.com/simplex.chat](https://wefunder.com/simplex.chat?utm_source=blog).
