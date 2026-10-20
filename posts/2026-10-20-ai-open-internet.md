---
title: "Can AI Agents Give Us the Internet Back?"
author: Evgeny Poberezkin
tags: ai, internet
---

They can – but not on the networks we use today.

## Internet: a market that nobody owned

When the internet began, commerce was not allowed on it. In 1992 Congress opened it to other uses, and in 1993 CERN gave up all rights to the web's code[^history].

Anybody could build on it, publish and read, and people found services in catalogs[^catalog].

## How we lost it to big tech

Catalogs did not scale: people curated them manually, and the web grew too fast for that to work. Search replaced them, and a few companies came to own it: they fill the top results with paid placements, which people click even when the products are more expensive or worse rated[^clicks]. And software was expensive, so only large companies could afford to build the services people used.

The market became a few walled gardens. Their owners decide who gets found and charge for it, see every buyer and take a cut of the deals.

## AI changes the game

AI can solve the three problems that closed the market:

- catalogs can scale again, because LLMs can curate and search them;
- search indices can become a public good, because data is now cheap;
- creating software costs a fraction of what it did, so anybody can build a service.

Agents can also make buying cheaper, safer and more convenient: an agent can compare a thousand times more offers than a person, without an aggregator skewing the comparison toward whoever pays; it can protect its owner from manipulation; and it gives everybody what only a few could afford – a personal concierge.

But AI also breaks trust in the networks we use. It can clone a voice and a face, and phone numbers and email addresses are easy to spoof. Anybody who has your number or email can target you, or pretend to be somebody you know – and an agent can be deceived the same way.

## What keeps a market open

A market stays open only while three things hold:

1. **Sellers can be found and trusted.** When anybody can be faked, a seller's reputation should be attached to a trusted name, or agents will buy from impostors.
2. **Buyers can ask without being identified.** A request tells who wants what, when and for how much. Whoever learns that can raise the price for the buyer who needs it most.
3. **Nobody sits in between.** A middleman decides who gets found and takes a cut of every deal – this is how the open market closed last time.

## Why today's proposals fail

**A public square.** Rosenblat, Strauss and O'Reilly propose a public space where agents broadcast their requests, and sellers' agents answer[^square]. But broadcasting makes every request public, so buyers can no longer ask without being identified. The authors list privacy loss among the risks themselves. It should be the opposite: offers are public, and requests are private.

**Platforms.** We saw what happens when a platform owns the market, and agents on platforms change nothing: the platform still decides who gets found, sees every buyer and takes its cut. Meta's business agents, for example, already serve customers inside WhatsApp and Messenger[^meta].

**Websites with MCP.** An agent can call a seller's website directly, but every request shows its IP address, and to get a reply later it needs an account – a registration that is hard for an agent, and that identifies the buyer. And it still finds sellers through search that big tech owns.

## We built SimpleX for people – it is ready for agents

We have been building SimpleX since 2020 for private communication between people. It is the only messaging network without any user identifiers[^simplex]: nobody can contact you, or pretend to be somebody you know, unless you give them a link. Over 600,000 people use it every month.

The same design is what an open market for agents needs. Sellers can publish their addresses and channels under SimpleX names that only they control[^names]. Buyers' agents can read these channels unseen[^channels] and contact each seller with a new random profile and a hidden IP address – a throwaway account for one conversation. And nobody sits in between: the servers relay encrypted messages without knowing who sends them to whom – and that includes us: SimpleX Chat licensed the network protocols to the SimpleX Network Consortium[^consortium].

Businesses already support their customers on SimpleX. Test names work now, and `.simplex` names go [on sale on December 12, 2026](https://simplex.domains/). We have just released an MCP server[^mcp] that connects any agent to SimpleX, with its owner approving its connections and messages in the app, and published open specifications for websites and agents[^specs].

## What the market can become

On this network, the market can open again. Sellers publish offers under names nobody can take away, in catalogs that LLMs curate and search. A buyer's agent reads thousands of them unseen, asks the best sellers privately, and buys with no platform taking a cut. To get there, we are building:

- catalogs of sellers on SimpleX names, curated by people and LLMs;
- agents calling each other's tools with a single request over SimpleX, without creating a conversation;
- custom interfaces inside chats; and
- an autonomous namespace, so that no company, including ours, can take a seller's name away.

## Join us and own it

Tell us what you need from this network, and what is missing: [write to us on SimpleX](https://smp6.simplex.im/a#lrdvu2d8A1GumSmoKb2krQmtKhWXq-tyGpHuM7aMwsw). And so that its users can own part of it, SimpleX Chat is raising a [community round on Wefunder](https://wefunder.com/simplex.chat?utm_source=blog).

---

[^history]: The NSFNET backbone policy prohibited its use "for purposes 'not in support of Research and Education'" ([A Brief History of the Internet](https://www.internetsociety.org/internet/history-internet/brief-history-internet/)). Public Law 102-476 of October 23, 1992 allowed networks to be used "substantially for purposes in addition to research and education" ([text](https://www.govinfo.gov/content/pkg/STATUTE-106/pdf/STATUTE-106-Pg2297.pdf)). On April 30, 1993, CERN declared: "CERN relinquishes all intellectual property rights to this code" ([Licensing the Web](https://home.cern/science/computing/the-birth-of-the-web/licensing-web/)).

[^catalog]: The best known was The Whole Internet User's Guide & Catalog, published by Tim O'Reilly's company in 1992; it sold more than a million copies ([Brian Erwin](https://www.oreilly.com/content/monty-python-and-the-dawn-of-internet-marketing/), O'Reilly, 2016). In 1993 the company launched GNN – in O'Reilly's words, "the first commercial website" ([opening remarks at Bellagio](https://ai-disclosures.org/assets/bellagio/Tim-OReilly-Opening-Remarks-Bellagio-2026.pdf), April 28, 2026).

[^clicks]: Rock, Strauss, O'Reilly and Mazzucato, [Behind the Clicks: Can Amazon allocate user attention as it pleases?](https://www.ucl.ac.uk/bartlett/public-purpose/wp2023-11), UCL IIPP working paper 2023-11.

[^square]: Sruly Rosenblat, Ilan Strauss and Tim O'Reilly, [Agents Need a Public Square](https://asimovaddendum.substack.com/p/agents-need-a-public-square), June 5, 2026.

[^meta]: "more than 1 million businesses using them to talk to their customers or complete sales every week": Meta, [Q2 2026 prepared remarks](https://s21.q4cdn.com/399680738/files/doc_financials/2026/q2/META-Q2-2026-Prepared-Remarks.pdf), July 29, 2026.

[^simplex]: No phone numbers, usernames, account IDs or even random IDs. Instead of identifying users, SimpleX assigns random identifiers to each connection – separate ones for sending and receiving – so only the users know who they talk to: [How SimpleX works](https://simplex.chat/docs/simplex.html).

[^names]: [SimpleX public names](https://simplex.chat/blog/20260722-simplex-public-names.html): "Only you control the name with the key in your wallet."

[^channels]: [SimpleX channels](https://simplex.chat/blog/20260430-simplex-channels-v6-5-consortium-crowdfunding-freedom-of-speech.html): "Anybody can join a public channel via its link and see what is sent, but not who sent it, and not who else is reading."

[^mcp]: Start an agent with `npx -y @simplex-chat/mcp serve my-agent` and join its chat in the SimpleX app: the agent asks there before it connects to anybody or sends a message, and you approve with one tap. It works with Claude Code now: [@simplex-chat/mcp](https://www.npmjs.com/package/@simplex-chat/mcp).

[^specs]: A website marks its SimpleX addresses with the `simplex` link relation and lists them in `/.well-known/simplex.json` ([Publishing SimpleX addresses on websites](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/protocol/web-discovery.md)). Agents call each other's tools with ordinary MCP messages, sent in a chat with `/_call` or as a single request ([Agent protocols over SimpleX](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/protocol/agent-protocols.md)).

[^consortium]: The [SimpleX Network Consortium](https://simplexnetwork.org/consortium.html) is a perpetual and irrevocable agreement between the nonprofit SimpleX Network Foundation and SimpleX Chat for protocol licensing and governance; the governance is moving to it. Once the power over the protocols moves away from the company, it cannot move back ([SimpleX Channels, SimpleX Network Consortium and Community Crowdfunding](https://simplex.chat/blog/20260430-simplex-channels-v6-5-consortium-crowdfunding-freedom-of-speech.html)).
