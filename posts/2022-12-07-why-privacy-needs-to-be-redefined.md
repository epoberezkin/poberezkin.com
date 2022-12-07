---
title: "Why privacy needs to be redefined"
author: Evgeny Poberezkin
tags: privacy, internet
reddit: r/SimpleXChat/comments/zf0j3x/why_privacy_needs_to_be_redefined/
---

It’s been more than a year since I wrote here. Since December 2021 I’ve been working on SimpleX Chat [^simplex] non-stop.

I said many times that SimpleX is not only one of the most private communication platforms in existence [^tob], it also redefines the meaning of privacy for its users, and delivers the level of metadata privacy no other communication platform can because of their design decisions.

Quite a few people criticized me for that, saying that you cannot really redefine what privacy means, it has a fixed meaning and you either have it or not.

This is what this post is about.

## Why privacy matters

**What do we mean by privacy and why do we believe it’s important?**

Cambridge dictionary [defines](https://dictionary.cambridge.org/dictionary/english/privacy) privacy as _"someone's right to keep their personal matters and relationships secret"_. Wikipedia [extends it](https://en.wikipedia.org/wiki/Privacy) a bit further: _"Privacy is the ability of an individual or group to seclude themselves or information about themselves, and thereby express themselves selectively"_.

When we asked our users why it is important to them, many said that it gives them the feeling of safety, as they know that what they write or even read, meaning no harm and within the boundaries of the law, cannot be used against them.

**What negative consequences can we have because of the lack of privacy?**

It starts from relatively benign violations of privacy via manipulative ads, that exploit people weaknesses, making them buy the things they don’t need, and price discrimination, that not only makes wealthy people pay higher prices, or makes people in urgencies buy more expensive airline tickets, but also makes poor people pay much higher prices for financial services and insurance – so called "poverty premium" [^wagestream].

These consequences continue with the manipulation of elections [^ca]. The violation of privacy doesn't just endanger our economics, they endanger democracy itself.

But it doesn’t stop here. People can be prosecuted solely based on their associations. The most shocking, life-changing consequences are described in the memoir of Mohamedou Ould Slahi, recently shot as a movie The Mauritanian [^slahi]. When he was released after 14 years in prison the judge wrote in his opinion: _"... associations alone are not enough, of course, to make detention lawful."_

## Is Fourth Amendment enough?

Here comes the important part: our associations can damage our lives beyond any reasonable expectations. Historically we felt like if the content of what we read or write is protected, then our privacy is protected. The relevant laws include [the 4th amendment to the US constitution](https://en.wikipedia.org/wiki/Fourth_Amendment_to_the_United_States_Constitution) that says:

> _The right of the people to be secure in their persons, houses, papers, and effects, against unreasonable searches and seizures, shall not be violated, and no Warrants shall issue, but upon probable cause, supported by Oath or affirmation, and particularly describing the place to be searched, and the persons or things to be seized._

In the end of the 18th century, when the dominating form of communication was paper mail, delivered by postal couriers, these protections were sufficient. People knew that if the envelope was not tampered with, it meant that their privacy was protected. Postal service was not able to determine all associations from the fact of delivering messages, and many envelopes did not have a return address, so even if the postal courier could see who the mail is delivered to, they could not see who it is from.

## E2E encryption is not bulletproof

Nowadays, for most people the postal service is replaced by large technology companies who have full visibility and control on who sends what to whom and when. Some communication platforms promise end-to-end encryption (for example, WhatsApp and Signal) and many people trust them, but before considering why it is not enough, let’s look at the end-to-end encryption.

Many people believe, quite religiously, that if the communication is end-to-end encrypted then it absolutely cannot be tampered with while in transit. What is end-to-end encryption? It is the application of cryptography to the content of the message that requires that the sender and the recipient somehow agree on the encryption key to use. It is not that different from putting your mail into a box with a complex lock on it, and somehow passing the key and the box to the recipient [^e2e]. If you then were to pass the box via the postal courier, then you probably wouldn’t want to pass the key to open the box via the same courier – it would be sensible to pass the key in some other way, maybe send it in a separate letter.

But when we use electronic communications this common sense betrays us and we use the same courier to pass the key as we use to pass the messages locked with the key. Although the modern cryptography is designed in such a way that the courier cannot decrypt the message if it was encrypted with the recipient key, almost nothing stops the courier from intercepting the key of the recipient and replacing it with their own key [^mitm], and then the courier would decrypt the message in transit and re-encrypt it with the recipient key for the delivery.

I am not saying that every single communication service performs such an attack on their users, and some of them (for example, Signal) allow the users to validate the keys by comparing the security codes [^verify]. But they do have technical ability to break into end-to-end encryption unless some alternative channel was used to pass the key, so a widely shared belief that end-to-end encryption cannot be tampered with is simply incorrect.

Even if end-to-end encryption is trustworthy (e.g., we verified the keys), and even if we do not use our real identities and do not use the phone numbers, with the current level of technology it is still not enough to provide sufficient privacy. The communication operators, and in some cases network observers, can see which user profile sends messages to which. Even if users of the network are identified by some random numbers (as is the case in Session, for example), it still makes all connections between the users observable.

The operator, or any attacker who got hold of this data, can now apply machine learning to compare communication patterns in this pseudonymous network with the communication patterns in the existing public networks (e.g., Twitter, Facebook, etc.) and correlate pseudonious identities with the real ones, with variable degree of certainty. So the names and associations that the users believe to be private can become known to the service providers, observers and attackers.

## How SimpleX design redefines privacy

Given the above, we should no longer say that privacy of content and of the real names only is "privacy" – it is only a required but not a sufficient part of what the communication platform should provide to its users to be considered "private". The other necessary part is protecting the privacy of the associations, even if they are pseudonymous. It can be achieved is by avoiding the use of any persistent user addresses or identifiers that are used to deliver messages [^address]. This is why and how we designed SimpleX – by using only temporary identifiers for the connections between the users [^design].

The way SimpleX operates can be compared with having multiple email addresses, two for each contact, when one address being used only to send messages, and another only to receive – and these addresses will soon automatically switch between different servers (currently they can be switched manually). Such communication via email would be quite difficult to manage – SimpleX clients automate it. You can see a diagram of how the SimpleX network operates on [the website](https://simplex.chat/#simplex-explained) and read more in [the whitepaper](https://github.com/simplex-chat/simplexmq/blob/stable/protocol/overview-tjr.md).

I hope to see that some other communication platforms adopt a design that avoids persistent user profile identifiers, and instead use [pairwise identifiers](https://csrc.nist.gov/glossary/term/Pairwise_Pseudonymous_Identifier) among other [privacy enhancing technologies](https://en.wikipedia.org/wiki/Privacy-enhancing_technologies). It is not too difficult technically, and I believe that such design will become the absolute minimum of what the communication network should adopt to be considered private.

This is how we should redefine privacy.

[^simplex]: [SimpleX Chat](https://simplex.chat) is the first, and probably the only, messaging platform that has no user profile identifiers of any kind – not even random numbers are used (we literally do not know a number of users who use our servers).

[^tob]: SimpleX Chat security has just been [assessed by Trail of Bits](https://simplex.chat/blog/20221108-simplex-chat-v4.2-security-audit-new-website.html), a US security and technology consulting company that has big tech companies, major blockchain projects and governmental agencies as its clients.

[^wagestream]:  [Wagestream](https://wagestream.com/en/), the company where I led the engineering team for a year prior to SimpleX Chat, aims to reduce [poverty premium](https://fairbydesign.com/povertypremium/), helping more than a million front-line employees in the UK, US and Australia to avoid expensive pay-day loans.

[^ca]: We have only recently observed how our Facebook connections and activity was [exploited by Cambridge Analytica](https://en.wikipedia.org/wiki/Facebook–Cambridge_Analytica_data_scandal) to manipulate the outcome of the US elections.

[^slahi]: He spent more than 14 years in Guantanamo prison because of his past association with Al Qaeda that he severed all ties with 9 years prior to 9/11 attacks. [Wikipedia](https://en.wikipedia.org/wiki/Mohamedou_Ould_Slahi).

[^e2e]: This is a simplification of course, and in modern cryptography the actual key used to decrypt the message is not passed – either only the encryption key is passed (which is different from the decryption key that is kept private – as happens with [RSA cryptography](https://en.wikipedia.org/wiki/RSA_(cryptosystem))) or both sides pass the public parts of their key pairs to each other and compute a shared secret using the received public part and the private part they never sent (it happens this way in [Diffe-Hellman exchange](https://en.wikipedia.org/wiki/Diffie–Hellman_key_exchange)).

[^mitm]: It is referred to as [man-in-the-middle attack](https://en.wikipedia.org/wiki/Man-in-the-middle_attack) (MITM). 

[^verify]: It allows you to validate that the key was not substituted by comparing the secrets you and your contact have in the app (e.g. by sending it via another channel or by scanning a QR code).

[^address]: Although, such a network can have temporary addresses for user discovery, without compromising privacy.

[^design]: In fact, there are 4 different identifiers, because each connection consists of two unidirectional messaging queues, commonly on two different servers, and each queue has different addresses to send and to receive the messages, so even if the transport connection security is compromised, there will be no shared identifiers or ciphertext between the received and sent traffic of the server.
