---
title: "MLS: The Naked King of End-to-End Encryption"
author: Evgeny Poberezkin
tags: security, internet
---

This essay is purposefully provocative. It has no intention to offend anybody. I appreciate that many people worked very hard designing and implementing MLS (Messaging Layer Security), and many of them have a fervent belief that MLS is a necessary technology.

But the value of the technology is not determined by how much was invested, nor by who supports it. The value of technology comes from effectively solving problems for people. By that metric, many simple technical solutions are more valuable than complex ones.

The goal of this essay is to revisit the core problems that MLS aims to solve, and consider whether it solves them well enough. And if it does not solve them well enough, then what areas it can be effectively applied to, and what are the alternatives to MLS.

## What Problem Does MLS Aim to Solve?

The problem that the cryptographers who designed MLS aimed to solve is end-to-end (E2E) encryption in large groups. E2E encryption, by definition, is a cryptographic solution that protects the content of the messages from the communication providers.

A simple approach to E2E encryption in large groups would be for all members to agree on a shared key. But the problem of this approach is the cost of key updates. If the group is really large, then every time a member leaves, the remaining members must agree on a new key so that the member who left cannot decrypt the new messages. With a simple approach the cost of agreeing on the group key grows linearly with the group size, and it does not scale well enough.

## What is the proposed solution?

Instead of agreeing on a new key with remaining members, MLS proposes to represent this shared key as a composition of the tree of keys (a "ratchet tree"), where each member contributes a "leaf" (some part of the key). When a member leaves, the remaining members would only need to update a small part of the tree, reducing the cost of an update from `O(N)` (N is the number of members) to `O(log N)`.

While the essence of the approach sounds very simple, practical implementation of this idea is very complex: it involves a key schedule, proposals/commits for group changes, and handling of pre-shared keys (PSKs) for added entropy.

## Does MLS Work for End-to-End Encryption?

*TL;DR: Yes, if you accept "Trust Me Bro" security model*.

To answer this question, we first need to agree what "to work" means in this context. Let’s start from a rather non-controversial statement: the purpose of E2E encryption is to protect message content from the untrusted communication providers, and to provide effective mechanisms to mitigate the risk of the provider being compromised. So, "to work", any solution for E2E encryption must satisfy those criteria.

Let’s look at the Double Ratchet algorithm, for comparison, that is used for E2E encryption in Signal, SimpleX Chat and many other messengers. Does it protect message content from the untrusted provider? Yes, as long as the provider did not perform man-in-the-middle attack [^mitm] on the key exchange. Does it prevent or mitigate the risk of MITM attack? This is implementation-dependent. Signal allows communication parties to compare security codes to confirm that key exchange was not compromised. SimpleX network servers simply cannot replace the keys, as key agreement requires another channel; in addition, SimpleX also allows comparing security codes, to mitigate the risk that the used channel was compromised. In any case, the Double Ratchet algorithm itself provides the security code that communication parties can compare to confirm the integrity of key exchange. From that we can conclude that the Double Ratchet algorithm is an effective solution for E2E encryption that achieves its goal, and in addition to that it has important security properties: forward secrecy and post-compromise security [^pfs].

Let’s apply the same reasoning to MLS. Does MLS protect message content from the untrusted provider? One of the components of MLS is the "authentication service" &mdash; a component that is supplied by a communication provider. The MLS specification states in [part 16.10](https://www.rfc-editor.org/rfc/rfc9420.html#name-authentication-service-comp):

> A compromised Authentication Service (AS) can assert a binding for a signature key and identity pair of its choice, thus allowing impersonation of a given user. This ability is sufficient to allow the AS to join new groups as if it were that user. Depending on the application architecture, it may also be sufficient to allow the compromised AS to join the group as an existing user, for instance, as if it were a new device associated with the same user.

The MLS specification explicitly requires trust in the communication provider as a condition for MLS securing the message content from the untrusted provider, which is self-contradictory &mdash; **we are required to trust an untrusted party**, which contradicts the purpose of E2E encryption.

Does the MLS specification offer a practical mechanism for the end users to mitigate the effect of a compromised Authentication Service? No, it does not; it only refers to the approaches based on key transparency, but they are not practical in real-world applications, and, to the best of my knowledge, are not implemented in any of the communication platforms that deployed MLS [^whitenoise].

Proponents of MLS would argue that it still provides some security, as it protects past messages from the provider being compromised in the future. But this is an invalid argument, as the expectation that users have about E2E encryption is that the provider is cryptographically unable to compromise it without detection, rather than it is only unable to compromise past messages. And if the provider wants to compromise group security, nothing stops it from infiltrating the group via the authentication service it controls at the group inception.

## Is End-to-End Encryption in Large Groups a Real Problem?

It depends on the definition of "large", but we are assuming that "large" is any group where simple member broadcast to update keys is inefficient, so we are talking about many thousands of members.

In practical scenarios, there are two types of such groups:

- groups comprised of the general public, which are not completely public. E.g., they may have some admission criteria. Practically, the content in such groups is impossible to protect beyond a thousand or so members (coincidentally, it is the limit on group size in Signal), as it becomes increasingly easy to join such a group as it grows. So members of such semi-closed/semi-public groups should not have expectations of content security, and MLS does not solve any real problems in this case. At best, such groups can provide participation privacy, but MLS does not provide it [^participation].
- large enterprise groups. While enterprises must protect their content from the public and from competitors, for every enterprise it is important to retain and to monitor content in their groups, for compliance, knowledge retention, and dispute resolution. Therefore, any E2E encryption would be counter-productive, and instead the enterprise would benefit more from using trusted servers it can control.

The proponents of MLS would argue that it allows E2E encryption in large federated groups. But this is also an invalid argument, as having multiple Authentication Services increases the risk of one of them being compromised, exposing messages of all users, even those that do not use the compromised provider. Federation also makes it harder to mitigate this risk.

I am open to being presented with the scenario where E2E encryption (protection from the untrusted provider) in really large groups could be more beneficial than choosing a secure and trusted provider or using servers under the control of the communication parties.

## Alternatives for Medium-Sized Groups

Signal’s approach for groups up to 1000 members is pragmatic and effective: all members agree pairwise states for the Double Ratchet algorithm. This way the cost of agreeing keys is "front-loaded" &mdash; each new member must agree on a key with each other member before being able to send messages. When a member leaves, no special action is needed, other than notifying all members about it.

Does it mean that each member has to encrypt and send a message 1000 times? No, it would be very inefficient. In Signal, each message is encrypted with one small random symmetric key, and then this small key, and not the whole message, is encrypted separately for each member. Then, a message sender packages all encrypted keys and an encrypted message together into a bundle and sends it to the provider, which forwards the message and one of the encrypted keys to each member.

What is the cost of sending a message with this approach? It depends on the variant of the Double Ratchet, but in any case, it is not more than 100-150kb plus the message size for a group of 1000 members, which is acceptable even on relatively slow internet connections.

How can the risk of the compromised provider be mitigated? In the case of Signal, there is no simple and effective solution to verify all connections, but some members could verify their pairwise security codes. In the case of a decentralized network, it is sufficient to have several unaffiliated servers re-broadcasting messages in the group to mitigate the risk of one of them being compromised &mdash; as long as some of the servers do not collude, they would act as "the second verification channel" to each other. This is the approach considered for medium-sized E2E encrypted groups in SimpleX network.

## Who Benefits From MLS?

This is the most controversial statement, and the reason to make it is not to upset people, but to be proven wrong.

To me, it increasingly looks like the only people who benefit from MLS are its designers and implementers, and not the end users, particularly if MLS is being compared to other, simpler and more secure alternatives.

MLS might have provided some security via its modular design and separation of components &mdash; e.g., if providers explained to end users the difference between Delivery Service component, which may be untrusted, and Authentication Service, which must be trusted, and offered a separate choice of providers for these components.

But without the separation of services on the business level, this separation in design is purely academic rather than practical. Promoting MLS as an effective solution for E2E encryption, without disclosing the requirement for provider trust, is at best misleading, at worst &mdash; fraudulent [^mitigation].

MLS, in its current form, hailed by cryptography experts, large technology companies, and tech media as the next generation of secure messaging, almost the king of encryption, appears to be naked. And, like the naked king's attire, its implementations are quite expensive to create as well.

I would love to hear the arguments about what problems MLS in its current state solves, and how the requirement for provider trust can be mitigated, as so far I have failed to find any coherent answers to these questions &mdash; MLS appears to be a proverbial "solution in search of a problem".

Until then, my strong recommendation to the users is to consider "MLS encryption" as ineffective in protecting message content from communication providers, and use communication products that use other, effective solutions for E2E encryption.

[^mitm]: Man-in-the-middle attack is the attack by the intermediary (e.g., a communication provider) on the key exchange, where instead of breaking the encryption, the attacker simply substitutes the key, allowing it to read all messages between members. You can read more about how a MITM attack is performed and how to mitigate it in [this post](https://simplex.chat/blog/20240314-simplex-chat-v5-6-quantum-resistance-signal-double-ratchet-algorithm.html#5-man-in-the-middle-attack-mitigated-by-two-factor-key-exchange).

[^pfs]: Somewhat counterintuitively, perfect-forward secrecy (PFS) protects past messages, sent before the long-term encryption keys were compromised, and post-compromise security (PCS, also known as break-in recovery) protects future messages, sent after long-term keys were compromised.

[^whitenoise]: A notable exception here is Nostr-based [WhiteNoise](https://www.whitenoise.chat) that avoids the need for an authentication service, relying on user identities being their public keys. The main downside of WhiteNoise remains the lack of participation privacy, as relays have knowledge of all groups where the user participates.

[^participation]: In fact, using MLS is likely to make it harder to provide participation privacy &mdash; centralized authentication service requires a network-wide user identity, so providers would know all groups where a user participates.

[^mitigation]: IETF’s [ongoing work](https://www.rfc-editor.org/rfc/rfc9750.html#name-authentication-service) to find solutions for mitigating a compromised Authentication Service confirms the acceptance of this problem: *"The AS is invested with a large amount of trust and the compromise of the AS could allow an adversary to, among other things, impersonate group members."* But this work has not found any robust solutions yet, its recommendations are not mandatory for MLS implementations, and some of them involve substantial complications of already complex specification, without fully removing AS trust requirement.
