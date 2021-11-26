---
title: "LTLSpec"
subtitle: "An extensible LTL verifier for distributed systems"
author: "Eric Conlon, Yanze Li, Tarcisio Teixeira"
date: "2021-12-07"
toc: false
section-titles: false
theme: "Rochester"
colortheme: "default"
fonttheme: "professionalfonts"
fontsize: "14pt"
linkstyle: "bold"
---

# Our project

- Implemented an LTL verifier
- Implemented several actor (message-passing) systems
- Defined an LTL theory for each system
- Verified execution traces of those systems
- See the [ltlspec repo](https://github.com/ejconlon/ltlspec)

# Our assumptions

- Can gather local or global traces of actions/messages in a distributed system
- Traces don't violate causality
- Traces start at the beginning
- Domains are finite (per-world)

# What is Linear Temporal Logic (LTL)?

- World-relevant propositional logic
- Atomic propositions (`P`, `Q`)
- Standard connectives
  - $\land$, $\lor$, $\neg$, $\implies$
- Modal connectives
  - $\Box$ (Always), $\diamond$ (Eventually)

# LTL Theories

- TODO

# Making LTL useful

- Add data variable quantification (`LTL-FO+`)
  - $\forall$, $\exists$
  - First order - no quantifying over propositions
- Add user-defined atomic props and quantification
- For each domain require user-defined "bridge"

# Verification is a conversation

- TODO

# Example system: Ping

- Actors send `Ping` messages and respond with `Pong` messages
- We want this responsiveness property to hold:
  - "Whenever I send a `Ping`, I eventually receive a `Pong`"
  - Not enough that a `Pong` was sent to me, I need to receive it!

# Example theory: Ping

- TODO

# Example valid trace: Ping

- TODO

# Example invalid trace: Ping

- TODO

# Encountering the infinite problem

- Modal connectives only have meaning on infinite traces
- Can you say something `Always` happens?
- Can't validate a liveness property with a finite trace...
  - Or can you?
- Trick: allow users to reason outside the logic about ALL future worlds

# Solving the infinite problem

- User bridge tells the verifier that after this world
  - Some set of types will have empty quantification
  - Some set of atomic props will have constant values
- This lets you prove responsiveness for quiescent systems
- For some props the best you can say is you can't prove or disprove them
- Not arbitrary: these criteria ensure no additional reduction is necessary

# Example truncation: Ping

- Trace of actions: `send/recv ping/pong message`
- From initial state, fold into a trace of worlds:
  - `(state before, current action, state after)`
- No more actions: `SentPing` and `RecvPong` types have empty quantification
- `IsPingPong` atomic prop is always a function of its arguments (all worlds)
- Consequence: can evaluate responsiveness on any trace

# Evaluation (1/2)

- Defined messages, roles, and actors for three domains:
  - Ping, Chat System, and Dining Philosophers
- Ran these in our actor framework with multi-threading and STM
  - Implementation and STM semantics ensure trace order does not violate causality
  - Wait for quiescence to ensure total trace collection

# Evaluation (2/2)

- Primary concern: Correctness
  - Collected traces and verified axioms of each theory
  - The tests pass... And we never write bugs!
- Secondary concern: Performance
  - Fine even with naive implementation - not stress-tested
  - Want safe-for-space machine
  - Want term graph deduplication

# Thanks!

`B-)`
