# Milestone 2
2021-11-15

## Deliverables

These were our deliverables from the proposal:

* Bridge interface definition
  * We will suggest an interface (as a set of Haskell type definitions) between the Theory, Bridge, and LTL Verifier.
* Working LTL verifier
  * We will have an LTL interpreter in Haskell that correctly passes a suite of unit tests that exercise various features of the LTL-FO+, including quantification and user-defined propositions.

We have delivered both items in this milestone as detailed below.

## Bridge interface

We use this interface for the bridge (present [here](https://github.com/ejconlon/ltlspec/blob/18489478c18e564184d11b5ed54e2d80c71676d9/src/Ltlspec/Types.hs#L143)):

```haskell
-- | A 'Bridge' is something that can eval props and quantify in a given world.
-- `w` is world type, `e` is error type, `v` is value type.
-- Typeclass-wise we associate instances with the world type. `w -> e v` means
-- the world type determines the others.
-- This is an "interpretation" in the logic sense.
class Bridge e v w | w -> e v where
  -- | Evaluate the atomic proposition or fail.
  bridgeEvalProp :: w -> Atom v -> Either e Prop
  -- | Quantify over all values of the given type or fail.
  bridgeQuantify :: w -> TyName -> Either e [v]
```

The two key operations are interpreting propositions (`bridgeEvalProp`) and enumerating values (`bridgeQuantify`). Note that we currently allow users to expand atomic propositions into complex propositions (`Prop` not `Bool`). We also allow bridges to raise errors (the `e` part of `Either e _`) to allow for runtime checking of type names and atom values.

An example implementation for the "Chat" domain that was introduced last milestone can be found [here](https://github.com/ejconlon/ltlspec/blob/18489478c18e564184d11b5ed54e2d80c71676d9/src/Ltlspec/Models/Chat.hs#L265).

## LTL verifier

We have implemented a basic LTL verifier:

* An LTL proposition verifier for a single world ([link](https://github.com/ejconlon/ltlspec/blob/790de377d53d132f43bc96e6d7d45060558f744c/src/Ltlspec.hs#L199))
* Folding over a sequence of worlds to evaluate a proposition ([link](https://github.com/ejconlon/ltlspec/blob/790de377d53d132f43bc96e6d7d45060558f744c/src/Ltlspec.hs#L315))
* A simple bridge for testing ([link](https://github.com/ejconlon/ltlspec/blob/790de377d53d132f43bc96e6d7d45060558f744c/src/Ltlspec/Test/Main.hs#L61))
* Test cases for some simple scenarios ([link](https://github.com/ejconlon/ltlspec/blob/e0013fcf825f37d9e1bc7c40e33fc2279095792f/src/Ltlspec/Test/Main.hs#L103))
