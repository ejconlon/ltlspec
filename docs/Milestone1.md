# Milestone 1
2021-10-29

## Deliverables
* Theory language (syntax)
  * We will offer a short description (a few paragraphs) of the constructs we support in the Theory, and we will suggest a plausible syntax for expressing them.
* Example domain (actor-based system)
  * Theory for the domain
    * We will define a Theory (in the above textual format) that contains some interesting propositions and axioms one might need to verify operation of an actor-based system.
  * Synthetic logs or interesting scenarios
    * We will document some interesting scenarios that one would encounter when trying to verify these propositions.
    * We will generate some synthetic logs that will demonstrate the occurrence of some of these scenarios. The code to generate these logs will be in our test suite, as the logs will serve as test input.

## Theory language

We offer two views on syntax for expressing a Theory, one textual for presentation and the other as an internal Haskell datatype for processing.

The first follows `Rabe 2006` in a textual presentation similar to LF. We do not support declarations of additional sorts, only types, propositions, and axioms. `Set` and `Prop` are the only two sorts defined. For now, we only support base types, not type constructors. A theory in this form consists of a sequence of type definitions of the form `<Identifier> : <Sort>` (where we use `<>` here to indicate classes of elements for replacement). There are three classes of declarations:

1. Type declarations of the form `<TypeName> : Set`
2. User-defined proposition declarations of the form `<PropName> : <TypeName> -> <TypeName> -> Prop`.
3. Axioms of the form `<AxiomName> : <PropositionExpression>`

Propositions are defined with all the (non-weak) connectives of LTL, and they will be enumerated fully in the second presentation below. We will generally use alphanumeric identifiers and capitalize type and prop names. The propositions `Forall` and `Exists` are to be interpreted as dependent function types that bind the given name in their body. Type ascriptions are always required.

We provided an example in our proposal, which I will recall here:

```
# Signature:
# We will quantify over processes and messages
ProcessId, MessageId : Set

# We will validate message sends and logical requests and responses
IsRequest : MessageId -> Prop
IsSent : ProcessId -> ProcessId -> MessageId -> Prop
IsResponse : MessageId -> MessageId -> Prop

# Axioms:
# This property says that all requests will eventually be
# responded to by SOMEONE, not necessarily from the recipient.
isResponsive :
  Always (
    Forall (requestMsgId : MessageId).
    Forall (clientId : ProcessId).
    Forall (serverId : ProcessId).
    IsRequest requestMsgId ->
    IsSent clientId serverId requestMsgId ->
      Eventually (
        Exists (responderId : ProcessId).
        Exists (responseMsgId : MessageId).
        IsSent responderId clientId responseMsgId /\
        IsResponse requestMessageId responseMsgId
      )
  )
```

The second presentation can be found as a set of type definitions in our codebase:

```
type PropName = String
type TyName = String
type AxiomName = String

type TyDefs = [TyName]
type PropDefs = Map PropName [TyName]
type AxiomDefs = Map AxiomName Prop

data Theory = Theory
  { theoryTypes :: !TyDefs
  , theoryProps :: !PropDefs
  , theoryAxioms :: !AxiomDefs
  } deriving stock (Eq, Show)
```

Propositions are defined as follows:

```
type VarName = String

data Atom = Atom !PropName ![VarName]
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

data Binder = Binder !VarName !TyName
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

-- | An LTL proposition with first-order data quantification.
-- This selection of operators corresponds to "Release Positive Normal Form"
data Prop =
    PropAtom !Atom
  -- ^ An atomic prop - use this to embed predicates from your domain
  | PropTrue
  -- ^ The constrant True
  | PropFalse
  -- ^ The constant False
  | PropNot Prop
  -- ^ Logical negation of the prop
  | PropAnd Prop Prop
  -- ^ Logical AND of several props (empty is true)
  | PropOr Prop Prop
  -- ^ Logical OR of several props (empty is false)
  | PropNext Prop
  -- ^ A prop that holds the next timestamp
  | PropUntil Prop Prop
  -- ^ 'PropUntil r1 r2' means 'eventually r2' and at least until 'r2' holds, 'r1' always holds.
  -- If both are false, the prop is false. When 'r2' holds, the prop is true.
  | PropRelease Prop Prop
  -- ^ 'PropRelease r1 r2' means 'always r2' until and including when 'r1' holds.
  -- If 'r2' is false, the prop is false. When 'r1' and 'r2' hold, the prop is true.
  | PropForAll !Binder Prop
  -- ^ 'PropForAll (Binder n t) r' means for all 'n' of type 't' 'r' holds.
  | PropExists !Binder Prop
  -- ^ 'PropForAll (Binder n t) r' means there exists an 'n' of type 't' for which 'r' holds.
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)
```

Substitution on these proposition expressions is defined in the normal way.

## Example domains

We have two example domains of actor-based systems: a Chat system and the Dining Philosophers problem.

### Chat system

We model a Chat system with a single server and multiple clients. Clients initiate the following actions by directly sending messages to the server:

1. Join a channel
2. List members of the channel
3. Send a chat message to all members of the channel
4. Leave the channel

The server sends acknowledgements and responses to these operations and forwards chat messages to all other members of the system.

### Dining Philosophers

We model the Dining Philosophers problem with actors for each philosopher and chopstick.
