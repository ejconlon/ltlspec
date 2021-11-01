# Milestone 1
2021-10-29

## Deliverables

These were our deliverables from the proposal

* Theory language (syntax)
  * We will offer a short description (a few paragraphs) of the constructs we support in the Theory, and we will suggest a plausible syntax for expressing them.
* Example domain (actor-based system)
  * Theory for the domain
    * We will define a Theory (in the above textual format) that contains some interesting propositions and axioms one might need to verify operation of an actor-based system. (NOTE: We've chosen to use a better and more useful Haskell datatype format instead, from Milestone 3. See note below.)
  * Synthetic logs or interesting scenarios
    * We will document some interesting scenarios that one would encounter when trying to verify these propositions.
    * We will generate some synthetic logs that will demonstrate the occurrence of some of these scenarios. The code to generate these logs will be in our test suite, as the logs will serve as test input.

### Evaluation

These delierables will be explained below. Some represent code that exists in this repository. See `README.md` for instructions on building and running (tldr: install the `stack` build tool and run `make test`). Some parts (like the traces) are declared as values in the code and don't themselves have meaningful output, but they can be inspected on the REPL `make ghci`. They will eventually be used in our test suite.

## Theory language

We offer two views on syntax for expressing a Theory, one textual for presentation and the other as an internal Haskell datatype for processing.

The first follows `Rabe 2006` in a textual presentation similar to LF. We do not support declarations of additional sorts, only types, propositions, and axioms. `Set` and `Prop` are the only two sorts defined. For now, we only support concrete base types, not type constructors nor polymorphic types. A theory in this form consists of a sequence of type definitions of the form `<Identifier> : <Sort>` (where we use `<>` here to indicate classes of elements for replacement). There are three classes of declarations:

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

```haskell
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

```haskell
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

We also offer a number of combinators such as:

```haskell
-- | A prop that holds at every timestep. If it is ever false, the prop is false.
propAlways :: Prop -> Prop
propAlways = PropRelease PropFalse

-- | A prop that will hold at some timestep. If it is ever true, the prop is true.
propEventually :: Prop -> Prop
propEventually = PropUntil PropTrue

-- | Propositional implication: r1 -> r2
propIf :: Prop -> Prop -> Prop
propIf = PropOr . PropNot
```

Propositions are defined [here](https://github.com/ejconlon/ltlspec/blob/master/src/Ltlspec.hs).

## Example domains

We have three example domains of actor-based systems: a ping example, a Chat system, and the Dining Philosophers problem.

In all examples we define either a type of messages observed to be exchanged between actors or a type of actions or events that may include exchanged messages. We then define a unique state type that will be used in the quantification of data varables and evaluation of atomic propositions in the domain's Bridge for its Theory. Depending on the complexity of the theory, we can define a world for the theory as the most recently seen action (message/event), pairs of `(action, new state)`, or triples of `(old state, action, new state)` With a simple update function from `action -> state -> state` we can scan lists of actions to lists of aforementioned triples (see `SAS` and `scanSAS` [here](https://github.com/ejconlon/ltlspec/blob/master/src/Ltlspec.hs)).

In place of delivering the "textual" theory for each domain, we believe we have delivered something better, which is that we've already encoded each theory as values of a `Theory` datatype in Haskell. We did this because we found it useful to have some machine-checked structure enforced in our encoding. This was work promised in Milestone 3 that is ready for this Milestone. We anticipate formatting portions of these theories textually for our final report, since they're easier to read and understand that way.

### Ping example

This example consists of actors sending "ping" messages to each other, expecting "pong" responses. The theory states that eventually all pings get pongs.

The system is modeled [here](https://github.com/ejconlon/ltlspec/blob/master/src/Ltlspec/Models/Ping.hs).
The theory for this system is `pingTheory` and a synthetic manual trace can be found in `pingMessagesOk`.

### Chat system

We model a Chat system with a single server and multiple clients. Clients initiate the following actions by directly sending messages to the server:

1. Join a channel
2. List members of the channel
3. Send a chat message to all members of the channel
4. Leave the channel

The server sends acknowledgements and responses to these operations and forwards chat messages to all other members of the system.

The system is modeled [here](https://github.com/ejconlon/ltlspec/blob/master/src/Ltlspec/Models/Chat.hs).
The theory for this system is `chatTheory` and a generated trace can be found in `systemTrace`.

### Dining Philosophers

We model the Dining Philosophers problem with actors for each philosopher and chopstick.

The system is modeled [here](https://github.com/ejconlon/ltlspec/blob/master/src/Ltlspec/Models/DinningHakker.hs).
The theory for this system is `dinningHakkerTheory` and a generated trace can be found in `dhtrace3`.
