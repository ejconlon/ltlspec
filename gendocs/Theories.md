# Ping Theory

    (* A pong message sent by an actor *)
    SentPing : Set
    (* A pong message received by an actor *)
    RecvPong : Set
    IsPingPong : SentPing → RecvPong → Prop
    isResponsive : □ (∀ (m1 : SentPing), ◇ (∃ (m2 : RecvPong), IsPingPong m1 m2))

# Chat Theory

    ClientID : Set
    ChannelID : Set
    ActionID : Set
    ChannelListNote : ActionID → ClientID → ChannelID → Prop
    IsMember : ClientID → ChannelID → Prop
    IsSameClient : ClientID → ClientID → Prop
    Joined : ActionID → ClientID → ChannelID → Prop
    Left : ActionID → ClientID → ChannelID → Prop
    ListRequested : ActionID → ClientID → Prop
    NewJoinNote : ActionID → ClientID → ChannelID → ClientID → Prop
    NewLeaveNote : ActionID → ClientID → ChannelID → ClientID → Prop
    Sent : ActionID → ClientID → ChannelID → Prop
    Shared : ActionID → ClientID → ClientID → Prop
    IfInChannelReceiveMessage : □ (∀ (c1 : ClientID) (ch : ChannelID) (c2 : ClientID) (m : ActionID), ((¬ (IsSameClient c1 c2)) ∧ (IsMember c1 ch) ∧ (IsMember c2 ch) ∧ (Sent m c1 ch)) ⇒ ((¬ (Shared m c1 c1)) ∧ (◇ (Shared m c1 c2))))
    IsMemberBetweenJoinAndLeave : □ (∀ (c : ClientID) (ch : ChannelID), ∃ (i : ActionID) (j : ActionID), (Joined i c ch) ⇒ (U (IsMember c ch) (Left j c ch)))
    NeverSendMessageToMyself : □ (∀ (c : ClientID) (m : ActionID), ¬ (Shared m c c))

# Dining Philosophers Theory

    HakkerId : Set
    ChopstickId : Set
    TimeStamp : Set
    HakkerMsg : Set
    ChopstickMsg : Set
    fromAdjacent : ChopstickId, HakkerMsg → Prop
    isEating : HakkerId → Prop
    isHungry : HakkerId → Prop
    isThinking : HakkerId → Prop
    receivedNotDelivered : ChopstickId, HakkerMsg → Prop
    Liveness : □ (∀ (h : HakkerId), (isThinking h) ⇒ (◇ (isEating h)))
    ReceiveFromAdjacentHakkers : □ (∀ (c : ChopstickId) (hm : HakkerMsg), (receivedNotDeliverd c hm) ⇒ (fromAdjacent c hm))

