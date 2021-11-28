# Ping Theory

    (* A pong message sent by an actor *)
    SentPing : Set
    (* A pong message received by an actor *)
    RecvPong : Set


    IsPingPong : SentPing → RecvPong → Prop


    isResponsive : 
      □ (∀ (m1 : SentPing), 
        ◇ (∃ (m2 : RecvPong), 
          IsPingPong m1 m2))

# Chat Theory

    Client : Set
    Channel : Set
    Action : Set


    ChannelListNote : Action → Client → Channel → Prop
    IsMember : Client → Channel → Prop
    IsSameClient : Client → Client → Prop
    Joined : Action → Client → Channel → Prop
    Left : Action → Client → Channel → Prop
    ListRequested : Action → Client → Prop
    NewJoinNote : Action → Client → Channel → Client → Prop
    NewLeaveNote : Action → Client → Channel → Client → Prop
    Sent : Action → Client → Channel → Prop
    Shared : Action → Client → Client → Prop


    ifInChannelReceiveMessage : 
      □ (∀ (c1 c2 : Client) (ch : Channel) (m : Action), 
        ((¬ (IsSameClient c1 c2)) ∧ (IsMember c1 ch) ∧ (IsMember c2 ch) ∧ (Sent m c1 ch)) ⇒ (◇ (Shared m c1 c2)))
    isMemberBetweenJoinAndLeave : 
      □ (∀ (c : Client) (ch : Channel), 
        ∃ (i j : Action), 
          (Joined i c ch) ⇒ (U (IsMember c ch) (Left j c ch)))
    neverSendMessageToMyself : 
      □ (∀ (c : Client) (m : Action), 
        ¬ (Shared m c c))

# Dining Philosophers Theory

    Hakker : Set
    Chopstick : Set
    TimeStamp : Set
    HakkerMsg : Set
    ChopstickMsg : Set


    FromAdjacent : Chopstick, HakkerMsg → Prop
    IsEating : Hakker → Prop
    IsHungry : Hakker → Prop
    IsThinking : Hakker → Prop
    ReceivedNotDelivered : Chopstick, HakkerMsg → Prop


    liveness : 
      □ (∀ (h : Hakker), 
        (IsThinking h) ⇒ (◇ (IsEating h)))
    receiveFromAdjacentHakkers : 
      □ (∀ (c : Chopstick) (hm : HakkerMsg), 
        (ReceivedNotDelivered c hm) ⇒ (FromAdjacent c hm))

