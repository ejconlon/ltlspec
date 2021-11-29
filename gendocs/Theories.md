# Ping Theory

    (* A ping request sent by an actor *)
    SentPing : Set
    (* A pong response received by an actor *)
    RecvPong : Set

    (* Characterizes a request-response pair *)
    IsPingPong : SentPing → RecvPong → Prop

    (* Every ping request eventually gets a pong response *)
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

    (* The id of an actor representing a hakker (philosopher) *)
    Hakker : Set
    (* The id of an actor representing a chopstick *)
    Chopstick : Set
    (* A message sent by a hakker *)
    HakkerMsg : Set
    (* A message sent by a chopstick *)
    ChopstickMsg : Set

    FromAdjacent : Chopstick → HakkerMsg → Prop
    InitiallyThinking : Hakker → Prop
    IsEating : Hakker → Prop
    IsHungry : Hakker → Prop
    ReceivedNotDelivered : Chopstick → HakkerMsg → Prop

    (* All hakkers that are initially thinking should eventually start eating *)
    liveness : 
      □ (∀ (h : Hakker), 
        (InitiallyThinking h) ⇒ (◇ (IsEating h)))
    (* All messages a chopstick received come from its adjacent hakkers *)
    receiveFromAdjacentHakkers : 
      □ (∀ (c : Chopstick) (hm : HakkerMsg), 
        (ReceivedNotDelivered c hm) ⇒ (FromAdjacent c hm))

