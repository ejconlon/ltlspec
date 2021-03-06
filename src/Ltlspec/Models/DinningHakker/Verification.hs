module Ltlspec.Models.DinningHakker.Verification where

import Data.Either (lefts, rights)
import qualified Data.Map.Strict as M
import Data.Sequence (Seq (..))
import qualified Data.Set as S
import Ltlspec.Models.DinningHakker.Trace (Action (..), Chopstick (..), ChopstickId, ChopstickMsg, GlobalState (..),
                                           Hakker (..), HakkerId, HakkerMsg (..), HakkerState (..), TimeStamp, hkState,
                                           initState, stepPerfect)
import Ltlspec.TriBool (TriBool (..))
import Ltlspec.Types (Atom (..), BinderGroup (..), Bridge (..), Commented (..), Error, Prop (..), SAS (..), SProp (..),
                      Theory (..), TruncBridge (..))

-- avoid orphan instance
newtype DHWorld = DHWorld { unDHWorld :: SAS GlobalState Action }
  deriving stock (Eq, Show)

type Trace = [DHWorld]

genTrace :: [Action] -> GlobalState -> [DHWorld]
genTrace [] _ = []
genTrace (a : as) gs =
  let gs' = stepPerfect a gs
  in DHWorld (SAS gs a gs') : genTrace as gs'

dhinitState3 :: GlobalState
dhinitState3 = initState ["Ghosh", "Boner", "Klang"]

-- This trace is not very intuitive
dhaction3 :: [Action]
dhaction3 =
  [ HakkerHungry "Ghosh"
  , HakkerHungry "Boner"
  , HakkerHungry "Klang"
  , ChopstickResp 0
  , ChopstickResp 2
  , HakkerEat "Ghosh"
  , HakkerThink "Ghosh"
  , ChopstickResp 0
  , ChopstickResp 0
  , ChopstickResp 0
  , ChopstickResp 1
  , HakkerEat "Boner"
  , HakkerThink "Boner"
  , ChopstickResp 1
  , ChopstickResp 1
  , ChopstickResp 1
  , ChopstickResp 2
  , ChopstickResp 2
  , ChopstickResp 2
  , HakkerEat "Klang"
  ]

dhtrace3 :: [DHWorld]
dhtrace3 = genTrace dhaction3 dhinitState3

-- Domain Theory
dinningHakkerTheory :: Theory
dinningHakkerTheory = Theory
  { theoryTypes =
    [ YesComment "Hakker" "The id of an actor representing a hakker (philosopher)"
    , YesComment "Chopstick" "The id of an actor representing a chopstick"
    , YesComment "HakkerMsg" "A message sent by a hakker"
    , YesComment "ChopstickMsg" "A message sent by a chopstick"
    ]
  , theoryProps = NoComment <$> M.fromList [
      ("InitiallyThinking", ["Hakker"])
    , ("IsHungry", ["Hakker"])
    , ("IsEating", ["Hakker"])
    -- A message that has been received but not yet delivered by a chopstick
    -- i.e., the message is currently in the chopRecvs Seq
    , ("ReceivedNotDelivered", ["Chopstick", "HakkerMsg"])
    , ("FromAdjacent", ["Chopstick", "HakkerMsg"])
  ]
  , theoryAxioms = M.fromList
    [ ("liveness",
        -- checking liveness properties for all hakkers
        -- FIXME: this is not fully liveness
        -- TODO: address this in the actor model
        YesComment
          (SPropAlways
            (SPropForAll
              [BinderGroup ["h"] "Hakker"]
              (SPropIf
                [SPropAtom (Atom "InitiallyThinking" ["h"])]
                (SPropEventually
                  (SPropAtom (Atom "IsEating" ["h"])))
              )
            )
          ) "All hakkers that are initially thinking should eventually start eating"
      )
    , ("receiveFromAdjacentHakkers",
        YesComment
          (SPropAlways
            (SPropForAll [BinderGroup ["c"] "Chopstick", BinderGroup ["hm"] "HakkerMsg"]
              (SPropIf
                [SPropAtom (Atom "ReceivedNotDelivered" ["c", "hm"])]
                (SPropAtom (Atom "FromAdjacent" ["c", "hm"]))
              )
            )
          ) "All messages a chopstick received come from its adjacent hakkers"
      )
    ]
  }

data DHVal =
    DHValHakker HakkerId
  | DHValChopstick ChopstickId
  | DHValTS TimeStamp
  | DHValHakkerMsg HakkerMsg
  | DHValChopMsg ChopstickMsg
  deriving stock (Eq, Show)

lookupHakkerAfter :: DHWorld -> HakkerId -> Maybe Hakker
lookupHakkerAfter (DHWorld (SAS _ _ GlobalState {hakkers=hks})) =
  flip M.lookup hks

lookupChopstickAfter :: DHWorld -> ChopstickId -> Maybe Chopstick
lookupChopstickAfter (DHWorld (SAS _ _ GlobalState {chopsticks=chops})) =
  flip M.lookup chops

exists :: Eq a => a -> Seq a -> Bool
exists _ Empty = False
exists a (xs :|> x) = (a == x) || exists a xs

getHakkerIdFromMsg :: HakkerMsg -> HakkerId
getHakkerIdFromMsg = \case
  Take _ hid _ -> hid
  Put _ hid _ -> hid

evalDHAtomProp :: DHWorld -> Atom DHVal -> Either Error Bool
evalDHAtomProp w@(DHWorld (SAS (GlobalState ts _ _ _) _ _)) (Atom prop vals) = case (prop, vals) of
    ("InitiallyThinking", [DHValHakker hid])  ->
      let res = lookupHakkerAfter w hid
      in case res of
        Nothing -> Left "Hakker ID doesn't exist"
        Just hk -> Right (ts == 0 && hkState hk == Thinking)
    ("IsEating", [DHValHakker hid]) ->
      let res = lookupHakkerAfter w hid
      in case res of
        Nothing -> Left "Hakker ID doesn't exist"
        Just hk -> Right (hkState hk == Eating)
    ("IsHungry", [DHValHakker hid]) ->
      let res = lookupHakkerAfter w hid
      in case res of
        Nothing -> Left "Hakker ID doesn't exist"
        Just hk -> Right (hkState hk == Hungry)
    ("ReceivedNotDelivered", [DHValChopstick cid, DHValHakkerMsg msg]) ->
      let res = lookupChopstickAfter w cid
      in case res of
        Nothing -> Left "Chopstick ID doesn't exist"
        Just chop -> Right $ exists msg (chopRecvs chop)
    ("FromAdjacent", [DHValChopstick cid, DHValHakkerMsg msg]) ->
      let res2 = lookupHakkerAfter w (getHakkerIdFromMsg msg)
      in case res2 of
        Nothing -> Left "The sender of the message does not exist"
        Just hk -> Right (lchop hk == cid || rchop hk == cid)
    _ -> Left $ "Could not evaluate atomic proposition \"" ++ prop  ++ "\" with argument list " ++ show vals

instance Bridge Error DHVal DHWorld where
  bridgeEvalProp w =
    fmap (\b -> if b then PropTrue else PropFalse) . evalDHAtomProp w
  bridgeQuantify (DHWorld (SAS _ _ (GlobalState _ hks chops msgs))) tyName = case tyName of
    "Hakker" -> Right $ fmap DHValHakker (M.keys hks)
    "Chopstick" -> Right $ fmap DHValChopstick (M.keys chops)
    "HakkerMsg" -> Right $ fmap DHValHakkerMsg (lefts msgs)
    "ChopstickMsg" -> Right $ fmap DHValChopMsg (rights msgs)
    _ -> Left $ "Could not quantify type " ++ tyName

instance TruncBridge Error DHVal DHWorld where
  truncBridgeEmpty _ = S.fromList ["Hakker", "Chopstick", "HakkerMsg", "ChopstickMsg"]
  truncBridgeOracle w = fmap (\b -> if b then TriBoolTrue else TriBoolFalse) . evalDHAtomProp w
