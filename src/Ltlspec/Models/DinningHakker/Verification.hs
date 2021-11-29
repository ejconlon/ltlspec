module Ltlspec.Models.DinningHakker.Verification where

import qualified Data.Map.Strict as M
import Ltlspec.Models.DinningHakker.Trace (Action (..), ChopstickId, GlobalState (..), HakkerId, TimeStamp, initState,
                                           stepPerfect)
import Ltlspec.Types (Atom (..), BinderGroup (..), Bridge (..), Commented (..), Error, SAS (..), SProp (..),
                      Theory (..))

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

dhaction3 :: [Action]
dhaction3 =
  [ HakkerHungry "Ghosh"
  , HakkerHungry "Boner"
  , HakkerHungry "Klang"
  , ChopstickResp 0
  , ChopstickResp 1
  , ChopstickResp 2
  , HakkerEat "Ghosh"
  ]

dhtrace3 :: [DHWorld]
dhtrace3 = genTrace dhaction3 dhinitState3

-- Domain Theory
dinningHakkerTheory :: Theory
dinningHakkerTheory = Theory
  { theoryTypes =
    [ YesComment "Hakker" "The id of an actor representing a hakker (philosopher)"
    , YesComment "Chopstick" "The id of an actor representing a chopstick"
    , YesComment "TimeStamp" "The timestamp of a message"
    , YesComment "HakkerMsg" "A message sent by a hakker"
    , YesComment "ChopstickMsg" "A message sent by a chopstick"
    ]
  , theoryProps = NoComment <$> M.fromList [
      ("IsThinking", ["Hakker"])
    , ("IsHungry", ["Hakker"])
    , ("IsEating", ["Hakker"])
    -- A message that has been received but not yet delivered by a chopstick
    -- i.e., the message is currently in the chopRecvs Seq
    , ("ReceivedNotDelivered", ["Chopstick, HakkerMsg"])
    , ("FromAdjacent", ["Chopstick, HakkerMsg"])
  ]
  , theoryAxioms = NoComment <$> M.fromList
    [ ("liveness",
        -- checking liveness properties for all hakkers
        -- All hakkers will start from thinking, and should eventually start eating
        -- Because the Eating state is mutually exclusive for adjacent Hakkers
        -- forall h: Hakker. isThinking(h) -> F[isEating(h)]
        SPropAlways
          (SPropForAll
            [BinderGroup ["h"] "Hakker"]
            (SPropIf
              [SPropAtom (Atom "IsThinking" ["h"])]
              (SPropEventually
                (SPropAtom (Atom "IsEating" ["h"])))
            )
          )
      )
    , ("receiveFromAdjacentHakkers",
        SPropAlways
          (SPropForAll [BinderGroup ["c"] "Chopstick", BinderGroup ["hm"] "HakkerMsg"]
            (SPropIf
              [SPropAtom (Atom "ReceivedNotDelivered" ["c", "hm"])]
              (SPropAtom (Atom "FromAdjacent" ["c", "hm"]))
            )
          )
      )
    ]
  }

data DHVal =
    DHValHakker HakkerId
  | DHValChopstick ChopstickId
  | DHValTS TimeStamp
  | DHValHakkerMsg
  | DHValChopMsg
  deriving stock (Eq, Show)

instance Bridge Error DHVal DHWorld where
  bridgeEvalProp _ _ = error "TODO"
  bridgeQuantify _ _ = error "TODO"
