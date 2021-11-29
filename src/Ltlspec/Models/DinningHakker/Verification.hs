module Ltlspec.Models.DinningHakker.Verification where

import qualified Data.Map.Strict as M
import Ltlspec.Types (Atom (..), BinderGroup (..), Commented (..), SProp (..), Theory (..))

-- Domain Theory
dinningHakkerTheory :: Theory
dinningHakkerTheory = Theory
  { theoryTypes = NoComment <$> ["Hakker", "Chopstick", "TimeStamp", "HakkerMsg", "ChopstickMsg"]
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
