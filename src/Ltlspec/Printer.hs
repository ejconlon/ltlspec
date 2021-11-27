{-# LANGUAGE OverloadedStrings #-}

module Ltlspec.Printer where

import Control.Monad (join)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Ltlspec.Models.Ping.Verification (pingTheory)
import Ltlspec.Types (AxiomDef, AxiomName, Commented (..), Prop, PropDef, PropName, Theory (..), TyDef, TyName)
import Prettyprinter (Doc, annotate, hsep, pretty, vsep)
import Prettyprinter.Render.Terminal (AnsiStyle, Color (..), color, putDoc)

data Element =
    ElementComment !Text
  | ElementType !TyName ![TyName]
  | ElementProp !PropName ![TyName]
  | ElementAxiom !AxiomName !Prop
  deriving stock (Eq, Show)

type Group = [Element]

commentedElements :: (a -> [Element]) -> Commented a -> [Element]
commentedElements f = \case
  NoComment a -> f a
  YesComment a c -> ElementComment (T.pack c) : f a

tyDefElements :: TyDef -> [Element]
tyDefElements = commentedElements $ \tn -> [ElementType tn []]

propDefElements :: PropDef -> [Element]
propDefElements (pn, ctns) = flip commentedElements ctns $ \tns -> [ElementProp pn tns]

axiomDefElements :: AxiomDef -> [Element]
axiomDefElements (an, cprop) = flip commentedElements cprop $ \prop -> [ElementAxiom an prop]

theoryGroups :: Theory -> [Group]
theoryGroups (Theory tds pds ads) =
  [ tds >>= tyDefElements
  , Map.toList pds >>= propDefElements
  , Map.toList ads >>= axiomDefElements
  ]

comColor, tyColor, synColor, propColor, axColor, sortColor, logColor  :: Color
comColor = Green
tyColor = Blue
synColor = White
propColor = Cyan
axColor = Magenta
sortColor = Yellow
logColor = Red

renderProp :: Prop -> Doc Color
renderProp _ = "TODO"

renderElement :: Element -> Doc Color
renderElement = \case
  ElementComment txt -> annotate comColor (hsep ["#", pretty txt])
  ElementType tn tns ->
    let start = [annotate tyColor (pretty tn), annotate synColor ":"]
        mid = join [[annotate tyColor (pretty an), annotate synColor "->"] | an <- tns]
        end = [annotate sortColor "Set"]
        complete = start ++ mid ++ end
    in hsep complete
  ElementProp pn tns ->
    let start = [annotate propColor (pretty pn), annotate synColor ":"]
        mid = join [[annotate tyColor (pretty an), annotate synColor "->"] | an <- tns]
        end = [annotate sortColor "Prop"]
        complete = start ++ mid ++ end
    in hsep complete
  ElementAxiom an prop ->
    let start = [annotate axColor (pretty an), annotate synColor ":"]
        end = [renderProp prop]
        complete = start ++ end
    in hsep complete

renderGroups :: [Group] -> Doc Color
renderGroups = vsep . fmap (vsep . fmap renderElement)

prettyTheory :: Theory -> Doc Color
prettyTheory = renderGroups . theoryGroups

ansiDoc :: Doc Color -> Doc AnsiStyle
ansiDoc = fmap color

putAnsiDoc :: Doc Color -> IO ()
putAnsiDoc = putDoc . ansiDoc

main :: IO ()
main = putAnsiDoc (prettyTheory pingTheory)
