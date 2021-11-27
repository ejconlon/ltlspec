{-# LANGUAGE OverloadedStrings #-}

module Ltlspec.Printer where

import Control.Monad (join)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Ltlspec.Models.Ping.Verification (pingTheory)
import Ltlspec.Types (AxiomDef, AxiomName, Commented (..), PropDef, PropName, SProp, Theory (..), TyDef, TyName, SPropF (..), Atom (..))
import Prettyprinter (Doc, annotate, hsep, pretty, vsep, punctuate, hcat)
import Prettyprinter.Render.Terminal (AnsiStyle, Color (..), color, putDoc)
import Data.Functor.Foldable (fold)

data Element =
    ElementComment !Text
  | ElementType !TyName ![TyName]
  | ElementProp !PropName ![TyName]
  | ElementAxiom !AxiomName !SProp
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
theoryGroups (Theory tds pds ads) = filter (not . null)
  [ tds >>= tyDefElements
  , Map.toList pds >>= propDefElements
  , Map.toList ads >>= axiomDefElements
  ]

comColor, tyColor, synColor, propColor, bindColor, valColor, axColor, sortColor, logColor  :: Color
comColor = Green
tyColor = Blue
synColor = White
propColor = Cyan
bindColor = Red
valColor = Magenta
axColor = valColor
sortColor = Yellow
logColor = propColor

type Prec = Int

paren :: (Bool, Doc Color) -> Doc Color
paren (many, doc) = if many then hcat ["(", doc, ")"] else doc

renderSProp :: SProp -> (Bool, Doc Color)
renderSProp = fold go where
  go = \case
    SPropAtomF (Atom name vals) -> (not (null vals), hsep (annotate propColor (pretty name) : fmap (annotate valColor . pretty) vals))
    SPropTrueF -> (False, annotate logColor "True")
    SPropFalseF -> (False, annotate logColor "False")
    SPropNotF x -> (True, hsep [annotate logColor "Not", paren x])
    SPropAndF xs -> (True, hsep (punctuate "/\\" (fmap paren xs)))
    SPropOrF xs -> (True, hsep (punctuate "\\/" (fmap paren xs)))
    SPropIfF xs y -> (True, "TODO")
    SPropIffF x y -> (False, "TODO")
    SPropNextF x-> (False, "TODO")
    SPropAlwaysF x -> (True, hsep [annotate logColor "Always", paren x])
    SPropEventuallyF x -> (True, hsep [annotate logColor "Eventually", paren x])
    SPropUntilF x y -> (False, "TODO")
    SPropReleaseF x y -> (False, "TODO")
    SPropForAllF bs x ->
      let foo = 1
      in (True, "TODO")
    SPropExistsF bs x -> (False, "TODO")


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
  ElementAxiom an sprop ->
    let start = [annotate axColor (pretty an), annotate synColor ":"]
        end = [snd (renderSProp sprop)]
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
