{-# LANGUAGE OverloadedStrings #-}

module Ltlspec.Printer where

import Control.Monad (join)
import Control.Monad.Reader (Reader, asks, runReader)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Ltlspec.Models.Ping.Verification (pingTheory)
import Ltlspec.Recursion (foldUpM)
import Ltlspec.Types (Atom (..), AxiomDef, AxiomName, Commented (..), PropDef, PropName, SProp, SPropF (..),
                      Theory (..), TyDef, TyName)
import Prettyprinter (Doc, annotate, hcat, hsep, pretty, punctuate, vsep)
import Prettyprinter.Render.Terminal (AnsiStyle, Color (..), color, putDoc)

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

data Role =
    RoleComment
  | RoleType
  | RoleSyntax
  | RoleProp
  | RoleBinder
  | RoleValue
  | RoleSort
  deriving stock (Eq, Show)

roleColor :: Role -> Color
roleColor = \case
  RoleComment -> Green
  RoleType -> Blue
  RoleSyntax -> White
  RoleProp -> Cyan
  RoleBinder -> Red
  RoleValue -> Magenta
  RoleSort -> Yellow

data Symbol =
    SymTrue
  | SymFalse
  | SymNot
  | SymAnd
  | SymOr
  | SymAlways
  | SymEventually
  | SymArrow
  | SymForAll
  | SymExists
  | SymNext
  | SymUntil
  | SymRelease
  | SymSet
  | SymProp
  | SymColon
  | SymComment
  deriving stock (Eq, Show)

asciiRep :: Symbol -> Text
asciiRep = \case
  SymTrue -> "True"
  SymFalse -> "False"
  SymNot -> "Not"
  SymAnd -> "And"
  SymOr -> "Or"
  SymAlways -> "Always"
  SymEventually -> "Eventually"
  SymArrow -> "->"
  SymForAll -> "ForAll"
  SymExists -> "Exists"
  SymNext -> "Next"
  SymUntil -> "Until"
  SymRelease -> "Release"
  SymSet -> "Set"
  SymProp -> "Prop"
  SymColon -> ":"
  SymComment -> "#"

symRole :: Symbol -> Role
symRole = \case
  SymTrue -> RoleProp
  SymFalse -> RoleProp
  SymNot -> RoleProp
  SymAnd -> RoleProp
  SymOr -> RoleProp
  SymAlways -> RoleProp
  SymEventually -> RoleProp
  SymArrow -> RoleSyntax
  SymForAll -> RoleProp
  SymExists -> RoleProp
  SymNext -> RoleProp
  SymUntil -> RoleProp
  SymRelease -> RoleProp
  SymSet -> RoleSort
  SymProp -> RoleSort
  SymColon -> RoleSyntax
  SymComment -> RoleComment

newtype RenderM a = RenderM { unRenderM :: Reader (Symbol -> Text) a }
  deriving newtype (Functor, Applicative, Monad)

runRenderM :: RenderM a -> (Symbol -> Text) -> a
runRenderM = runReader . unRenderM

askSym :: Symbol -> RenderM (Doc Role)
askSym sym = RenderM (asks (\rep -> annotate (symRole sym) (pretty (rep sym))))

type Prec = Int

paren :: (Bool, Doc ann) -> Doc ann
paren (many, doc) = if many then hcat ["(", doc, ")"] else doc

renderSPropRec :: SProp -> RenderM (Bool, Doc Role)
renderSPropRec = foldUpM go where
  go = \case
    SPropAtomF (Atom name vals) -> pure (not (null vals), hsep (annotate RoleProp (pretty name) : fmap (annotate RoleValue . pretty) vals))
    SPropTrueF -> pure (False, annotate RoleProp "True")
    SPropFalseF -> pure (False, annotate RoleProp "False")
    SPropNotF x -> pure (True, hsep [annotate RoleProp "Not", paren x])
    SPropAndF xs -> pure (True, hsep (punctuate (annotate RoleProp "/\\") (fmap paren xs)))
    SPropOrF xs -> pure (True, hsep (punctuate (annotate RoleProp "\\/") (fmap paren xs)))
    SPropIfF xs y -> pure (True, "TODO")
    SPropIffF x y -> pure (False, "TODO")
    SPropNextF x -> pure (False, "TODO")
    SPropAlwaysF x -> pure (True, hsep [annotate RoleProp "Always", paren x])
    SPropEventuallyF x -> pure (True, hsep [annotate RoleProp "Eventually", paren x])
    SPropUntilF x y -> pure (False, "TODO")
    SPropReleaseF x y -> pure (False, "TODO")
    SPropForAllF bs x -> pure (True, "TODO")
    SPropExistsF bs x -> pure (False, "TODO")

renderSProp :: SProp -> RenderM (Doc Role)
renderSProp = fmap snd . renderSPropRec

renderElement :: Element -> RenderM (Doc Role)
renderElement = \case
  ElementComment txt -> do
    com <- askSym SymComment
    pure (hsep [com, annotate RoleComment (pretty txt)])
  ElementType tn tns -> do
    col <- askSym SymColon
    arr <- askSym SymArrow
    set <- askSym SymSet
    let start = [annotate RoleType (pretty tn), col]
        mid = join [[annotate RoleType (pretty an), arr] | an <- tns]
        end = [set]
        complete = start ++ mid ++ end
    pure (hsep complete)
  ElementProp pn tns -> do
    col <- askSym SymColon
    arr <- askSym SymArrow
    prop <- askSym SymProp
    let start = [annotate RoleProp (pretty pn), col]
        mid = join [[annotate RoleType (pretty an), arr] | an <- tns]
        end = [prop]
        complete = start ++ mid ++ end
    pure (hsep complete)
  ElementAxiom an sprop -> do
    col <- askSym SymColon
    rest <- renderSProp sprop
    let start = [annotate RoleValue (pretty an), col]
        end = [rest]
        complete = start ++ end
    pure (hsep complete)

renderGroup :: Group -> RenderM [Doc Role]
renderGroup = traverse renderElement

renderGroups :: [Group] -> RenderM (Doc Role)
renderGroups = fmap vsep . traverse (fmap vsep . renderGroup)

prettyTheory :: Theory -> RenderM (Doc Role)
prettyTheory = renderGroups . theoryGroups

ansiDoc :: Doc Role -> Doc AnsiStyle
ansiDoc = fmap (color . roleColor)

putAnsiDoc :: Doc Role -> IO ()
putAnsiDoc = putDoc . ansiDoc

main :: IO ()
main = putAnsiDoc (runRenderM (prettyTheory pingTheory) asciiRep) *> putChar '\n'
