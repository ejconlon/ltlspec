{-# LANGUAGE OverloadedStrings #-}

module Ltlspec.Printer where

import Control.Monad (join)
import Control.Monad.Reader (Reader, asks, runReader)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Ltlspec.Models.Ping.Verification (pingTheory)
import Ltlspec.Recursion (foldUpM)
import Ltlspec.Types (Atom (..), AxiomDef, AxiomName, Binder (..), Commented (..), PropDef, PropName, SProp,
                      SPropF (..), Theory (..), TyDef, TyName)
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
  | SymIf
  | SymIff
  | SymForAll
  | SymExists
  | SymNext
  | SymUntil
  | SymRelease
  | SymSet
  | SymProp
  | SymColon
  | SymComma
  | SymOpenParen
  | SymCloseParen
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
  SymIf -> "=>"
  SymIff -> "<=>"
  SymForAll -> "ForAll"
  SymExists -> "Exists"
  SymNext -> "Next"
  SymUntil -> "Until"
  SymRelease -> "Release"
  SymSet -> "Set"
  SymProp -> "Prop"
  SymColon -> ":"
  SymComma -> ","
  SymOpenParen -> "("
  SymCloseParen -> ")"
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
  SymIf -> RoleProp
  SymIff -> RoleProp
  SymForAll -> RoleProp
  SymExists -> RoleProp
  SymNext -> RoleProp
  SymUntil -> RoleProp
  SymRelease -> RoleProp
  SymSet -> RoleSort
  SymProp -> RoleSort
  SymColon -> RoleSyntax
  SymComma -> RoleSyntax
  SymOpenParen -> RoleSyntax
  SymCloseParen -> RoleSyntax
  SymComment -> RoleComment

newtype RenderM a = RenderM { unRenderM :: Reader (Symbol -> Text) a }
  deriving newtype (Functor, Applicative, Monad)

runRenderM :: RenderM a -> (Symbol -> Text) -> a
runRenderM = runReader . unRenderM

askSym :: Symbol -> RenderM (Doc Role)
askSym sym = RenderM (asks (\rep -> annotate (symRole sym) (pretty (rep sym))))

type Prec = Int

paren :: (Bool, Doc Role) -> RenderM (Doc Role)
paren (many, doc) =
  if many
    then do
      openDoc <- askSym SymOpenParen
      closeDoc <- askSym SymCloseParen
      pure (hcat [openDoc, doc, closeDoc])
    else pure doc

renderBinder :: Binder -> RenderM (Doc Role)
renderBinder (Binder v t) = do
    openDoc <- askSym SymOpenParen
    closeDoc <- askSym SymCloseParen
    colonDoc <- askSym SymColon
    let vDoc = annotate RoleBinder (pretty v)
        tDoc = annotate RoleType (pretty t)
    pure (hcat [openDoc, hsep [vDoc, colonDoc, tDoc], closeDoc])

-- TODO coalesce binders of same type
renderBinders :: [Binder] -> RenderM (Doc Role)
renderBinders bs = do
  commaDoc <- askSym SymComma
  bsDocs <- traverse renderBinder bs
  pure (hcat [hsep bsDocs, commaDoc])

-- TODO render infix until/release?
renderSPropRec :: SProp -> RenderM (Bool, Doc Role)
renderSPropRec = foldUpM go where
  go = \case
    SPropAtomF (Atom name vals) -> pure (not (null vals), hsep (annotate RoleProp (pretty name) : fmap (annotate RoleValue . pretty) vals))
    SPropTrueF -> do
      trueDoc <- askSym SymTrue
      pure (False, trueDoc)
    SPropFalseF -> do
      falseDoc <- askSym SymFalse
      pure (False, falseDoc)
    SPropNotF x -> do
      notDoc <- askSym SymNot
      xDoc <- paren x
      pure (True, hsep [notDoc, xDoc])
    SPropAndF xs -> do
      andDoc <- askSym SymAnd
      xsDocs <- traverse paren xs
      pure (True, hsep (punctuate andDoc xsDocs))
    SPropOrF xs -> do
      orDoc <- askSym SymOr
      xsDocs <- traverse paren xs
      pure (True, hsep (punctuate orDoc xsDocs))
    SPropIfF xs y -> do
      ifDoc <- askSym SymIf
      xsDocs <- traverse paren xs
      yDoc <- paren y
      pure (True, hsep (punctuate ifDoc xsDocs ++ [ifDoc, yDoc]))
    SPropIffF x y -> do
      iffDoc <- askSym SymIff
      xDoc <- paren x
      yDoc <- paren y
      pure (True, hsep [xDoc, iffDoc, yDoc])
    SPropNextF x -> do
      nextDoc <- askSym SymNext
      xDoc <- paren x
      pure (True, hsep [nextDoc, xDoc])
    SPropAlwaysF x -> do
      alwaysDoc <- askSym SymAlways
      xDoc <- paren x
      pure (True, hsep [alwaysDoc, xDoc])
    SPropEventuallyF x -> do
      evDoc <- askSym SymEventually
      xDoc <- paren x
      pure (True, hsep [evDoc, xDoc])
    SPropUntilF x y -> do
      untilDoc <- askSym SymUntil
      xDoc <- paren x
      yDoc <- paren y
      pure (True, hsep [untilDoc, xDoc, yDoc])
    SPropReleaseF x y -> do
      releaseDoc <- askSym SymRelease
      xDoc <- paren x
      yDoc <- paren y
      pure (True, hsep [releaseDoc, xDoc, yDoc])
    SPropForAllF bs x -> do
      forDoc <- askSym SymForAll
      bsDoc <- renderBinders bs
      xDoc <- paren x
      pure (True, hsep [forDoc, bsDoc, xDoc])
    SPropExistsF bs x -> do
      exDoc <- askSym SymExists
      bsDoc <- renderBinders bs
      xDoc <- paren x
      pure (True, hsep [exDoc, bsDoc, xDoc])

renderSProp :: SProp -> RenderM (Doc Role)
renderSProp = fmap snd . renderSPropRec

renderElement :: Element -> RenderM (Doc Role)
renderElement = \case
  ElementComment txt -> do
    comDoc <- askSym SymComment
    pure (hsep [comDoc, annotate RoleComment (pretty txt)])
  ElementType tn tns -> do
    colDoc <- askSym SymColon
    arrDoc <- askSym SymArrow
    setDoc <- askSym SymSet
    let start = [annotate RoleType (pretty tn), colDoc]
        mid = join [[annotate RoleType (pretty an), arrDoc] | an <- tns]
        end = [setDoc]
        complete = start ++ mid ++ end
    pure (hsep complete)
  ElementProp pn tns -> do
    colDoc <- askSym SymColon
    arrDoc <- askSym SymArrow
    propDoc <- askSym SymProp
    let start = [annotate RoleProp (pretty pn), colDoc]
        mid = join [[annotate RoleType (pretty an), arrDoc] | an <- tns]
        end = [propDoc]
        complete = start ++ mid ++ end
    pure (hsep complete)
  ElementAxiom an sprop -> do
    colDoc <- askSym SymColon
    spropDoc <- renderSProp sprop
    let start = [annotate RoleValue (pretty an), colDoc]
        end = [spropDoc]
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
