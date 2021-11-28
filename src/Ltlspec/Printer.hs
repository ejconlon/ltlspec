{-# LANGUAGE OverloadedStrings #-}

module Ltlspec.Printer where

import Control.Monad (join)
import Control.Monad.Reader (Reader, asks, runReader)
import Data.List (intersperse)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
-- import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.IO as TLIO
import Ltlspec.Recursion (foldUpM)
import Ltlspec.Types (Atom (..), AxiomDef, AxiomName, BinderGroup (..), Commented (..), PropDef, PropName, SProp,
                      SPropF (..), Theory (..), TyDef, TyName)
import Prettyprinter (Doc, LayoutOptions (LayoutOptions), PageWidth (..), SimpleDocStream (..), annotate,
                      defaultLayoutOptions, hcat, hsep, indent, layoutPretty, line, pretty, vsep)
import System.IO (Handle)

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
  | SymOpenComment
  | SymCloseComment
  deriving stock (Eq, Show)

asciiRep :: Symbol -> Text
asciiRep = \case
  SymTrue -> "True"
  SymFalse -> "False"
  SymNot -> "not"
  SymAnd -> "And"
  SymOr -> "Or"
  SymAlways -> "Always"
  SymEventually -> "Eventually"
  SymArrow -> "->"
  SymIf -> "=>"
  SymIff -> "<=>"
  SymForAll -> "forall"
  SymExists -> "exists"
  SymNext -> "Next"
  SymUntil -> "Until"
  SymRelease -> "Release"
  SymSet -> "Set"
  SymProp -> "Prop"
  SymColon -> ":"
  SymComma -> ","
  SymOpenParen -> "("
  SymCloseParen -> ")"
  SymOpenComment -> "(*"
  SymCloseComment -> "*)"

unicodeRep :: Symbol -> Text
unicodeRep = \case
  SymTrue -> "⊤"
  SymFalse -> "⊥"
  SymNot -> "¬"
  SymAnd -> "∧"
  SymOr -> "∨"
  SymAlways -> "□"
  SymEventually -> "◇"
  SymArrow -> "→"
  SymIf -> "⇒"
  SymIff -> "⇔"
  SymForAll -> "∀"
  SymExists -> "∃"
  SymNext -> "X"
  SymUntil -> "U"
  SymRelease -> "R"
  SymSet -> "Set"
  SymProp -> "Prop"
  SymColon -> ":"
  SymComma -> ","
  SymOpenParen -> "("
  SymCloseParen -> ")"
  SymOpenComment -> "(*"
  SymCloseComment -> "*)"

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
  SymOpenComment -> RoleComment
  SymCloseComment -> RoleComment

newtype RenderM a = RenderM { unRenderM :: Reader (Symbol -> Text) a }
  deriving newtype (Functor, Applicative, Monad)

runRenderM :: RenderM a -> (Symbol -> Text) -> a
runRenderM = runReader . unRenderM

askSym :: Symbol -> RenderM (Doc Role)
askSym sym = RenderM (asks (\rep -> annotate (symRole sym) (pretty (rep sym))))

type Prec = Int

breakIndent :: Doc ann -> Doc ann
breakIndent doc = line <> indent 2 doc

paren :: (Bool, Doc Role) -> RenderM (Doc Role)
paren (many, doc) =
  if many
    then do
      openDoc <- askSym SymOpenParen
      closeDoc <- askSym SymCloseParen
      pure (hcat [openDoc, doc, closeDoc])
    else pure doc

renderBinderGroup :: BinderGroup -> RenderM (Doc Role)
renderBinderGroup (BinderGroup vs t) = do
  openDoc <- askSym SymOpenParen
  closeDoc <- askSym SymCloseParen
  colonDoc <- askSym SymColon
  let vsDoc = hsep (fmap (annotate RoleBinder . pretty) vs)
      tDoc = annotate RoleType (pretty t)
  pure (hcat [openDoc, hsep [vsDoc, colonDoc, tDoc], closeDoc])

-- TODO coalesce binders of same type
renderBinderGroups :: [BinderGroup] -> RenderM (Doc Role)
renderBinderGroups bgs = do
  commaDoc <- askSym SymComma
  bgsDocs <- traverse renderBinderGroup bgs
  pure (hcat [hsep bgsDocs, commaDoc])

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
      pure (True, hsep (intersperse andDoc xsDocs))
    SPropOrF xs -> do
      orDoc <- askSym SymOr
      xsDocs <- traverse paren xs
      pure (True, hsep (intersperse orDoc xsDocs))
    SPropIfF xs y -> do
      ifDoc <- askSym SymIf
      xsDocs <- traverse paren xs
      yDoc <- paren y
      pure (True, hsep (intersperse ifDoc xsDocs ++ [ifDoc, yDoc]))
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
    SPropForAllF bgs x -> do
      forDoc <- askSym SymForAll
      bgsDoc <- renderBinderGroups bgs
      let xDoc = snd x
      pure (True, hsep [forDoc, bgsDoc, breakIndent xDoc])
    SPropExistsF bgs x -> do
      exDoc <- askSym SymExists
      bgsDoc <- renderBinderGroups bgs
      let xDoc = snd x
      pure (True, hsep [exDoc, bgsDoc, breakIndent xDoc])

renderSProp :: SProp -> RenderM (Doc Role)
renderSProp = fmap snd . renderSPropRec

renderElement :: Element -> RenderM (Doc Role)
renderElement = \case
  ElementComment txt -> do
    openComDoc <- askSym SymOpenComment
    closeComDoc <- askSym SymCloseComment
    pure (hsep [openComDoc, annotate RoleComment (pretty txt), closeComDoc])
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
        end = [breakIndent spropDoc]
        complete = start ++ end
    pure (hsep complete)

renderGroup :: Group -> RenderM [Doc Role]
renderGroup = traverse renderElement

renderGroups :: [Group] -> RenderM (Doc Role)
renderGroups = fmap (vsep . intersperse line) . traverse (fmap vsep . renderGroup)

prettyTheory :: Theory -> RenderM (Doc Role)
prettyTheory = renderGroups . theoryGroups

texColor :: Role -> Text
texColor = \case
  RoleComment -> "black"
  RoleType -> "blue"
  RoleSyntax -> "darkgray"
  RoleProp -> "teal"
  RoleBinder -> "red"
  RoleValue -> "purple"
  RoleSort -> "cyan"

texRep :: Symbol -> Text
texRep = \case
  SymTrue -> "$\\top$"
  SymFalse -> "$\\bot$"
  SymNot -> "$\\neg$"
  SymAnd -> "$\\land$"
  SymOr -> "$\\lor$"
  SymAlways -> "$\\Box$"
  SymEventually -> "$\\Diamond$"
  SymArrow -> "$\\rightarrow$"
  SymIf -> "$\\implies$"
  SymIff -> "$\\iff$"
  SymForAll -> "$\\forall$"
  SymExists -> "$\\exists$"
  SymNext -> "X"
  SymUntil -> "U"
  SymRelease -> "R"
  SymSet -> "Set"
  SymProp -> "Prop"
  SymColon -> ":"
  SymComma -> ","
  SymOpenParen -> "("
  SymCloseParen -> ")"
  SymOpenComment -> "(*"
  SymCloseComment -> "*)"

data TexOptions = TexOptions
  { texOptFontSize :: Maybe Text
  , texOptWidth :: Maybe Int
  } deriving stock (Eq, Show)

texLayoutOptions :: TexOptions -> LayoutOptions
texLayoutOptions = maybe defaultLayoutOptions (\w -> LayoutOptions (AvailablePerLine w 1.0)) . texOptWidth

texVerbatimOptions :: TexOptions -> Text
texVerbatimOptions (TexOptions mfs _) =
  let base = "commandchars=\\\\\\{\\},codes={\\catcode`$=3}"
  in maybe base (\fs -> base <> ",fontsize=\\" <> fs) mfs

bRenderTex :: TexOptions -> Doc Role -> TLB.Builder
bRenderTex texOpts doc = whole where
  layoutOpts = texLayoutOptions texOpts
  verbatimOpts = texVerbatimOptions texOpts
  before = TLB.fromText "\\begin{Verbatim}[" <> TLB.fromText verbatimOpts <> TLB.fromText "]\n"
  during = go (layoutPretty layoutOpts doc)
  after = TLB.fromText "\n\\end{Verbatim}\n"
  whole = before <> during <> after
  go = \case
    SFail -> error "fail"
    SEmpty -> mempty
    SChar c rest -> TLB.singleton c <> go rest
    SText _ t rest -> TLB.fromText t <> go rest
    SLine i rest -> TLB.fromText "\n" <> TLB.fromText (T.replicate i " ") <> go rest
    SAnnPush ann rest -> TLB.fromText "\\textcolor{" <> TLB.fromText (texColor ann) <> TLB.fromText "}{" <> go rest
    SAnnPop rest -> TLB.singleton '}' <> go rest

hRenderTex :: TexOptions -> Handle -> Doc Role -> IO ()
hRenderTex texOpts handle doc = TLIO.hPutStr handle (TLB.toLazyText (bRenderTex texOpts doc))
