{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Foldable (for_)
import Data.Text (Text)
import qualified Data.Text as T
import Ltlspec.Models.Chat.Verification (chatTheory)
import Ltlspec.Models.DinningHakker.Verification (dinningHakkerTheory)
import Ltlspec.Models.Ping.Verification (pingTheory)
import Ltlspec.Printer (Role (..), defaultTexOptions, hRenderTex, prettyTheory, runRenderM, texRep, unicodeRep)
import Ltlspec.Types (Theory)
import Prettyprinter (Doc, annotate, hsep, indent, pretty)
import Prettyprinter.Render.Terminal (AnsiStyle, Color (..), color, putDoc)
import Prettyprinter.Render.Text (hPutDoc)
import System.Directory (createDirectoryIfMissing)
import System.IO (IOMode (..), hPutStr, withFile)

roleColor :: Role -> Color
roleColor = \case
  RoleComment -> Green
  RoleType -> Blue
  RoleSyntax -> White
  RoleProp -> Cyan
  RoleBinder -> Red
  RoleValue -> Magenta
  RoleSort -> Yellow

ansiAnn :: Role -> AnsiStyle
ansiAnn = color . roleColor

ansiDoc :: Doc Role -> Doc AnsiStyle
ansiDoc = fmap ansiAnn

putAnsiDoc :: Doc Role -> IO ()
putAnsiDoc = putDoc . ansiDoc

theories :: [(Text, Text, Theory)]
theories =
  [ ("Ping", "ping", pingTheory)
  , ("Chat", "chat", chatTheory)
  , ("Dining Philosophers", "phil", dinningHakkerTheory)
  ]

printConsole :: IO ()
printConsole = do
  for_ theories $ \(name, _, theory) -> do
    putDoc (annotate (color Red) (hsep ["#", pretty name, "Theory"]))
    putStr "\n\n"
    putAnsiDoc (indent 4 (runRenderM (prettyTheory theory) unicodeRep))
    putStr "\n\n"

printMarkdown :: IO ()
printMarkdown = do
  let filename = "gendocs/Theories.md"
  withFile filename WriteMode $ \handle -> do
    for_ theories $ \(name, _, theory) -> do
        hPutStr handle (T.unpack ("# " <> name <> " Theory\n\n"))
        hPutDoc handle (indent 4 (runRenderM (prettyTheory theory) unicodeRep))
        hPutStr handle "\n\n"

printTex :: IO ()
printTex = do
  createDirectoryIfMissing False "gendocs/tex"
  for_ theories $ \(_, slug, theory) -> do
    let filename = "gendocs/tex/" <> T.unpack slug <> ".tex"
    withFile filename WriteMode $ \handle ->
      hRenderTex defaultTexOptions handle (runRenderM (prettyTheory theory) texRep)

main :: IO ()
main = do
  printConsole
  printMarkdown
  printTex
