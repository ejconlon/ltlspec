{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Foldable (for_)
import Data.Text (Text)
import qualified Data.Text as T
import Ltlspec.Models.Chat.Chat (chatTheory)
import Ltlspec.Models.DinningHakker (dinningHakkerTheory)
import Ltlspec.Models.Ping.Verification (pingTheory)
import Ltlspec.Printer (Role (..), prettyTheory, runRenderM, unicodeRep)
import Ltlspec.Types (Theory)
import Prettyprinter (Doc, annotate, hsep, indent, pretty)
import Prettyprinter.Render.Text (hPutDoc)
import Prettyprinter.Render.Terminal (AnsiStyle, Color (..), color, putDoc)
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

theories :: [(Text, Theory)]
theories = [("Ping", pingTheory), ("Chat", chatTheory), ("Dining Philosophers", dinningHakkerTheory)]

printConsole :: IO ()
printConsole = do
  for_ theories $ \(name, theory) -> do
    putDoc (annotate (color Red) (hsep ["#", pretty name, "Theory"]))
    putStr "\n\n"
    putAnsiDoc (indent 4 (runRenderM (prettyTheory theory) unicodeRep))
    putStr "\n\n"

printFile :: IO ()
printFile = do
  let filename = "gendocs/Theories.md"
  withFile filename WriteMode $ \handle -> do
    for_ theories $ \(name, theory) -> do
        hPutStr handle (T.unpack ("# " <> name <> " Theory\n\n"))
        hPutDoc handle (indent 4 (runRenderM (prettyTheory theory) unicodeRep))
        hPutStr handle "\n\n"

main :: IO ()
main = do
  printConsole
  printFile
