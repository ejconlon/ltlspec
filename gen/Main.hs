{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Foldable (for_)
import Data.Text (Text)
import Ltlspec.Models.Chat.Chat (chatTheory)
import Ltlspec.Models.DinningHakker (dinningHakkerTheory)
import Ltlspec.Models.Ping.Verification (pingTheory)
import Ltlspec.Printer (Role (..), prettyTheory, runRenderM, unicodeRep)
import Ltlspec.Types (Theory)
import Prettyprinter (Doc, annotate, hsep, indent, pretty)
import Prettyprinter.Render.Terminal (AnsiStyle, Color (..), color, putDoc)

roleColor :: Role -> Color
roleColor = \case
  RoleComment -> Green
  RoleType -> Blue
  RoleSyntax -> White
  RoleProp -> Cyan
  RoleBinder -> Red
  RoleValue -> Magenta
  RoleSort -> Yellow

ansiDoc :: Doc Role -> Doc AnsiStyle
ansiDoc = fmap (color . roleColor)

putAnsiDoc :: Doc Role -> IO ()
putAnsiDoc = putDoc . ansiDoc

theories :: [(Text, Theory)]
theories = [("Ping", pingTheory), ("Chat", chatTheory), ("Dining Philosophers", dinningHakkerTheory)]

main :: IO ()
main = do
  for_ theories $ \(name, theory) -> do
    putDoc (annotate (color Red) (hsep ["#", pretty name, "Theory"]))
    putStr "\n\n"
    putAnsiDoc (indent 4 (runRenderM (prettyTheory theory) unicodeRep))
    putStr "\n\n"
