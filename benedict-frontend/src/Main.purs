module Main where

import Prelude (Unit, ($), bind, unit, pure, const)
import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (throwException)
import Halogen (HalogenEffects, runUI)
import Halogen.Util (appendToBody, onLoad)

import Components.Sidebar as Sidebar
import Components.Sidebar (sidebar)
import Components.Word as Word

main :: Eff (HalogenEffects ()) Unit
main = runAff throwException (const (pure unit)) $ do
    app <- runUI Word.word { name: "boat", knowledge: Word.Poor }
    onLoad $ appendToBody app.node
