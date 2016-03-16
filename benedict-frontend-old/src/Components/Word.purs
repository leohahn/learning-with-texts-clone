module Components.Word
    ( word
    , WordQuery()
    , WordKnowledge(..)
    ) where

import Prelude (class Functor, ($), pure, bind)
import CSS.Background (backgroundColor)
import CSS.Color (rgb)
import CSS.Stylesheet (CSS())
import Halogen (ComponentDSL, Natural, ComponentHTML
               ,Component, component, modify)
import Halogen.HTML.Indexed as H
import Halogen.HTML.CSS.Indexed as C


type WordState =
    { name      :: String
    , knowledge :: WordKnowledge
    }

data WordKnowledge
    = Bad
    | Poor
    | Average
    | Good
    | Excelent

data WordQuery a
    = ChangeKnowledge WordKnowledge a

word :: forall g. (Functor g) => Component WordState WordQuery g
word = component render eval
  where
    render :: WordState -> ComponentHTML WordQuery
    render state =
        H.span
            [ C.style $ toColor state.knowledge ]
            [ H.text state.name ]

    eval :: Natural WordQuery (ComponentDSL WordState WordQuery g)
    eval (ChangeKnowledge knowledge next) = do
        modify (\state -> state { knowledge = knowledge })
        pure next

toColor :: WordKnowledge -> CSS
toColor Bad      = backgroundColor $ rgb 228 136 27
toColor Poor     = backgroundColor $ rgb 218 187 55
toColor Average  = backgroundColor $ rgb 147 170 39
toColor Good     = backgroundColor $ rgb 42 186 156
toColor Excelent = backgroundColor $ rgb 70 137 170
