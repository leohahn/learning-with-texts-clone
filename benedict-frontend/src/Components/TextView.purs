module Components.TextView where

import Prelude

import Halogen
import Halogen.HTML.Indexed as H
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Properties.Indexed as P

type Word = 
    { name      :: String
    , lang      :: String
    , knowledge :: Knowledge
    }

type State =
    { dictionary :: Array Word
    }
textView :: forall g. (Functor g) => Component state query g
