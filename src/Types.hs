module Types
       ( AppM
       ) where

import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Reader (ReaderT)
import Servant (ServantErr)
import Config (Config)

-- | The monad that the application is going to run into.
-- It uses a Reader transformer, so that it can use a configuration.
type AppM = ReaderT Config (ExceptT ServantErr IO)
