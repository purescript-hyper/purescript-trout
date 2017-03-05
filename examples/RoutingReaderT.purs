module Examples.RoutingReaderT where

import Prelude
import Control.IxMonad ((:*>))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Maybe (fromMaybe)
import Hyper.Node.Server (defaultOptionsWithLogging, runServer')
import Hyper.Response (closeHeaders, respond, writeStatus)
import Hyper.Routing.ContentType.HTML (class EncodeHTML, HTML)
import Hyper.Routing.Method (Get)
import Hyper.Routing.Router (RoutingError, router)
import Node.HTTP (HTTP)
import Text.Smolder.HTML (p)
import Text.Smolder.Markup (text)
import Type.Proxy (Proxy(..))

data Greeting = Greeting String

type Site = Get HTML Greeting

instance encodeHTMLGreeting :: EncodeHTML Greeting where
  encodeHTML (Greeting g) = p (text g)

runAppM ∷ ∀ e a. String -> ReaderT String (Aff e) a → (Aff e) a
runAppM = flip runReaderT

site :: Proxy Site
site = Proxy

greet :: forall m. Monad m => ExceptT RoutingError (ReaderT String m) Greeting
greet = Greeting <$> ask

main :: forall e. Eff (console :: CONSOLE, http :: HTTP | e) Unit
main =
  let app = router site greet onRoutingError

      onRoutingError status msg =
        writeStatus status
        :*> closeHeaders
        :*> respond (fromMaybe "" msg)

  in runServer' defaultOptionsWithLogging {} (runAppM "Hello") app
