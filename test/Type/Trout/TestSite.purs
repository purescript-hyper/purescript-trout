module Type.Trout.TestSite where

import Prelude
import Data.Argonaut (class EncodeJson, assoc, jsonEmptyObject, (~>))
import Data.Either (Either(..))
import Data.String (trim)
import Text.Smolder.HTML (h1)
import Text.Smolder.Markup (text)
import Type.Proxy (Proxy(..))
import Type.Trout (type (:/), type (:<|>), type (:=), type (:>), Capture, CaptureAll, Raw, Resource)
import Type.Trout.ContentType.HTML (HTML, class EncodeHTML)
import Type.Trout.ContentType.JSON (JSON)
import Type.Trout.Method (Get)
import Type.Trout.PathPiece (class FromPathPiece, class ToPathPiece)

data Home = Home

instance encodeJsonHome :: EncodeJson Home where
  encodeJson Home = jsonEmptyObject

instance encodeHTMLHome :: EncodeHTML Home where
  encodeHTML Home = h1 (text "Home")

newtype UserID = UserID String

instance fromPathPieceUserID :: FromPathPiece UserID where
  fromPathPiece s =
    case trim s of
      "" -> Left "UserID must not be blank."
      s' -> Right (UserID s')

instance toPathPieceUserID :: ToPathPiece UserID where
  toPathPiece (UserID s) = s

data User = User UserID

instance encodeUser :: EncodeJson User where
  encodeJson (User (UserID userId)) =
    assoc "userId" userId
    ~> jsonEmptyObject

data WikiPage = WikiPage String

instance encodeHTMLWikiPage :: EncodeHTML WikiPage where
  encodeHTML (WikiPage title) = text ("Viewing page: " <> title)

type UsersRoutes =
  "user" := Capture "user-id" UserID :> ("profile" := "profile" :/ Resource (Get JSON User)
                                         :<|>
                                         "friends" := "friends" :/ Resource (Get JSON (Array User)))

type TestSite =
  "home" := Resource (Get (HTML :<|> JSON) Home)
  -- nested routes with capture
  :<|> "users" := "users" :/ UsersRoutes
  -- capture all
  :<|> "wiki" := "wiki" :/ CaptureAll "segments" String :> Resource (Get HTML WikiPage)
  -- raw middleware
  :<|> "about" := "about" :/ Raw "GET"

testSite :: Proxy TestSite
testSite = Proxy
