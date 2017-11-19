module Type.Trout.LinksSpec (spec) where

import Prelude
import Data.URI.URI (print)
import Type.Trout.Links (linksTo)
import Type.Trout.TestSite (UserID(..), testSite)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: forall e. Spec e Unit
spec = do
  describe "Hyper.Routing.Links" $
    describe "linksTo" $
      case linksTo testSite of
        { home, users, wiki, about } -> do

          it "returns link for Lit" $
            print home `shouldEqual` "/"

          it "returns link for nested routes" $
            case users.user (UserID "owi") of
              { profile, friends } -> do
                  print profile `shouldEqual` "/users/owi/profile"
                  print friends `shouldEqual` "/users/owi/friends"

          it "returns link for CaptureAll" $
            print (wiki ["foo", "bar", "baz.txt"]) `shouldEqual` "/wiki/foo/bar/baz.txt"

          it "returns link for Raw" $
            print about `shouldEqual` "/about"
