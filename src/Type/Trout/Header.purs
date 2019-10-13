module Type.Trout.Header where

import Data.Int as Int
import Control.Category (identity)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Show (show)

class ToHeader x where
  toHeader :: x -> String

instance toHeaderString :: ToHeader String where
  toHeader = identity

instance toHeaderInt :: ToHeader Int where
  toHeader = show

class FromHeader x where
  fromHeader :: String -> Either String x

instance fromHeaderString :: FromHeader String where
  fromHeader = Right

instance fromHeaderInt :: FromHeader Int where
  fromHeader s =
    case Int.fromString s of
      Just n -> Right n
      Nothing -> Left ("Invalid Int: " <> s)
