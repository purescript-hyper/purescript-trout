module Type.Trout.ContentType.JSON where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, parseJson, printJsonDecodeError)
import Data.Argonaut.Core (stringify)
import Data.Bifunctor (lmap)
import Data.MediaType.Common (applicationJSON)
import Data.Tuple (Tuple(..))
import Type.Trout.ContentType
  ( class AllMimeRender
  , class HasMediaType
  , class MimeParse
  , class MimeRender
  , getMediaType
  , mimeRender
  )

-- | A content type, corresponding to the `application/json` media type.
data JSON

instance hasMediaTypeJson :: HasMediaType JSON where
  getMediaType _ = applicationJSON

instance mimeRenderJson :: EncodeJson a => MimeRender a JSON String where
  mimeRender _ = stringify <<< encodeJson

instance mimeParseJson :: DecodeJson a => MimeParse String JSON a where
  mimeParse _ = lmap printJsonDecodeError <<< (decodeJson <=< parseJson)

instance allMimeRenderJson :: EncodeJson a => AllMimeRender a JSON String where
  allMimeRender p x = pure (Tuple (getMediaType p) (mimeRender p x))
