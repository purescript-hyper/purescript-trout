module Type.Trout.ContentType where

import Prelude
import Data.List.NonEmpty (NonEmptyList)
import Data.MediaType (MediaType)
import Data.Tuple (Tuple(..))
import Type.Proxy (Proxy(..))
import Type.Trout (type (:<|>))

-- | Associates a media type to a Trout content type.
class HasMediaType ct where
  getMediaType :: Proxy ct -> MediaType

-- | Renders a value of type `a`, as appropriate for content type `ct`, as
-- | a value of type `b`.
class MimeRender a ct b | a -> b, ct -> b  where
  mimeRender :: Proxy ct -> a -> b

-- | Renders a value of type `a`, as appropriate for each content type in `cts`,
-- | as a non-empty list of values of type `b`, corresponding to the content
-- | types. Content types can be a single type, e.g. `HTML`, or multiple types
-- | separated by `:<|>`, e.g. `(HTML :<|> JSON)`.
-- |
-- | This is used to support content negotiation.
class AllMimeRender a cts b | a -> b, cts -> b where
  allMimeRender :: Proxy cts -> a -> NonEmptyList (Tuple MediaType b)

instance allMimeRenderAltAlt :: ( MimeRender a ct1 b
                                , HasMediaType ct1
                                , AllMimeRender a (ct2 :<|> ct3) b
                                , HasMediaType ct2
                                )
                                => AllMimeRender a (ct1 :<|> ct2 :<|> ct3) b where
  allMimeRender _ a =
    pure (Tuple (getMediaType p) (mimeRender p a)) <> allMimeRender p' a
    where
      p = Proxy :: Proxy ct1
      p' = Proxy :: Proxy (ct2 :<|> ct3)

instance allMimeRenderAlt :: ( MimeRender a ct1 b
                             , HasMediaType ct1
                             , MimeRender a ct2 b
                             , HasMediaType ct2
                             )
                             => AllMimeRender a (ct1 :<|> ct2) b where
  allMimeRender _ a =
    pure (Tuple (getMediaType p1) (mimeRender p1 a))
    <> pure (Tuple (getMediaType p2) (mimeRender p2 a))
    where
      p1 = Proxy :: Proxy ct1
      p2 = Proxy :: Proxy ct2
