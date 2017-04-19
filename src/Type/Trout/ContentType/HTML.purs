module Type.Trout.ContentType.HTML
       ( HTML
       , linkTo
       , class EncodeHTML
       , encodeHTML
       ) where

import Prelude
import Data.MediaType.Common (textHTML)
import Data.Tuple (Tuple(..))
import Data.URI (URI, printURI)
import Text.Smolder.HTML (a)
import Text.Smolder.HTML.Attributes (href)
import Text.Smolder.Markup (Markup, MarkupM, (!))
import Text.Smolder.Renderer.String (render)
import Type.Trout.ContentType (class AllMimeRender, class HasMediaType, class MimeRender, getMediaType, mimeRender)

-- | A content type, corresponding to the `text/html` media type.
data HTML

-- TODO: Enforce that URI comes from a GET-able resource,
-- perhaps by wrapping the URI type and adding some phantom
-- type parameter for the HTTP method.

-- | Helper function for generating a Smolder anchor tag based on
-- | a `URI`.
linkTo :: URI -> Markup Unit -> Markup Unit
linkTo uri = a ! href (printURI uri)

-- | Encodes a value as HTML, using Smolder markup.
class EncodeHTML a where
  -- | Encode the given value as Smolder markup.
  encodeHTML :: a -> Markup Unit

instance hasMediaTypeHTML :: HasMediaType HTML where
  getMediaType _ = textHTML

instance mimeRenderHTML :: MimeRender (MarkupM Unit Unit) HTML String where
  mimeRender p = render

instance mimeRenderHTMLEncodeHTML :: EncodeHTML a => MimeRender a HTML String where
  mimeRender _ = render <<< encodeHTML

instance allMimeRenderHTML :: EncodeHTML a => AllMimeRender a HTML String where
  allMimeRender p x = pure (Tuple (getMediaType p) (mimeRender p x))
