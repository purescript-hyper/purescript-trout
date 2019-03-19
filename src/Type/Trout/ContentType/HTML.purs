module Type.Trout.ContentType.HTML
       ( HTML
       , linkTo
       , class EncodeHTML
       , encodeHTML
       , TroutURI
       ) where

import Prelude

import Control.Monad.Free (Free)
import Data.MediaType.Common (textHTML)
import Data.Tuple (Tuple(..))
import Text.Smolder.HTML (a)
import Text.Smolder.HTML.Attributes (href)
import Text.Smolder.Markup (Markup, MarkupM, (!))
import Text.Smolder.Renderer.String (render)
import Type.Trout.ContentType (class AllMimeRender, class HasMediaType, class MimeRender, getMediaType, mimeRender)
import URI (Fragment, HierPath, Host, Path, Port, Query, RelPath, URIRef, UserInfo)
import URI.HostPortPair (HostPortPair)
import URI.HostPortPair as HostPortPair
import URI.URIRef (URIRefPrintOptions)
import URI.URIRef as URIRef

-- | A content type, corresponding to the `text/html` media type.
data HTML

-- TODO: Enforce that URI comes from a GET-able resource,
-- perhaps by wrapping the URI type and adding some phantom
-- type parameter for the HTTP method.

type TroutURI = URIRef UserInfo (HostPortPair Host Port) Path HierPath RelPath Query Fragment

uriOpts ∷ Record (URIRefPrintOptions UserInfo (HostPortPair Host Port) Path HierPath RelPath Query Fragment ()) 
uriOpts =
  { printUserInfo: identity
  , printHosts: HostPortPair.print identity identity
  , printPath: identity
  , printHierPath: identity
  , printQuery: identity
  , printFragment: identity
  , printRelPath: identity
  }

-- | Helper function for generating a Smolder anchor tag based on
-- | a `URI`.
linkTo :: TroutURI -> Markup Unit -> Markup Unit
linkTo uri = a ! href (URIRef.print uriOpts uri)

-- | Encodes a value as HTML, using Smolder markup.
class EncodeHTML a where
  -- | Encode the given value as Smolder markup.
  encodeHTML :: a -> Markup Unit

instance hasMediaTypeHTML :: HasMediaType HTML where
  getMediaType _ = textHTML

instance mimeRenderHTMLEncodeHTML :: EncodeHTML a => MimeRender a HTML String where
  mimeRender _ = render <<< encodeHTML
else instance mimeRenderHTML :: MimeRender (Free (MarkupM e) Unit) HTML String where
  mimeRender p = render

instance allMimeRenderHTML :: EncodeHTML a => AllMimeRender a HTML String where
  allMimeRender p x = pure (Tuple (getMediaType p) (mimeRender p x))
