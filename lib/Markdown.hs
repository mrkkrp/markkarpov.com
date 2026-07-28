{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Markdown
  ( render,
  )
where

import Control.Lens hiding ((.=), (<.>))
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.Lens
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Development.Shake hiding (Verbosity (..))
import Lucid qualified as L
import Text.MMark qualified as MMark
import Text.MMark.Extension qualified as Ext
import Text.MMark.Extension.Common qualified as Ext
import Text.Megaparsec qualified as M
import Text.URI (URI)
import Text.URI qualified as URI
import Text.URI.Lens (uriPath, uriScheme)
import Text.URI.QQ (scheme)

-- | Render a markdown document.
render :: Value -> Text -> FilePath -> Action (Value, TL.Text)
render env txt path =
  case MMark.parse path txt of
    Left bundle -> fail (M.errorBundlePretty bundle)
    Right doc -> do
      let toc = MMark.runScanner doc (Ext.tocScanner (\x -> x > 1 && x < 5))
          r =
            MMark.useExtensions
              [ Ext.fontAwesome,
                Ext.footnotes,
                Ext.kbd,
                Ext.linkTarget,
                Ext.mathJax (Just '$'),
                Ext.punctuationPrettifier,
                Ext.ghcSyntaxHighlighter,
                Ext.skylighting,
                Ext.toc "toc" toc,
                addTableClasses,
                addImageClasses,
                addHeadingAnchors,
                addLinkRel,
                provideSocialUrls env
              ]
              doc
          v = fromMaybe (object []) (MMark.projectYaml doc)
      return (v, L.renderText (MMark.render r))

addTableClasses :: MMark.Extension
addTableClasses = Ext.blockRender $ \old block ->
  case block of
    t@(Ext.Table _ _) -> L.with (old t) [L.class_ "site-table"]
    other -> old other

addImageClasses :: MMark.Extension
addImageClasses = Ext.inlineRender $ \old inline ->
  case inline of
    i@Ext.Image {} -> L.with (old i) [L.class_ "site-image"]
    other -> old other

-- | Add @rel="noopener noreferrer"@ to external links (those with an http/https
-- scheme). Prevents reverse-tabnabbing on @target="_blank"@ links and follows
-- current best practice for cross-origin links generally.
addLinkRel :: MMark.Extension
addLinkRel = Ext.inlineRender $ \old inline ->
  case inline of
    l@(Ext.Link _ uri _)
      | isExternal uri ->
          L.with (old l) [L.rel_ "noopener noreferrer"]
    other -> old other
  where
    isExternal uri = case URI.uriScheme uri of
      Just s -> s == [scheme|http|] || s == [scheme|https|]
      Nothing -> False

-- | Statically append a permalink anchor to section headings h2-h4. The
-- fragment is computed with the same 'Ext.headerId' MMark uses to emit the
-- heading @id@, so the anchor always points at its own heading.
addHeadingAnchors :: MMark.Extension
addHeadingAnchors = Ext.blockRender $ \old block ->
  case block of
    Ext.Heading2 x -> heading L.h2_ x
    Ext.Heading3 x -> heading L.h3_ x
    Ext.Heading4 x -> heading L.h4_ x
    other -> old other
  where
    heading tag (ois, inner) =
      let inlines = Ext.getOis ois
          anchorId = Ext.headerId inlines
          fragment = URI.render (Ext.headerFragment anchorId)
          anchor =
            L.a_
              [L.class_ "anchor", L.href_ fragment, L.term "aria-hidden" "true"]
              anchorIcon
       in L.with tag [L.id_ anchorId] (inner <> anchor)

-- | The little link glyph shown next to headings (was drawn by anchor.js).
anchorIcon :: L.Html ()
anchorIcon =
  L.toHtmlRaw
    ( "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"16\" height=\"16\" "
        <> "viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" "
        <> "stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\" "
        <> "class=\"inline-block align-[-0.125em]\">"
        <> "<path d=\"M10 13a5 5 0 0 0 7.54.54l3-3a5 5 0 0 0-7.07-7.07l-1.72 1.71\"></path>"
        <> "<path d=\"M14 11a5 5 0 0 0-7.54-.54l-3 3a5 5 0 0 0 7.07 7.07l1.71-1.71\"></path>"
        <> "</svg>" ::
        Text
    )

provideSocialUrls :: Value -> MMark.Extension
provideSocialUrls v = Ext.inlineTrans $ \case
  l@(Ext.Link inner uri mtitle) ->
    if URI.uriScheme uri == Just [scheme|social|]
      then case uri ^. uriPath of
        [x] ->
          case v
            ^? key "social"
              . key (Key.fromText (URI.unRText x))
              . _String
              . getURI of
            Nothing -> Ext.Plain "!lookup failed!"
            Just t ->
              if Ext.asPlainText inner == "x"
                then
                  Ext.Link
                    (Ext.Plain (URI.render t) :| [])
                    ((uriScheme ?~ [scheme|mailto|]) t)
                    mtitle
                else Ext.Link inner t mtitle
        _ -> l
      else l
  other -> other

getURI :: Traversal' Text URI
getURI f txt = maybe txt URI.render <$> traverse f (URI.mkURI txt :: Maybe URI)
