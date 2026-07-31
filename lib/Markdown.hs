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
              [ fontAwesomeSvg,
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
                disableBullets,
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

-- | The marker that opts a bullet list out of its bullets. Writing
--
-- > * NOBULLETS
-- >   * first item
-- >   * second item
--
-- renders the inner list without bullet markers (the outer wrapper and the
-- marker itself are dropped). See 'disableBullets'.
noBulletsMarker :: Text
noBulletsMarker = "NOBULLETS"

-- | Render a bullet list without its bullets when it is written as a nested
-- list under a lone @NOBULLETS@ marker (see 'noBulletsMarker'). This is an
-- explicit, content-agnostic opt-out controlled entirely from the markdown
-- source; the stylesheet drops the markers via the @no-bullets@ class.
disableBullets :: MMark.Extension
disableBullets = Ext.blockRender $ \old block ->
  case block of
    Ext.UnorderedList (item :| [])
      | (marker : Ext.UnorderedList sub : _) <- item,
        isMarker marker ->
          L.with (old (Ext.UnorderedList sub)) [L.class_ "no-bullets"]
    other -> old other
  where
    isMarker blk = case blk of
      Ext.Naked (ois, _) -> plain ois == noBulletsMarker
      Ext.Paragraph (ois, _) -> plain ois == noBulletsMarker
      _ -> False
    plain = Ext.asPlainText . Ext.getOis

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
          let field = URI.unRText x
              social l' = v ^? key "social" . key (Key.fromText field) . l'
              -- The email field is a bare address in env.yaml, so it needs a
              -- mailto: scheme; every other social field is already a URL.
              withScheme = if field == "email" then uriScheme ?~ [scheme|mailto|] else id
           in case (,) <$> social _String <*> social (_String . getURI) of
                Nothing -> Ext.Plain "!lookup failed!"
                Just (raw, t) ->
                  if Ext.asPlainText inner == "x"
                    then
                      -- The "x" sentinel means: show the raw social value
                      -- as the link text (rendering the URI would
                      -- percent-encode it, e.g. turning "@" into "%40").
                      Ext.Link (Ext.Plain raw :| []) (withScheme t) mtitle
                    else Ext.Link inner (withScheme t) mtitle
        _ -> l
      else l
  other -> other

getURI :: Traversal' Text URI
getURI f txt = maybe txt URI.render <$> traverse f (URI.mkURI txt :: Maybe URI)

-- | Render @\<fa:name\>@ autolinks as inline SVG icons. This site does not
-- load the Font Awesome webfont, so the upstream 'Ext.fontAwesome' (which
-- emits empty @\<span class="fa ...">@ elements) would render nothing. Here we
-- emit a self-contained SVG for each supported icon instead. An unknown name
-- renders a visible @!icon:name!@ marker so typos are caught rather than
-- silently dropped.
fontAwesomeSvg :: MMark.Extension
fontAwesomeSvg = Ext.inlineRender $ \old inline ->
  case inline of
    l@(Ext.Link _ uri _)
      | URI.uriScheme uri == Just [scheme|fa|] ->
          case uri ^. uriPath of
            [name] -> case lookup (URI.unRText name) icons of
              Just svg -> L.toHtmlRaw svg
              Nothing -> L.toHtml ("!icon:" <> URI.unRText name <> "!")
            _ -> old l
    other -> old other
  where
    -- The SVGs mirror the templates/icon-*.mustache partials.
    icons :: [(Text, Text)]
    icons =
      [ ("envelope", "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\" class=\"inline-block align-[-0.125em]\" aria-hidden=\"true\"><rect x=\"2\" y=\"4\" width=\"20\" height=\"16\" rx=\"2\"></rect><path d=\"m22 7-8.97 5.7a1.94 1.94 0 0 1-2.06 0L2 7\"></path></svg>"),
        ("github", "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" fill=\"currentColor\" class=\"inline-block align-[-0.125em]\" aria-hidden=\"true\"><path d=\"M12 .297c-6.63 0-12 5.373-12 12 0 5.303 3.438 9.8 8.205 11.385.6.113.82-.258.82-.577 0-.285-.01-1.04-.015-2.04-3.338.724-4.042-1.61-4.042-1.61-.546-1.387-1.333-1.756-1.333-1.756-1.089-.745.083-.729.083-.729 1.205.084 1.839 1.237 1.839 1.237 1.07 1.834 2.807 1.304 3.492.997.107-.775.418-1.305.762-1.604-2.665-.305-5.467-1.334-5.467-5.931 0-1.311.469-2.381 1.236-3.221-.124-.303-.535-1.524.117-3.176 0 0 1.008-.322 3.301 1.23A11.5 11.5 0 0 1 12 5.803c1.02.005 2.047.138 3.006.404 2.291-1.552 3.297-1.23 3.297-1.23.653 1.653.242 2.874.118 3.176.77.84 1.235 1.911 1.235 3.221 0 4.609-2.807 5.624-5.479 5.921.43.372.823 1.102.823 2.222 0 1.606-.014 2.898-.014 3.293 0 .322.216.694.825.576C20.565 22.092 24 17.592 24 12.297c0-6.627-5.373-12-12-12\"></path></svg>"),
        ("linkedin", "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" fill=\"currentColor\" class=\"inline-block align-[-0.125em]\" aria-hidden=\"true\"><path d=\"M20.447 20.452h-3.554v-5.569c0-1.328-.027-3.037-1.852-3.037-1.853 0-2.136 1.445-2.136 2.939v5.667H9.351V9h3.414v1.561h.046c.477-.9 1.637-1.85 3.37-1.85 3.601 0 4.267 2.37 4.267 5.455v6.286zM5.337 7.433a2.062 2.062 0 0 1-2.063-2.065 2.064 2.064 0 1 1 2.063 2.065zm1.782 13.019H3.555V9h3.564v11.452zM22.225 0H1.771C.792 0 0 .774 0 1.729v20.542C0 23.227.792 24 1.771 24h20.451C23.2 24 24 23.227 24 22.271V1.729C24 .774 23.2 0 22.222 0h.003z\"></path></svg>"),
        ("reddit", "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" fill=\"currentColor\" class=\"inline-block align-[-0.125em]\" aria-hidden=\"true\"><path d=\"M12 0A12 12 0 0 0 0 12a12 12 0 0 0 12 12 12 12 0 0 0 12-12A12 12 0 0 0 12 0zm5.01 4.744c.688 0 1.25.561 1.25 1.249a1.25 1.25 0 0 1-2.498.056l-2.597-.547-.8 3.747c1.824.07 3.48.632 4.674 1.488.308-.309.73-.491 1.207-.491.968 0 1.754.786 1.754 1.754 0 .716-.435 1.333-1.01 1.614a3.111 3.111 0 0 1 .042.52c0 2.694-3.13 4.87-7.004 4.87-3.874 0-7.004-2.176-7.004-4.87 0-.183.015-.366.043-.534A1.748 1.748 0 0 1 4.028 12c0-.968.786-1.754 1.754-1.754.463 0 .898.196 1.207.49 1.207-.883 2.878-1.43 4.744-1.487l.885-4.182a.342.342 0 0 1 .14-.197.35.35 0 0 1 .238-.042l2.906.617a1.214 1.214 0 0 1 1.108-.701zM9.25 12C8.561 12 8 12.562 8 13.25c0 .687.561 1.248 1.25 1.248.687 0 1.248-.561 1.248-1.249 0-.688-.561-1.249-1.249-1.249zm5.5 0c-.688 0-1.249.561-1.249 1.25 0 .687.561 1.248 1.25 1.248.688 0 1.249-.561 1.249-1.249 0-.687-.562-1.249-1.25-1.249zm-5.466 3.99a.327.327 0 0 0-.231.094.33.33 0 0 0 0 .463c.842.842 2.484.913 2.961.913.477 0 2.105-.056 2.961-.913a.361.361 0 0 0 .029-.463.33.33 0 0 0-.464 0c-.547.533-1.684.73-2.512.73-.828 0-1.979-.196-2.512-.73a.326.326 0 0 0-.232-.095z\"></path></svg>"),
        ("file-pdf", "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\" class=\"inline-block align-[-0.125em]\" aria-hidden=\"true\"><path d=\"M15 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V7z\"></path><path d=\"M14 2v4a2 2 0 0 0 2 2h4\"></path><path d=\"M9 15v-2h1a1 1 0 0 1 0 2z\"></path><path d=\"M13 17v-4h1a1.5 1.5 0 0 1 0 3h-1\"></path></svg>")
      ]
