{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Lens hiding ((.=), (<.>))
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Lens
import Data.List (foldl1', sortOn)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (Down (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.IO qualified as TL
import Data.Vector qualified as V
import Data.Yaml qualified as Y
import Development.Shake hiding (Verbosity (..))
import Development.Shake.FilePath
import Markdown qualified
import Post (LocalPost, Post (..))
import Post qualified
import Route (Route)
import Route qualified
import Text.Mustache
import Utils

----------------------------------------------------------------------------
-- Menu items

-- | Menu items.
data MenuItem
  = Posts
  | LearnHaskell
  | OSS
  | Resume
  | About
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Get human-readable title of 'MenuItem'.
menuItemTitle :: MenuItem -> Text
menuItemTitle = \case
  Posts -> "Posts"
  LearnHaskell -> "Learn Haskell"
  OSS -> "OSS"
  Resume -> "Resume"
  About -> "About me"

----------------------------------------------------------------------------
-- Build system

main :: IO ()
main = shakeArgs shakeOptions $ do
  -- Helpers
  envCache <- cacheYamlFile "env.yaml"
  templateCache <- newCache' $ \() -> do
    let templatePattern = "templates/*.mustache"
    getMatchingFiles templatePattern >>= need
    liftIO (compileMustacheDir "default" (takeDirectory templatePattern))
  getMd <- newCache $ \path -> do
    env <- envCache
    getMdHelper env path
  let gatherLocalPosts ::
        FilePattern ->
        Route FilePath ->
        Action [Route.WithPage LocalPost]
      gatherLocalPosts pat route = do
        srcs <- getMatchingFiles pat
        forM srcs $ \src -> do
          need [src]
          v :: LocalPost <- getMd src >>= interpretValue . fst
          return (Route.withPage route src v)
      justFromTemplate ::
        Either Text MenuItem ->
        PName ->
        FilePath ->
        Action ()
      justFromTemplate etitle template output = do
        env <- envCache
        ts <- templateCache
        renderAndWrite
          ts
          [template, "default"]
          Nothing
          [ either (const env) (`menuItem` env) etitle,
            provideAs "title" (either id menuItemTitle etitle)
          ]
          output
  postCache <- newCache' $ \() -> do
    env <- envCache
    internalPosts <-
      fmap InternalPost
        <$> gatherLocalPosts Route.postPattern Route.post
    let externalPosts = parseExternalPosts "external_posts" env
    return $
      sortOn (Down . Post.published) (internalPosts ++ externalPosts)
  let routeTags :: Route Text
      routeTags = Route.tags (Post.tags <$> postCache)

  -- Page implementations
  Route.rule Route.notFound $ \() output ->
    justFromTemplate (Left "404 Not Found") "404" output
  Route.rule Route.atomFeed $ \() output -> do
    env <- envCache
    templates <- templateCache
    posts <- postCache
    let feedUpdated = renderIso8601 (maximum (Post.normalizedUpdated <$> posts))
    renderAndWrite
      templates
      ["atom-feed"]
      Nothing
      [ env,
        provideAs "entry" posts,
        provideAs "feed_file" (dropDirectory1 output),
        provideAs "feed_updated" feedUpdated
      ]
      output
  Route.rule Route.resumeHtml $ \input output -> do
    env <- envCache
    templates <- templateCache
    need [input]
    (v, content) <- getMd input
    let resumePdfPage = Route.page Route.resumePdf Route.resumePdfInput
    renderAndWrite
      templates
      ["resume", "default"]
      (Just content)
      [ menuItem Resume env,
        mkTitle Resume,
        v,
        provideAs "resume_pdf_page" resumePdfPage
      ]
      output
  Route.rule Route.resumePdf copyFile'
  Route.rule Route.about $ \input output -> do
    env <- envCache
    templates <- templateCache
    need [input]
    (v, content) <- getMd input
    renderAndWrite
      templates
      ["about", "default"]
      (Just content)
      [menuItem About env, mkTitle About, v]
      output
  Route.rule Route.oss $ \() output ->
    justFromTemplate (Right OSS) "oss" output
  Route.rule Route.learnHaskell $ \() output -> do
    env <- envCache
    templates <- templateCache
    internalPosts <-
      fmap InternalPost
        <$> gatherLocalPosts Route.tutorialPattern Route.tutorial
    let externalPosts = parseExternalPosts "external_tutorials" env
    renderAndWrite
      templates
      ["learn-haskell", "default"]
      Nothing
      [ menuItem LearnHaskell env,
        provideAs
          "my_tutorials"
          (sortOn (Down . Post.published) (internalPosts ++ externalPosts)),
        mkTitle LearnHaskell
      ]
      output
  Route.rule Route.posts $ \() output -> do
    env <- envCache
    templates <- templateCache
    posts <- postCache
    let tags = tagsWithPages routeTags (Post.tags posts)
    renderAndWrite
      templates
      ["posts", "default"]
      Nothing
      [ menuItem Posts env,
        provideAs "post" posts,
        provideAs "tag" tags,
        mkTitle Posts
      ]
      output
  Route.rule routeTags $ \thisTag output -> do
    env <- envCache
    templates <- templateCache
    posts <- postCache
    let tags = tagsWithPages routeTags (Post.tags posts)
    renderAndWrite
      templates
      ["posts", "default"]
      Nothing
      [ menuItem Posts env,
        provideAs "post" (Post.filterByTag thisTag posts),
        provideAs "tag" tags,
        mkTitle Posts
      ]
      output
  Route.rule Route.post $ \input output -> do
    env <- envCache
    templates <- templateCache
    need [input]
    (v, content) <- getMd input
    let tags = tagsWithPages routeTags (tagsFromPostContext v)
    renderAndWrite
      templates
      ["post", "default"]
      (Just content)
      [ provideAs "tag" tags,
        menuItem Posts env,
        v
      ]
      output
  Route.rule Route.tutorial $ \input output -> do
    env <- envCache
    templates <- templateCache
    need [input]
    (v, content) <- getMd input
    renderAndWrite
      templates
      ["post", "default"]
      (Just content)
      [menuItem LearnHaskell env, v]
      output

----------------------------------------------------------------------------
-- Helpers

selectTemplate :: PName -> Template -> Template
selectTemplate name t = t {templateActual = name}

renderAndWrite ::
  (MonadIO m) =>
  -- | Templates to use
  Template ->
  -- | Names of templates, in order
  [PName] ->
  -- | First inner value to interpolate
  Maybe TL.Text ->
  -- | Rendering context
  [Value] ->
  -- | File path where to write rendered file
  FilePath ->
  m ()
renderAndWrite ts pnames minner context out =
  liftIO . TL.writeFile out $
    foldl f (fromMaybe TL.empty minner) pnames
  where
    f inner pname =
      renderMustache
        (selectTemplate pname ts)
        (mkContext (provideAs "inner" inner : context))

menuItem :: MenuItem -> Value -> Value
menuItem item = over (key "main_menu" . _Array) . V.map $ \case
  Object m ->
    Object $
      if KeyMap.lookup "title" m == (Just . String . menuItemTitle) item
        then KeyMap.insert "active" (Bool True) m
        else m
  v -> v

getMdHelper :: Value -> FilePath -> Action (Value, TL.Text)
getMdHelper env path = do
  txt <- liftIO (T.readFile path)
  Markdown.render env txt path

interpretValue :: (FromJSON v) => Value -> Action v
interpretValue v =
  case fromJSON v of
    Error str -> fail str
    Success a -> return a

mkContext :: [Value] -> Value
mkContext = foldl1' f
  where
    f (Object m0) (Object m1) = Object (KeyMap.union m0 m1)
    f _ _ = error "context merge failed"

mkTitle :: MenuItem -> Value
mkTitle = provideAs "title" . menuItemTitle

provideAs :: (ToJSON v) => Text -> v -> Value
provideAs k v = Object (KeyMap.singleton (Key.fromText k) (toJSON v))

cacheYamlFile :: (FromJSON v) => FilePath -> Rules (Action v)
cacheYamlFile yamlFile = newCache' $ \() -> do
  need [yamlFile]
  r <- liftIO (Y.decodeFileEither yamlFile)
  case r of
    Left err -> fail (Y.prettyPrintParseException err)
    Right value -> return value

parseExternalPosts :: Text -> Value -> [Post]
parseExternalPosts k v =
  fromMaybe [] $
    v
      ^? key (Key.fromText k)
        . _Array
        . to (mapMaybe parseExternalPost . V.toList)

parseExternalPost :: Value -> Maybe Post
parseExternalPost o = do
  published <- (o ^? key "published" . _String) >>= parseDay
  title <- o ^? key "title" . _String
  url <- o ^? key "url" . _String
  return (ExternalPost published title url)

tagsFromPostContext :: Value -> Set Text
tagsFromPostContext o =
  maybe Set.empty parseTags (o ^? key "tag" . _String)

-- | Equip every tag with its corresponding page.
tagsWithPages :: Route Text -> Set Text -> Set (Route.WithPage Text)
tagsWithPages route = Set.map equipWithPage
  where
    equipWithPage x = Route.withPage route x x
