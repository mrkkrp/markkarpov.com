{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Lens hiding ((.=), (<.>))
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Lens
import Data.List (foldl', foldl1', sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (Down (..))
import Data.Set (Set)
import Data.Set qualified as E
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.IO qualified as TL
import Data.Time
import Data.Time.Format.ISO8601 qualified as ISO8601
import Data.Vector qualified as V
import Data.Yaml qualified as Y
import Development.Shake hiding (Verbosity (..))
import Development.Shake.FilePath
import Lucid qualified as L
import Text.MMark qualified as MMark
import Text.MMark.Extension qualified as Ext
import Text.MMark.Extension.Common qualified as Ext
import Text.Megaparsec qualified as M
import Text.Mustache
import Text.URI (URI)
import Text.URI qualified as URI
import Text.URI.Lens (uriPath, uriScheme)
import Text.URI.QQ (scheme)

----------------------------------------------------------------------------
-- Settings

-- | Top-level output directory of the site.
outdir :: FilePath
outdir = "_build"

----------------------------------------------------------------------------
-- Routing

-- | A route equipped with 'FilePattern' for input source files and a
-- function how to get output file name from input file name. The function
-- should not mess with 'outdir', that will be done for users automatically.
data Route
  = -- | Produced from inputs
    Ins FilePattern (FilePath -> FilePath)
  | -- | Generated, fixed file
    Gen FilePath
  | -- | Generated, pattern
    GenPat FilePath

-- | A helper for defining rules.
buildRoute ::
  -- | 'Route' we want to build
  Route ->
  -- | Input file, output file
  (FilePath -> FilePath -> Action ()) ->
  Rules ()
buildRoute (Ins pat mapOut') f = do
  let mapOut x = outdir </> mapOut' x
  action $
    getMatchingFiles pat >>= need . fmap mapOut
  inputMap <- fmap ($ ()) . newCache $ \() -> do
    ifiles <- getMatchingFiles pat
    return $ Map.fromList (zip (mapOut <$> ifiles) ifiles)
  mapOut pat %> \output -> do
    input <- (Map.! output) <$> inputMap
    f input output
buildRoute (Gen outFile') f = do
  let outFile = outdir </> outFile'
  want [outFile]
  outFile %> \output ->
    f output output
buildRoute (GenPat outFile') f = do
  let outFile = outdir </> outFile'
  outFile %> \output ->
    f output output

----------------------------------------------------------------------------
-- Routes

notFoundR,
  atomFeedR,
  resumeHtmlR,
  resumePdfR,
  aboutR,
  ossR,
  writingR,
  writingPieceR,
  learnHaskellR,
  postsR,
  postR,
  tagsR,
  tutorialR ::
    Route
notFoundR = Gen "404.html"
atomFeedR = Gen "feed.atom"
resumeHtmlR = Ins "resume/resume.md" (\x -> dropDirectory1 x -<.> "html")
resumePdfR = Ins "resume/resume.pdf" dropDirectory1
aboutR = Ins "about.md" (-<.> "html")
ossR = Gen "oss.html"
writingR = Gen "writing.html"
writingPieceR = Ins "writing/*.md" (-<.> "html")
learnHaskellR = Gen "learn-haskell.html"
postsR = Gen "posts.html"
tagsR = GenPat "tag/*.html"
postR = Ins "post/*.md" (-<.> "html")
tutorialR = Ins "tutorial/*.md" (-<.> "html")

----------------------------------------------------------------------------
-- Post info

-- | Information about post.
data PostInfo
  = -- | 'LocalInfo' injection
    InternalPost LocalInfo
  | -- | Published, title, URL
    ExternalPost Day Text Text
  deriving (Eq, Show)

-- | The instance is only used for atom feed.
instance ToJSON PostInfo where
  toJSON (InternalPost localInfo) = toJSON localInfo
  toJSON (ExternalPost published title url) =
    object
      [ "title" .= title,
        "published" .= renderDay published,
        "published_iso8601" .= renderIso8601 published,
        "updated" .= renderDay published,
        "updated_iso8601" .= renderIso8601 published,
        "desc" .= ("" :: Text),
        "url" .= url
      ]

-- | Information about a post that is hosted on my site.
data LocalInfo = LocalInfo
  { localTitle :: !Text,
    localPublished :: !Day,
    localUpdated :: !(Maybe Day),
    localDesc :: !Text,
    localFile :: !FilePath,
    localTags :: Set Text
  }
  deriving (Eq, Show)

instance FromJSON LocalInfo where
  parseJSON = withObject "local post metadata" $ \o -> do
    localTitle <- o .: "title"
    localPublished <- (o .: "date") >>= (.: "published") >>= parseDay
    localUpdated <-
      (o .: "date")
        >>= (.:? "updated")
        >>= maybe (pure Nothing) (fmap Just . parseDay)
    localDesc <- o .: "desc"
    let localFile = ""
    localTags <- parseTags <$> (o .:? "tag" .!= "")
    return LocalInfo {..}

instance ToJSON LocalInfo where
  toJSON info@LocalInfo {..} =
    object
      [ "title" .= localTitle,
        "published" .= renderDay localPublished,
        "published_iso8601" .= renderIso8601 localPublished,
        "updated" .= fmap renderDay localUpdated,
        "updated_iso8601" .= renderIso8601 (localNormalizedUpdated info),
        "desc" .= localDesc,
        "file" .= ("/" ++ localFile)
      ]

-- | Information about a piece of writing.
data WritingPiece = WritingPiece
  { wpieceTitle :: !Text,
    wpieceType :: !Text,
    wpieceDesc :: !(Maybe Text),
    wpieceFile :: !FilePath,
    wpieceDateStarted :: !Day,
    wpieceDateFinished :: !(Maybe Day)
  }
  deriving (Eq, Show)

instance FromJSON WritingPiece where
  parseJSON = withObject "metadata of a piece of writing" $ \o -> do
    wpieceTitle <- o .: "title"
    wpieceType <- o .: "type"
    wpieceDesc <- o .:? "desc"
    let wpieceFile = ""
    wpieceDateStarted <-
      (o .: "date") >>= (.: "started") >>= parseDay
    wpieceDateFinished <-
      (o .: "date")
        >>= (.:? "finished")
        >>= maybe (pure Nothing) (fmap Just . parseDay)
    return WritingPiece {..}

instance ToJSON WritingPiece where
  toJSON WritingPiece {..} =
    object
      [ "title" .= wpieceTitle,
        "type" .= wpieceType,
        "desc" .= wpieceDesc,
        "file" .= ("/" ++ wpieceFile),
        "started" .= renderDay wpieceDateStarted,
        "finished" .= fmap renderDay wpieceDateFinished
      ]

----------------------------------------------------------------------------
-- Menu items

-- | Menu items.
data MenuItem
  = Posts
  | LearnHaskell
  | OSS
  | Writing
  | Resume
  | About
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Get human-readable title of 'MenuItem'.
menuItemTitle :: MenuItem -> Text
menuItemTitle = \case
  Posts -> "Posts"
  LearnHaskell -> "Learn Haskell"
  OSS -> "OSS"
  Writing -> "Writing"
  Resume -> "Resume"
  About -> "About me"

----------------------------------------------------------------------------
-- Build system

main :: IO ()
main = shakeArgs shakeOptions $ do
  -- Helpers

  phony "clean" $ do
    putNormal ("Cleaning files in " ++ outdir)
    removeFilesAfter outdir ["//*"]
  commonEnv <- fmap ($ ()) . newCache $ \() -> do
    let commonEnvFile = "env.yaml"
    need [commonEnvFile]
    r <- liftIO (Y.decodeFileEither commonEnvFile)
    case r of
      Left err -> fail (Y.prettyPrintParseException err)
      Right value -> return value
  templates <- fmap ($ ()) . newCache $ \() -> do
    let templateP = "templates/*.mustache"
    getMatchingFiles templateP >>= need
    liftIO (compileMustacheDir "default" (takeDirectory templateP))
  getPost <- newCache $ \path -> do
    env <- commonEnv
    getPostHelper env path
  let gatherLocalInfo ::
        (Ord a) =>
        Route ->
        (LocalInfo -> a) ->
        Action [LocalInfo]
      gatherLocalInfo (Ins pat mapOut) f = do
        ps' <- getMatchingFiles pat
        fmap (sortOn f) . forM ps' $ \post -> do
          need [post]
          v <- getPost post >>= interpretValue . fst
          return v {localFile = mapOut post}
      gatherLocalInfo (Gen outFile) _ =
        fail $ "cannot gather local info about: " ++ outFile
      gatherLocalInfo (GenPat pat) _ =
        fail $ "cannot gather local info about: " ++ pat
      gatherWritingPieces ::
        (Ord a) =>
        Route ->
        (WritingPiece -> a) ->
        Action [WritingPiece]
      gatherWritingPieces (Ins pat mapOut) f = do
        ps' <- getMatchingFiles pat
        fmap (sortOn f) . forM ps' $ \post -> do
          need [post]
          v <- getPost post >>= interpretValue . fst
          return v {wpieceFile = mapOut post}
      gatherWritingPieces (Gen outFile) _ =
        fail $ "cannot gather info about: " ++ outFile
      gatherWritingPieces (GenPat pat) _ =
        fail $ "cannot gather info about: " ++ pat
      justFromTemplate ::
        Either Text MenuItem ->
        PName ->
        FilePath ->
        Action ()
      justFromTemplate etitle template output = do
        env <- commonEnv
        ts <- templates
        renderAndWrite
          ts
          [template, "default"]
          Nothing
          [ either (const env) (`menuItem` env) etitle,
            provideAs "title" (either id menuItemTitle etitle)
          ]
          output
  allPosts <- fmap ($ ()) . newCache $ \() -> do
    env <- commonEnv
    ips <-
      fmap InternalPost
        <$> gatherLocalInfo postR (Down . localPublished)
    let ets = parseExternalPosts "external_posts" env
    return $
      sortOn (Down . postInfoPublished) (ips ++ ets)
  allWritingPieces <- fmap ($ ()) . newCache $ \() ->
    gatherWritingPieces writingPieceR wpieceDateStarted

  -- Page implementations

  buildRoute notFoundR $ \_ output ->
    justFromTemplate (Left "404 Not Found") "404" output
  buildRoute atomFeedR $ \_ output -> do
    env <- commonEnv
    ts <- templates
    ps <- allPosts
    let feedUpdated =
          renderIso8601 $
            maximum (normalizedUpdated <$> ps)
    renderAndWrite
      ts
      ["atom-feed"]
      Nothing
      [ env,
        provideAs "entry" ps,
        provideAs "feed_file" (dropDirectory1 output),
        provideAs "feed_updated" feedUpdated
      ]
      output
  buildRoute resumeHtmlR $ \input output -> do
    env <- commonEnv
    ts <- templates
    need [input]
    (v, content) <- getPost input
    renderAndWrite
      ts
      ["resume", "default"]
      (Just content)
      [menuItem Resume env, mkTitle Resume, v]
      output
  buildRoute resumePdfR copyFile'
  buildRoute aboutR $ \input output -> do
    env <- commonEnv
    ts <- templates
    need [input]
    (v, content) <- getPost input
    renderAndWrite
      ts
      ["about", "default"]
      (Just content)
      [menuItem About env, mkTitle About, v]
      output
  buildRoute ossR $ \_ output ->
    justFromTemplate (Right OSS) "oss" output
  buildRoute learnHaskellR $ \_ output -> do
    env <- commonEnv
    ts <- templates
    its <-
      fmap InternalPost
        <$> gatherLocalInfo tutorialR (Down . localPublished)
    let ets = parseExternalPosts "external_tutorials" env
    renderAndWrite
      ts
      ["learn-haskell", "default"]
      Nothing
      [ menuItem LearnHaskell env,
        provideAs
          "my_tutorials"
          (sortOn (Down . postInfoPublished) (its ++ ets)),
        mkTitle LearnHaskell
      ]
      output
  buildRoute postsR $ \_ output -> do
    env <- commonEnv
    ts <- templates
    ps <- allPosts
    let tags = getTags ps
    need (tagToPath <$> E.toList tags)
    renderAndWrite
      ts
      ["posts", "default"]
      Nothing
      [ menuItem Posts env,
        provideAs "post" ps,
        provideAs "tag" tags,
        mkTitle Posts
      ]
      output
  buildRoute writingR $ \_ output -> do
    env <- commonEnv
    ts <- templates
    ps <- allWritingPieces
    renderAndWrite
      ts
      ["writing", "default"]
      Nothing
      [ menuItem Writing env,
        provideAs "item" ps,
        mkTitle Writing
      ]
      output
  buildRoute tagsR $ \_ output -> do
    let tag = pathToTag output
    env <- commonEnv
    ts <- templates
    ps <- allPosts
    renderAndWrite
      ts
      ["posts", "default"]
      Nothing
      [ menuItem Posts env,
        provideAs "post" (filterByTag tag ps),
        provideAs "tag" (getTags ps),
        mkTitle Posts
      ]
      output
  buildRoute postR $ \input output -> do
    env <- commonEnv
    ts <- templates
    need [input]
    (v, content) <- getPost input
    renderAndWrite
      ts
      ["post", "default"]
      (Just content)
      [ provideAs "tag" (tagsFromPostContext v),
        menuItem Posts env,
        v,
        mkLocation output
      ]
      output
  buildRoute tutorialR $ \input output -> do
    env <- commonEnv
    ts <- templates
    need [input]
    (v, content) <- getPost input
    renderAndWrite
      ts
      ["post", "default"]
      (Just content)
      [menuItem LearnHaskell env, v, mkLocation output]
      output
  buildRoute writingPieceR $ \input output -> do
    env <- commonEnv
    ts <- templates
    need [input]
    (v, content) <- getPost input
    renderAndWrite
      ts
      ["wpiece", "default"]
      (Just content)
      [menuItem Writing env, v]
      output

----------------------------------------------------------------------------
-- Custom MMark extensions

addTableClasses :: MMark.Extension
addTableClasses = Ext.blockRender $ \old block ->
  case block of
    t@(Ext.Table _ _) -> L.with (old t) [L.class_ "table table-striped"]
    other -> old other

addImageClasses :: MMark.Extension
addImageClasses = Ext.inlineRender $ \old inline ->
  case inline of
    i@Ext.Image {} -> L.with (old i) [L.class_ "img-fluid"]
    other -> old other

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

colorSample :: MMark.Extension
colorSample = Ext.inlineRender $ \old inline ->
  case inline of
    l@(Ext.Link _ uri _) ->
      if URI.uriScheme uri == Just [scheme|c|]
        then case uri ^. uriPath of
          [] -> old l
          (colorHex : _) ->
            let colorStyle = "color:#" <> URI.unRText colorHex <> ";"
             in L.span_ [L.class_ "fa fa-square", L.style_ colorStyle] ""
        else old l
    other -> old other

----------------------------------------------------------------------------
-- Helpers

getMatchingFiles :: FilePattern -> Action [FilePath]
getMatchingFiles = getDirectoryFiles "" . pure

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

getTags :: [PostInfo] -> Set Text
getTags = foldl' f E.empty
  where
    f s = \case
      InternalPost LocalInfo {..} -> E.union s localTags
      ExternalPost {} -> s

tagToPath :: Text -> FilePath
tagToPath tag = outdir </> "tag" </> T.unpack tag <.> "html"

pathToTag :: FilePath -> Text
pathToTag path =
  T.pack . dropExtensions $
    makeRelative (outdir </> "tag") path

filterByTag :: Text -> [PostInfo] -> [PostInfo]
filterByTag tag = filter f
  where
    f = \case
      InternalPost LocalInfo {..} -> tag `elem` localTags
      ExternalPost {} -> False

getPostHelper :: Value -> FilePath -> Action (Value, TL.Text)
getPostHelper env path = do
  txt <- liftIO (T.readFile path)
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
                Ext.obfuscateEmail "protected-email",
                Ext.punctuationPrettifier,
                Ext.ghcSyntaxHighlighter,
                Ext.skylighting,
                Ext.toc "toc" toc,
                addTableClasses,
                addImageClasses,
                provideSocialUrls env,
                colorSample
              ]
              doc
          v = fromMaybe (object []) (MMark.projectYaml doc)
      return (v, L.renderText (MMark.render r))

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

mkLocation :: FilePath -> Value
mkLocation = provideAs "location" . dropDirectory1

provideAs :: (ToJSON v) => Text -> v -> Value
provideAs k v = Object (KeyMap.singleton (Key.fromText k) (toJSON v))

parseDay :: (MonadFail m) => Text -> m Day
parseDay = parseTimeM True defaultTimeLocale "%B %e, %Y" . T.unpack

localNormalizedUpdated :: LocalInfo -> Day
localNormalizedUpdated LocalInfo {..} =
  fromMaybe localPublished localUpdated

normalizedUpdated :: PostInfo -> Day
normalizedUpdated = \case
  InternalPost localInfo -> localNormalizedUpdated localInfo
  ExternalPost published _ _ -> published

renderDay :: Day -> String
renderDay = formatTime defaultTimeLocale "%B %e, %Y"

renderIso8601 :: Day -> String
renderIso8601 day =
  ISO8601.formatShow
    (ISO8601.calendarFormat ISO8601.ExtendedFormat)
    day
    <> "T00:00:00Z"

parseExternalPosts :: Text -> Value -> [PostInfo]
parseExternalPosts k v =
  fromMaybe [] $
    v
      ^? key (Key.fromText k)
        . _Array
        . to (mapMaybe parseExternalPost . V.toList)

parseExternalPost :: Value -> Maybe PostInfo
parseExternalPost o = do
  published <- (o ^? key "published" . _String) >>= parseDay
  title <- o ^? key "title" . _String
  url <- o ^? key "url" . _String
  return (ExternalPost published title url)

postInfoPublished :: PostInfo -> Day
postInfoPublished = \case
  InternalPost localInfo -> localPublished localInfo
  ExternalPost published _ _ -> published

parseTags :: Text -> Set Text
parseTags = E.fromList . T.words . T.toLower

tagsFromPostContext :: Value -> Set Text
tagsFromPostContext o =
  maybe E.empty parseTags (o ^? key "tag" . _String)
