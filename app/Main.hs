{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Main (main) where

import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Lens
import Data.Default.Class
import Data.List (sortBy, foldl1')
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (comparing, Down (..))
import Data.Proxy
import Data.Tagged
import Data.Text (Text)
import Data.Time
import Development.Shake
import Development.Shake.FilePath
import Text.Blaze.Html.Renderer.Text
import Text.Mustache
import Text.Pandoc hiding (Template, Null)
import qualified Data.HashMap.Strict as HM
import qualified Data.Set            as S
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as TE
import qualified Data.Text.IO        as T
import qualified Data.Text.Lazy      as TL
import qualified Data.Text.Lazy.IO   as TL
import qualified Data.Vector         as V
import qualified Data.Yaml           as Y

----------------------------------------------------------------------------
-- Settings

outdir :: FilePath
outdir = "_build"

----------------------------------------------------------------------------
-- Routing

-- | Pattern for a 'Route'.

newtype Pat r = Pat { unPat :: FilePattern }

class Route (r :: Routes) where

  -- | Pattern for source files.
  pat :: Pat r

  -- | How to get input file name given output file name.
  mapIn :: Tagged r (FilePath -> FilePath)
  mapIn = Tagged dropDirectory1

  -- | How to get output file name given input file name.
  mapOut :: Tagged r (FilePath -> FilePath)
  mapOut = Tagged (\x -> outdir </> x)

-- | Input pattern, aka 'pat' mapped to output.

outPattern :: forall r. Route r => Pat r
outPattern = (Pat . f . unPat) (pat @r)
  where
    f = unTagged (mapOut @r)

-- | Defined rotues.

data Routes
  = PostR
  | CssR
  | JsR
  | ImgR
  | RawR
  | AttachmentR
  | MTutorialR
  | TutorialR
  | ResumeHtmlR
  | ResumePdfR

instance Route 'PostR where
  pat    = Pat "post/*.md"
  mapIn  = Tagged (\x -> dropDirectory1 x -<.> "md")
  mapOut = Tagged (\x -> outdir </> x -<.> "html")

instance Route 'CssR where
  pat    = Pat "static/css/*.css"

instance Route 'JsR where
  pat    = Pat "static/js/*.js"

instance Route 'ImgR where
  pat    = Pat "static/img/*"

instance Route 'RawR where
  pat    = Pat "raw/*"
  mapIn  = Tagged (\x -> "raw" </> dropDirectory1 x)
  mapOut = Tagged (\x -> outdir </> dropDirectory1 x)

instance Route 'AttachmentR where
  pat    = Pat "attachment/*"

instance Route 'MTutorialR where
  pat    = Pat "megaparsec/*.md"
  mapIn  = Tagged (\x -> dropDirectory1 x -<.> "md")
  mapOut = Tagged (\x -> outdir </> x -<.> "html")

instance Route 'TutorialR where
  pat    = Pat "tutorial/*.md"
  mapIn  = Tagged (\x -> dropDirectory1 x -<.> "md")
  mapOut = Tagged (\x -> outdir </> x -<.> "html")

instance Route 'ResumeHtmlR where
  pat    = Pat "resume/resume.md"
  mapIn  = Tagged (\x -> "resume" </> dropDirectory1 x -<.> "md")
  mapOut = Tagged (\x -> outdir </> dropDirectory1 x -<.> "html")

instance Route 'ResumePdfR where
  pat    = Pat "resume/resume.pdf"
  mapIn  = Tagged (\x -> "resume" </> dropDirectory1 x)
  mapOut = Tagged (\x -> outdir </> dropDirectory1 x)

-- | TODO Find a way to abstract working with these files.

aboutFile, ossFile, notFoundFile, learnFile, postsFile, atomFile :: FilePath
aboutFile    = "about.html"
ossFile      = "oss.html"
notFoundFile = "404.html"
learnFile    = "learn-haskell.html"
postsFile    = "posts.html"
atomFile     = "feed.atom"

cmnOut :: FilePath -> FilePath
cmnOut x = outdir </> x

----------------------------------------------------------------------------
-- Post info

data PostInfo = PostInfo
  { postTitle      :: Text
  , postPublished  :: Day
  , postUpdated    :: Maybe Day
  , postDesc       :: Text
  , postDifficulty :: Maybe Int
  , postFile       :: FilePath
  } deriving (Eq, Show)

instance FromJSON PostInfo where
  parseJSON = withObject "post metadata" $ \o -> do
    postTitle     <- o .: "title"
    postPublished <- (o .: "date") >>= (.: "published") >>= parseDay
    postUpdated   <- (o .: "date") >>= (.:? "updated")  >>=
      maybe (pure Nothing) (fmap Just . parseDay)
    postDesc      <- o .: "desc"
    postDifficulty <- o .:? "difficulty"
    let postFile = ""
    return PostInfo {..}

instance ToJSON PostInfo where
  toJSON info@PostInfo {..} = object
    [ "title"             .= postTitle
    , "published"         .= renderDay postPublished
    , "published_iso8601" .= renderIso8601 postPublished
    , "updated"           .= fmap renderDay postUpdated
    , "updated_iso8601"   .= renderIso8601 (normalizedUpdated info)
    , "desc"              .= postDesc
    , "file"              .= postFile ]

data TutorialInfo
  = InternalTutorial PostInfo      -- ^ 'PostInfo' injection
  | ExternalTutorial Day Text Text -- ^ Published, title, URL
  deriving (Eq, Show)

instance ToJSON TutorialInfo where
  toJSON (InternalTutorial postInfo) = toJSON postInfo
  toJSON (ExternalTutorial published title url) = object
    [ "published" .= renderDay published
    , "title"     .= title
    , "url"       .= url ]

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

menuItemTitle :: MenuItem -> Text
menuItemTitle = \case
  Posts        -> "Posts"
  LearnHaskell -> "Learn Haskell"
  OSS          -> "OSS"
  Resume       -> "Resume"
  About        -> "About"

----------------------------------------------------------------------------
-- Build system

main :: IO ()
main = shakeArgs shakeOptions $ do

  let z :: forall r. Route r => Proxy r -> Rules ()
      z Proxy = action $
        getDirFiles (pat @r) >>= need . fmap (unTagged $ mapOut @r)

  z @'PostR Proxy
  z @'CssR Proxy
  z @'JsR Proxy
  z @'ImgR Proxy
  z @'RawR Proxy
  z @'AttachmentR Proxy
  z @'MTutorialR Proxy
  z @'TutorialR Proxy
  z @'ResumeHtmlR Proxy
  z @'ResumePdfR Proxy

  want $ cmnOut <$>
    [aboutFile, ossFile, notFoundFile, learnFile, postsFile, atomFile]

  phony "clean" $ do
    putNormal ("Cleaning files in " ++ outdir)
    removeFilesAfter outdir ["//*"]

  commonEnv <- newCache $ \() -> do
    let commonEnvFile = "config/env.yaml"
    need [commonEnvFile]
    r <- liftIO (Y.decodeFileEither commonEnvFile)
    case r of
      Left  err   -> fail (Y.prettyPrintParseException err)
      Right value -> return value

  templates <- newCache $ \() -> do
    let templateP = "templates/*.mustache"
    getDirFiles (Pat templateP) >>= need
    liftIO (compileMustacheDir "default" (takeDirectory templateP))

  unPat (outPattern @'CssR) %> \out ->
    copyFile' (unTagged (mapIn @'CssR) out) out

  unPat (outPattern @'JsR) %> \out ->
    copyFile' (unTagged (mapIn @'JsR) out) out

  unPat (outPattern @'ImgR) %> \out ->
    copyFile' (unTagged (mapIn @'ImgR) out) out

  unPat (outPattern @'RawR) %> \out ->
    copyFile' (unTagged (mapIn @'RawR) out) out

  unPat (outPattern @'AttachmentR) %> \out ->
    copyFile' (unTagged (mapIn @'AttachmentR) out) out

  unPat (outPattern @'PostR) %> \out -> do
    env <- commonEnv ()
    ts  <- templates ()
    let src = unTagged (mapIn @'PostR) out
    need [src]
    (v, content) <- getPost src
    renderAndWrite ts ["post","default"] (Just content)
      [menuItem Posts env, v, mkLocation out]
      out

  cmnOut postsFile %> \out -> do
    env <- commonEnv ()
    ts  <- templates ()
    ps  <- gatherPostInfo @'PostR Proxy (Down . postPublished)
    renderAndWrite ts ["posts","default"] Nothing
      [menuItem Posts env, provideAs "post" ps, mkTitle Posts]
      out

  cmnOut atomFile %> \out -> do
    env <- commonEnv ()
    ts  <- templates ()
    ps  <- gatherPostInfo @'PostR Proxy (Down . postPublished)
    let feedUpdated = renderIso8601 $ maximum (normalizedUpdated <$> ps)
    renderAndWrite ts ["atom-feed"] Nothing
      [ env
      , provideAs "entry" ps
      , provideAs "feed_file" (dropDirectory1 out)
      , provideAs "feed_updated" feedUpdated ]
      out

  unPat (outPattern @'MTutorialR) %> \out -> do
    env <- commonEnv ()
    ts  <- templates ()
    let src = unTagged (mapIn @'MTutorialR) out
    need [src]
    (v, content) <- getPost src
    renderAndWrite ts ["post","default"] (Just content)
      [menuItem LearnHaskell env, v, mkLocation out]
      out

  unPat (outPattern @'TutorialR) %> \out -> do
    env <- commonEnv ()
    ts  <- templates ()
    let src = unTagged (mapIn @'TutorialR) out
    need [src]
    (v, content) <- getPost src
    renderAndWrite ts ["post","default"] (Just content)
      [menuItem LearnHaskell env, v, mkLocation out]
      out

  unPat (outPattern @'ResumeHtmlR) %> \out -> do
    env <- commonEnv ()
    ts  <- templates ()
    let src = unTagged (mapIn @'ResumeHtmlR) out
    need [src]
    (v, content) <- getPost src
    renderAndWrite ts ["post","default"] (Just content)
      [menuItem Resume env, v]
      out

  unPat (outPattern @'ResumePdfR) %> \out ->
    copyFile' (unTagged (mapIn @'ResumePdfR) out) out

  cmnOut learnFile %> \out -> do
    env <- commonEnv ()
    ts  <- templates ()
    mts <- gatherPostInfo @'MTutorialR Proxy postDifficulty
    its <- fmap InternalTutorial <$> gatherPostInfo @'TutorialR  Proxy (Down . postPublished)
    let ets = fromMaybe [] $ env
          ^? key "external_tutorials"
          . _Array
          . to (mapMaybe parseExternalTutorial . V.toList)
    renderAndWrite ts ["learn-haskell","default"] Nothing
      [ menuItem LearnHaskell env
      , provideAs "megaparsec_tutorials" mts
      , provideAs "generic_tutorials"
          (sortBy (comparing (Down . tutorialInfoPublished)) (its ++ ets))
      , mkTitle LearnHaskell ]
      out

  let justFromTemplate :: Either Text MenuItem -> PName -> FilePath -> Action ()
      justFromTemplate etitle template out = do
        env <- commonEnv ()
        ts  <- templates ()
        renderAndWrite ts [template,"default"] Nothing
          [ either (const env) (`menuItem` env) etitle
          , provideAs "title" (either id menuItemTitle etitle) ]
          out

  cmnOut ossFile      %> justFromTemplate (Right OSS)            "oss"
  cmnOut aboutFile    %> justFromTemplate (Right About)          "about"
  cmnOut notFoundFile %> justFromTemplate (Left "404 Not Found") "404"

----------------------------------------------------------------------------
-- Helpers

getDirFiles :: Pat r -> Action [FilePath]
getDirFiles = getDirectoryFiles "" . pure . unPat

selectTemplate :: PName -> Template -> Template
selectTemplate name t = t { templateActual = name }

renderAndWrite :: MonadIO m
  => Template          -- ^ Templates to use
  -> [PName]           -- ^ Names of templates, in order
  -> Maybe TL.Text     -- ^ First inner value to interpolate
  -> [Value]           -- ^ Rendering context
  -> FilePath          -- ^ File path where to write rendered file
  -> m ()
renderAndWrite ts pnames minner context out =
  liftIO . TL.writeFile out $
    foldl f (fromMaybe TL.empty minner) pnames
  where
    f inner pname = renderMustache
      (selectTemplate pname ts)
      (mkContext (provideAs "inner" inner : context))

menuItem :: MenuItem -> Value -> Value
menuItem item = over (key "main_menu" . _Array) . V.map $ \case
  Object m -> Object $
    if HM.lookup "title" m == (Just . String . menuItemTitle) item
      then HM.insert "active" (Bool True) m
      else m
  v -> v

getPost :: (MonadIO m, FromJSON v) => FilePath -> m (v, TL.Text)
getPost path = do
  (yaml', doc') <- T.breakOn "\n---\n" <$> liftIO (T.readFile path)
  yaml <-
    case Y.decodeEither' (TE.encodeUtf8 yaml') of
      Left err -> fail (Y.prettyPrintParseException err)
      Right value -> return value
  let r = writeHtml pandocWriterOpts . handleError $
        readMarkdown pandocReaderOpts (T.unpack (T.drop 5 doc'))
  return (yaml, renderHtml r)

pandocReaderOpts :: ReaderOptions
pandocReaderOpts = def
  { readerExtensions = S.union pandocExtensions $ S.fromList
    [ Ext_autolink_bare_uris
    , Ext_lists_without_preceding_blankline ]
  ,  readerSmart = True }

pandocWriterOpts :: WriterOptions
pandocWriterOpts = def
  { writerHtml5          = True
  , writerHighlight      = True
  , writerHTMLMathMethod = MathJax "" }

mkContext :: [Value] -> Value
mkContext = foldl1' f
  where
    f (Object m0) (Object m1) = Object (HM.union m0 m1)
    f _ _                     = error "context merge failed"

mkTitle :: MenuItem -> Value
mkTitle = provideAs "title" . menuItemTitle

mkLocation :: FilePath -> Value
mkLocation = provideAs "location" . dropDirectory1

provideAs :: ToJSON v => Text -> v -> Value
provideAs k v = Object (HM.singleton k (toJSON v))

gatherPostInfo :: forall r a. (Route r, Ord a)
  => Proxy r
  -> (PostInfo -> a)
  -> Action [PostInfo]
gatherPostInfo Proxy f = do
  ps' <- getDirFiles (pat @r)
  fmap (sortBy (comparing f)) . forM ps' $ \post -> do
    need [post]
    v <- fst <$> getPost post
    return v { postFile = dropDirectory1 (unTagged (mapOut @r) post) }

parseDay :: Monad m => Text -> m Day
parseDay = parseTimeM True defaultTimeLocale "%B %e, %Y" . T.unpack

normalizedUpdated :: PostInfo -> Day
normalizedUpdated PostInfo {..} = fromMaybe postPublished postUpdated

renderDay :: Day -> String
renderDay = formatTime defaultTimeLocale "%B %e, %Y"

renderIso8601 :: Day -> String
renderIso8601 = formatTime defaultTimeLocale fmt
  where
    fmt = iso8601DateFormat (Just "00:00:00Z")

parseExternalTutorial :: Value -> Maybe TutorialInfo
parseExternalTutorial o = do
  published <- (o ^? key "published" . _String) >>= parseDay
  title     <- o ^? key "title" . _String
  url       <- o ^? key "url" . _String
  return (ExternalTutorial published title url)

tutorialInfoPublished :: TutorialInfo -> Day
tutorialInfoPublished (InternalTutorial postInfo) = postPublished postInfo
tutorialInfoPublished (ExternalTutorial published _ _) = published
