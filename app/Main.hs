{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Main (main) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Default.Class
import Data.List (sortBy, foldl1')
import Data.Maybe (fromMaybe)
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
-- Build system

main :: IO ()
main = shakeArgs shakeOptions $ do

  action $ do
    let r :: forall r. Route r => Proxy r -> Action ()
        r Proxy = getDirFiles (pat @r) >>= need . fmap (unTagged $ mapOut @r)
    r @'PostR Proxy
    r @'CssR Proxy
    r @'JsR Proxy
    r @'ImgR Proxy
    r @'RawR Proxy
    r @'AttachmentR Proxy
    r @'MTutorialR Proxy
    r @'ResumeHtmlR Proxy
    r @'ResumePdfR Proxy

    need $ cmnOut <$>
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
    let context =
          [ env
          , v
          , provideAs "location" (dropDirectory1 out) ]
        post = renderMustache
          (selectTemplate "post" ts)
          (mkContext (provideAs "inner" content : context))
    liftIO . TL.writeFile out $ renderMustache
      (selectTemplate "default" ts)
      (mkContext (provideAs "inner" post : context))

  cmnOut postsFile %> \out -> do
    env <- commonEnv ()
    ts  <- templates ()
    ps  <- gatherPostInfo @'PostR Proxy (Down . postPublished)
    liftIO . TL.writeFile out $ renderMustache
      (selectTemplate "default" ts)
      (mkContext [ env
                 , provideAs "title" ("Posts" :: Text)
                 , provideAs "inner"
                   (renderMustache
                     (selectTemplate "posts" ts)
                     (mkContext [env, provideAs "post" ps])) ] )

  cmnOut atomFile %> \out -> do
    env <- commonEnv ()
    ts  <- templates ()
    ps  <- gatherPostInfo @'PostR Proxy (Down . postPublished)
    let feedUpdated = renderIso8601 $ maximum (normalizedUpdated <$> ps)
    liftIO . TL.writeFile out $ renderMustache
      (selectTemplate "atom-feed" ts)
      (mkContext [ env
                 , provideAs "entry" ps
                 , provideAs "feed_file" (dropDirectory1 out)
                 , provideAs "feed_updated" feedUpdated])

  unPat (outPattern @'MTutorialR) %> \out -> do
    env <- commonEnv ()
    ts  <- templates ()
    let src = unTagged (mapIn @'MTutorialR) out
    need [src]
    (v, content) <- getPost src
    let context =
          [ env
          , v
          , provideAs "location" (dropDirectory1 out) ]
        tutorial = renderMustache
          (selectTemplate "post" ts)
          (mkContext (provideAs "inner" content : context))
    liftIO . TL.writeFile out $ renderMustache
      (selectTemplate "default" ts)
      (mkContext (provideAs "inner" tutorial : context))

  unPat (outPattern @'ResumeHtmlR) %> \out -> do
    env <- commonEnv ()
    ts  <- templates ()
    let src = unTagged (mapIn @'ResumeHtmlR) out
    need [src]
    (v, content) <- getPost src
    let context = [env, v]
        post = renderMustache
          (selectTemplate "post" ts)
          (mkContext (provideAs "inner"content : context))
    liftIO . TL.writeFile out $ renderMustache
      (selectTemplate "default" ts)
      (mkContext (provideAs "inner" post : context))

  unPat (outPattern @'ResumePdfR) %> \out ->
    copyFile' (unTagged (mapIn @'ResumePdfR) out) out

  cmnOut learnFile %> \out -> do
    env <- commonEnv ()
    ts  <- templates ()
    mts <- gatherPostInfo @'MTutorialR Proxy postDifficulty
    let post = renderMustache
          (selectTemplate "learn-haskell" ts)
          (mkContext [env, provideAs "megaparsec_tutorials" mts])
    liftIO . TL.writeFile out $ renderMustache
      (selectTemplate "default" ts)
      (mkContext [ env
                 , provideAs "inner" post
                 , provideAs "title" ("Learn Haskell" :: Text) ])

  let justFromTemplate :: Text -> PName -> FilePath -> Action ()
      justFromTemplate title template out = do
        env <- commonEnv ()
        ts  <- templates ()
        let post = renderMustache
              (selectTemplate template ts)
              (mkContext [env])
        liftIO . TL.writeFile out $ renderMustache
          (selectTemplate "default" ts)
          (mkContext [ env
                     , provideAs "inner" post
                     , provideAs "title" title ])

  cmnOut ossFile      %> justFromTemplate "Open Source"   "oss"
  cmnOut aboutFile    %> justFromTemplate "About me"      "about"
  cmnOut notFoundFile %> justFromTemplate "404 Not Found" "404"

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

----------------------------------------------------------------------------
-- Helpers

getDirFiles :: Pat r -> Action [FilePath]
getDirFiles = getDirectoryFiles "" . pure . unPat

selectTemplate :: PName -> Template -> Template
selectTemplate name t = t { templateActual = name }

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

mkContext :: [Value] -> Value
mkContext = foldl1' f
  where
    f (Object m0) (Object m1) = Object (HM.union m0 m1)
    f _ _                     = error "context merge failed"

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
