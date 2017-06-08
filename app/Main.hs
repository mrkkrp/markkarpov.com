{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main (main) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Default.Class
import Data.List (sortBy, foldl1')
import Data.Maybe (fromMaybe)
import Data.Ord (comparing, Down (..))
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

postsP, cssP, jsP, templateP, imgP, rawP, attachmentP, mtutorialP :: FilePattern
postsP      = "post/*.md"
cssP        = "static/css/*.css"
jsP         = "static/js/*.js"
templateP   = "templates/*.mustache"
imgP        = "static/img/*"
rawP        = "raw/*"
attachmentP = "attachment/*"
mtutorialP  = "megaparsec/*.md"

aboutFile, ossFile, notFoundFile, learnFile, postsFile, atomFile :: FilePath
aboutFile    = "about.html"
ossFile      = "oss.html"
notFoundFile = "404.html"
learnFile    = "learn-haskell.html"
postsFile    = "posts.html"
atomFile     = "feed.atom"

postOut, cmnOut, rawOut :: FilePath -> FilePath
postOut x = outdir </> x -<.> "html"
cmnOut  x = outdir </> x
rawOut  x = outdir </> dropDirectory1 x

postIn, cmnIn, rawIn :: FilePath -> FilePath
postIn x = dropDirectory1 x -<.> "md"
cmnIn    = dropDirectory1
rawIn  x = "raw" </> dropDirectory1 x

aboutT, defaultT, postT, ossT, notFoundT, learnT, postsT, atomT :: PName
aboutT    = "about"
defaultT  = "default"
postT     = "post"
ossT      = "oss"
notFoundT = "404"
learnT    = "learn-haskell"
postsT    = "posts"
atomT     = "atom-feed"

data PostInfo = PostInfo
  { postTitle     :: Text
  , postPublished :: Day
  , postUpdated   :: Maybe Day
  , postDesc      :: Text
  , postDifficulty :: Maybe Int
  , postFile      :: FilePath
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
-- Build system

main :: IO ()
main = shakeArgs shakeOptions $ do

  action $ do
    getDirFiles postsP      >>= need . fmap postOut
    getDirFiles cssP        >>= need . fmap cmnOut
    getDirFiles jsP         >>= need . fmap cmnOut
    getDirFiles imgP        >>= need . fmap cmnOut
    getDirFiles rawP        >>= need . fmap rawOut
    getDirFiles attachmentP >>= need . fmap cmnOut
    getDirFiles mtutorialP  >>= need . fmap postOut
    need (cmnOut <$>
          [aboutFile, ossFile, notFoundFile, learnFile, postsFile, atomFile])

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
    getDirFiles templateP >>= need
    liftIO (compileMustacheDir defaultT (takeDirectory templateP))

  cmnOut cssP %> \out ->
    copyFile' (cmnIn out) out

  cmnOut jsP %> \out ->
    copyFile' (cmnIn out) out

  cmnOut imgP %> \out ->
    copyFile' (cmnIn out) out

  rawOut rawP %> \out ->
    copyFile' (rawIn out) out

  cmnOut attachmentP %> \out ->
    copyFile' (cmnIn out) out

  postOut postsP %> \out -> do
    env <- commonEnv ()
    ts  <- templates ()
    let src = postIn out
    need [src]
    (v, content) <- getPost src
    let context =
          [ env
          , v
          , provideAs "location" (dropDirectory1 out)
          , provideAs "attachment" Null -- FIXME when Stache is fixed
          ]
        post = renderMustache
          (selectTemplate postT ts)
          (mkContext (provideAs "inner" content : context))
    liftIO . TL.writeFile out $ renderMustache
      (selectTemplate defaultT ts)
      (mkContext (provideAs "inner" post : context))

  cmnOut postsFile %> \out -> do
    env <- commonEnv ()
    ts  <- templates ()
    ps' <- getDirFiles postsP
    ps  <- fmap (sortBy (comparing (Down . postPublished))) . forM ps' $ \post -> do
      need [post]
      v <- fst <$> getPost post
      return v { postFile = dropDirectory1 (postOut post) }
    let postList = provideAs "post" ps
        posts = renderMustache
          (selectTemplate postsT ts)
          (mkContext [env, postList])
    liftIO . TL.writeFile out $ renderMustache
      (selectTemplate defaultT ts)
      (mkContext [ env
                 , postList
                 , provideAs "title" ("Posts" :: Text)
                 , provideAs "inner" posts ] )

  cmnOut atomFile %> \out -> do
    env <- commonEnv ()
    ts  <- templates ()
    ps' <- getDirFiles postsP
    ps  <- fmap (sortBy (comparing (Down . postPublished))) . forM ps' $ \post -> do
      need [post]
      v <- fst <$> getPost post
      return v { postFile = dropDirectory1 (postOut post) }
    let feedUpdated = renderIso8601 $ maximum (normalizedUpdated <$> ps)
    liftIO . TL.writeFile out $ renderMustache
      (selectTemplate atomT ts)
      (mkContext [ env
                 , provideAs "entry" ps
                 , provideAs "feed_file" (dropDirectory1 out)
                 , provideAs "feed_updated" feedUpdated])

  postOut mtutorialP %> \out -> do
    env <- commonEnv ()
    ts  <- templates ()
    let src = postIn out
    need [src]
    (v, content) <- getPost src
    let context =
          [ env
          , v
          , provideAs "location" (dropDirectory1 out)
          , provideAs "attachment" Null -- FIXME when Stache is fixed
          ]
        tutorial = renderMustache
          (selectTemplate postT ts)
          (mkContext (provideAs "inner" content : context))
    liftIO . TL.writeFile out $ renderMustache
      (selectTemplate defaultT ts)
      (mkContext (provideAs "inner" tutorial : context))

  cmnOut learnFile %> \out -> do
    env <- commonEnv ()
    ts  <- templates ()
    mt' <- getDirFiles mtutorialP
    mt  <- fmap (sortBy (comparing postDifficulty)) . forM mt' $ \post -> do
      need [post]
      v <- fst <$> getPost post
      return v { postFile = dropDirectory1 (postOut post) }
    let mtutorialList = provideAs "megaparsec_tutorials" mt
        post = renderMustache
          (selectTemplate learnT ts)
          (mkContext [env, mtutorialList])
    liftIO . TL.writeFile out $ renderMustache
      (selectTemplate defaultT ts)
      (mkContext [ env
                 , provideAs "inner" post
                 , mtutorialList
                 , provideAs "title" ("Learn Haskell" :: Text) ])

  let justFromTemplate :: Text -> PName -> FilePath -> Action ()
      justFromTemplate title template out = do
        env <- commonEnv ()
        ts  <- templates ()
        let post = renderMustache
              (selectTemplate template ts)
              (mkContext [env])
        liftIO . TL.writeFile out $ renderMustache
          (selectTemplate defaultT ts)
          (mkContext [ env
                     , provideAs "inner" post
                     , provideAs "title" title ])

  cmnOut ossFile      %> justFromTemplate "Open Source"   ossT
  cmnOut aboutFile    %> justFromTemplate "About me"      aboutT
  cmnOut notFoundFile %> justFromTemplate "404 Not Found" notFoundT

----------------------------------------------------------------------------
-- Helpers

getDirFiles :: FilePattern -> Action [FilePath]
getDirFiles = getDirectoryFiles "" . pure

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
