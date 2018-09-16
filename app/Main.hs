{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE QuasiQuotes         #-}
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
import Data.List (sortOn, foldl1')
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (Down (..))
import Data.Proxy
import Data.Tagged
import Data.Text (Text)
import Data.Time
import Development.Shake
import Development.Shake.FilePath
import Text.Mustache
import Text.URI (URI)
import Text.URI.Lens (uriPath)
import Text.URI.QQ (scheme)
import qualified Data.HashMap.Strict  as HM
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import qualified Data.Text.Lazy       as TL
import qualified Data.Text.Lazy.IO    as TL
import qualified Data.Vector          as V
import qualified Data.Yaml            as Y
import qualified Lucid                as L
import qualified Text.MMark           as MMark
import qualified Text.MMark.Extension as Ext
import qualified Text.MMark.Extension.Common as Ext
import qualified Text.URI             as URI

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
  | AboutR

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

instance Route 'AboutR where
  pat    = Pat "about.md"
  mapIn  = Tagged (\x -> dropDirectory1 x -<.> "md")
  mapOut = Tagged (\x -> outdir </> x -<.> "html")

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

data PostInfo
  = InternalPost LocalInfo     -- ^ 'LocalInfo' injection
  | ExternalPost Day Text Text -- ^ Published, title, URL
  deriving (Eq, Show)

instance ToJSON PostInfo where
  toJSON (InternalPost localInfo) = toJSON localInfo
  toJSON (ExternalPost published title url) = object
    [ "title"             .= title
    , "published"         .= renderDay published
    , "published_iso8601" .= renderIso8601 published
    , "updated"           .= renderDay published
    , "updated_iso8601"   .= renderIso8601 published
    , "desc"              .= ("" :: Text)
    , "url"               .= url ]

data LocalInfo = LocalInfo
  { localTitle      :: !Text
  , localPublished  :: !Day
  , localUpdated    :: !(Maybe Day)
  , localDesc       :: !Text
  , localDifficulty :: !(Maybe Int)
  , localFile       :: !FilePath
  } deriving (Eq, Show)

instance FromJSON LocalInfo where
  parseJSON = withObject "local post metadata" $ \o -> do
    localTitle     <- o .: "title"
    localPublished <- (o .: "date") >>= (.: "published") >>= parseDay
    localUpdated   <- (o .: "date") >>= (.:? "updated")  >>=
      maybe (pure Nothing) (fmap Just . parseDay)
    localDesc      <- o .: "desc"
    localDifficulty <- o .:? "difficulty"
    let localFile = ""
    return LocalInfo {..}

instance ToJSON LocalInfo where
  toJSON info@LocalInfo {..} = object
    [ "title"             .= localTitle
    , "published"         .= renderDay localPublished
    , "published_iso8601" .= renderIso8601 localPublished
    , "updated"           .= fmap renderDay localUpdated
    , "updated_iso8601"   .= renderIso8601 (localNormalizedUpdated info)
    , "desc"              .= localDesc
    , "file"              .= localFile ]

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

  commonEnv <- fmap ($ ()) . newCache $ \() -> do
    let commonEnvFile = "config/env.yaml"
    need [commonEnvFile]
    r <- liftIO (Y.decodeFileEither commonEnvFile)
    case r of
      Left  err   -> fail (Y.prettyPrintParseException err)
      Right value -> return value

  templates <- fmap ($ ()) . newCache $ \() -> do
    let templateP = "templates/*.mustache"
    getDirFiles (Pat templateP) >>= need
    liftIO (compileMustacheDir "default" (takeDirectory templateP))

  getPost <- newCache $ \path -> do
    env <- commonEnv
    getPostHelper env path

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
    env <- commonEnv
    ts  <- templates
    let src = unTagged (mapIn @'PostR) out
    need [src]
    (v, content) <- getPost src
    renderAndWrite ts ["post","default"] (Just content)
      [menuItem Posts env, v, mkLocation out]
      out

  let gatherLocalInfo :: forall r a. (Route r, Ord a)
        => Proxy r
        -> (LocalInfo -> a)
        -> Action [LocalInfo]
      gatherLocalInfo Proxy f = do
        ps' <- getDirFiles (pat @r)
        fmap (sortOn f) . forM ps' $ \post -> do
          need [post]
          v <- getPost post >>= interpretValue . fst
          return v { localFile = dropDirectory1 (unTagged (mapOut @r) post) }
      gatherPosts = do
        env <- commonEnv
        ips <- fmap InternalPost
          <$> gatherLocalInfo @'PostR Proxy (Down . localPublished)
        let ets = parseExternalPosts "external_posts" env
        return $
          sortOn (Down . postInfoPublished) (ips ++ ets)

  cmnOut postsFile %> \out -> do
    env <- commonEnv
    ts  <- templates
    ps  <- gatherPosts
    renderAndWrite ts ["posts","default"] Nothing
      [ menuItem Posts env
      , provideAs "post" ps
      , mkTitle Posts ]
      out

  cmnOut atomFile %> \out -> do
    env <- commonEnv
    ts  <- templates
    ps  <- gatherPosts
    let feedUpdated = renderIso8601 $ maximum (normalizedUpdated <$> ps)
    renderAndWrite ts ["atom-feed"] Nothing
      [ env
      , provideAs "entry" ps
      , provideAs "feed_file" (dropDirectory1 out)
      , provideAs "feed_updated" feedUpdated ]
      out

  unPat (outPattern @'MTutorialR) %> \out -> do
    env <- commonEnv
    ts  <- templates
    let src = unTagged (mapIn @'MTutorialR) out
    need [src]
    (v, content) <- getPost src
    renderAndWrite ts ["post","default"] (Just content)
      [menuItem LearnHaskell env, v, mkLocation out]
      out

  unPat (outPattern @'TutorialR) %> \out -> do
    env <- commonEnv
    ts  <- templates
    let src = unTagged (mapIn @'TutorialR) out
    need [src]
    (v, content) <- getPost src
    renderAndWrite ts ["post","default"] (Just content)
      [menuItem LearnHaskell env, v, mkLocation out]
      out

  unPat (outPattern @'ResumeHtmlR) %> \out -> do
    env <- commonEnv
    ts  <- templates
    let src = unTagged (mapIn @'ResumeHtmlR) out
    need [src]
    (v, content) <- getPost src
    renderAndWrite ts ["post","default"] (Just content)
      [menuItem Resume env, v]
      out

  unPat (outPattern @'ResumePdfR) %> \out ->
    copyFile' (unTagged (mapIn @'ResumePdfR) out) out

  cmnOut learnFile %> \out -> do
    env <- commonEnv
    ts  <- templates
    mts <- gatherLocalInfo @'MTutorialR Proxy localDifficulty
    its <- fmap InternalPost <$>
      gatherLocalInfo @'TutorialR  Proxy (Down . localPublished)
    let ets = parseExternalPosts "external_tutorials" env
    renderAndWrite ts ["learn-haskell","default"] Nothing
      [ menuItem LearnHaskell env
      , provideAs "megaparsec_tutorials" mts
      , provideAs "generic_tutorials"
          (sortOn (Down . postInfoPublished) (its ++ ets))
      , mkTitle LearnHaskell ]
      out

  unPat (outPattern @'AboutR) %> \out -> do
    env <- commonEnv
    ts  <- templates
    let src = unTagged (mapIn @'AboutR) out
    need [src]
    (v, content) <- getPost src
    renderAndWrite ts ["post", "default"] (Just content)
      [menuItem About env, v]
      out

  let justFromTemplate :: Either Text MenuItem -> PName -> FilePath -> Action ()
      justFromTemplate etitle template out = do
        env <- commonEnv
        ts  <- templates
        renderAndWrite ts [template,"default"] Nothing
          [ either (const env) (`menuItem` env) etitle
          , provideAs "title" (either id menuItemTitle etitle) ]
          out

  cmnOut ossFile      %> justFromTemplate (Right OSS)            "oss"
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

getPostHelper :: Value -> FilePath -> Action (Value, TL.Text)
getPostHelper env path = do
  txt <- liftIO (T.readFile path)
  case MMark.parse path txt of
    Left errs -> fail (MMark.parseErrorsPretty txt errs)
    Right doc -> do
      let toc = MMark.runScanner doc (Ext.tocScanner (\x -> x > 1 && x < 5))
          r   = MMark.useExtensions
            [ Ext.fontAwesome
            , Ext.footnotes
            , Ext.kbd
            , Ext.linkTarget
            , Ext.mathJax (Just '$')
            , Ext.obfuscateEmail "protected-email"
            , Ext.punctuationPrettifier
            , Ext.ghcSyntaxHighlighter
            , Ext.skylighting
            , Ext.toc "toc" toc
            , addTableClasses
            , addImageClasses
            , provideSocialUrls env
            ]
            doc
          v = fromMaybe (object []) (MMark.projectYaml doc)
      return (v, L.renderText (MMark.render r))

interpretValue :: FromJSON v => Value -> Action v
interpretValue v =
  case fromJSON v of
    Error str -> fail str
    Success a -> return a

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

parseDay :: Monad m => Text -> m Day
parseDay = parseTimeM True defaultTimeLocale "%B %e, %Y" . T.unpack

localNormalizedUpdated :: LocalInfo -> Day
localNormalizedUpdated LocalInfo {..} =
  fromMaybe localPublished localUpdated

normalizedUpdated :: PostInfo -> Day
normalizedUpdated = \case
  InternalPost localInfo -> localNormalizedUpdated localInfo
  ExternalPost published _ _  -> published

renderDay :: Day -> String
renderDay = formatTime defaultTimeLocale "%B %e, %Y"

renderIso8601 :: Day -> String
renderIso8601 = formatTime defaultTimeLocale fmt
  where
    fmt = iso8601DateFormat (Just "00:00:00Z")

parseExternalPosts :: Text -> Value -> [PostInfo]
parseExternalPosts k v = fromMaybe [] $
  v ^? key k . _Array . to (mapMaybe parseExternalPost . V.toList)

parseExternalPost :: Value -> Maybe PostInfo
parseExternalPost o = do
  published <- (o ^? key "published" . _String) >>= parseDay
  title     <- o ^? key "title" . _String
  url       <- o ^? key "url" . _String
  return (ExternalPost published title url)

postInfoPublished :: PostInfo -> Day
postInfoPublished = \case
 InternalPost localInfo     -> localPublished localInfo
 ExternalPost published _ _ -> published

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
    (Ext.Image inner src (Just "my_photo")) -> L.with
      (old $ Ext.Image inner src Nothing)
      [ L.class_  "float-right d-none d-md-block ml-3"
      , L.width_  "300"
      , L.height_ "375"
      ]
    i@Ext.Image {} -> L.with (old i) [L.class_ "img-fluid"]
    other -> old other

provideSocialUrls :: Value -> MMark.Extension
provideSocialUrls v = Ext.inlineTrans $ \case
  l@(Ext.Link inner uri mtitle) ->
    if URI.uriScheme uri == Just [scheme|social|]
      then case uri ^. uriPath of
             [x] ->
               case v ^? key "social" . key (URI.unRText x) . _String . getURI of
                 Nothing -> Ext.Plain "!lookup failed!"
                 Just t  ->
                   if Ext.asPlainText inner == "x"
                     then Ext.Link (Ext.Plain (URI.render t) :| []) t mtitle
                     else Ext.Link inner t mtitle
             _ -> l
      else l
  other -> other

getURI :: Traversal' Text URI
getURI f txt = maybe txt URI.render <$> traverse f (URI.mkURI txt :: Maybe URI)
