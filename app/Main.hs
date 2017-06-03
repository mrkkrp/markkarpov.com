{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Default.Class
import Development.Shake
import Development.Shake.FilePath
import Text.Blaze.Html.Renderer.Text
import Text.Mustache
import Text.Pandoc hiding (Template)
import qualified Data.HashMap.Strict as HM
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

postsPattern, cssPattern, jsPattern, templPattern, imgPattern :: FilePattern
postsPattern = "post/*.md"
cssPattern   = "static/css/*.css"
jsPattern    = "static/js/*.js"
templPattern = "templates/*.mustache"
imgPattern   = "static/img/*"

aboutFile, ossFile, notFoundFile :: FilePath
aboutFile    = "about.html"
ossFile      = "oss.html"
notFoundFile = "404.html"

postOut, cmnOut :: FilePath -> FilePath
postOut x = outdir </> x -<.> "html"
cmnOut  x = outdir </> x

postIn, cmnIn :: FilePath -> FilePath
postIn x = dropDirectory1 x -<.> "md"
cmnIn    = dropDirectory1

aboutT, defaultT, postT, ossT, notFoundT :: PName
aboutT    = "about"
defaultT  = "default"
postT     = "post"
ossT      = "oss"
notFoundT = "404"

----------------------------------------------------------------------------
-- Build system

main :: IO ()
main = shakeArgs shakeOptions $ do

  action $ do
    getDirFiles postsPattern >>= need . fmap postOut
    getDirFiles cssPattern   >>= need . fmap cmnOut
    getDirFiles jsPattern    >>= need . fmap cmnOut
    getDirFiles imgPattern   >>= need . fmap cmnOut
    need (cmnOut <$> [aboutFile, ossFile, notFoundFile])

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
    getDirFiles templPattern >>= need
    liftIO (compileMustacheDir defaultT (takeDirectory templPattern))

  cmnOut cssPattern %> \out ->
    copyFile' (cmnIn out) out

  cmnOut jsPattern %> \out ->
    copyFile' (cmnIn out) out

  cmnOut imgPattern %> \out ->
    copyFile' (cmnIn out) out

  postOut postsPattern %> \out -> do
    env <- commonEnv ()
    ts  <- templates ()
    let src = postIn out
    need [src]
    (v, content) <- getPost src
    let post = renderMustache
          (selectTemplate postT ts)
          (mkContext env v content)
    liftIO . TL.writeFile out $ renderMustache
      (selectTemplate defaultT ts)
      (mkContext env v post)

  let justFromTemplate template out = do
        env <- commonEnv ()
        ts  <- templates ()
        let post = renderMustache
              (selectTemplate template ts)
              (mkContext env (Object HM.empty) "")
        liftIO . TL.writeFile out $ renderMustache
          (selectTemplate defaultT ts)
          (mkContext env (Object HM.empty) post)

  cmnOut aboutFile    %> justFromTemplate aboutT
  cmnOut ossFile      %> justFromTemplate ossT
  cmnOut notFoundFile %> justFromTemplate notFoundT

----------------------------------------------------------------------------
-- Helpers

getDirFiles :: FilePattern -> Action [FilePath]
getDirFiles = getDirectoryFiles "" . pure

selectTemplate :: PName -> Template -> Template
selectTemplate name t = t { templateActual = name }

pandocReaderOpts :: ReaderOptions
pandocReaderOpts = def
  { readerSmart = True }

pandocWriterOpts :: WriterOptions
pandocWriterOpts = def
  { writerHtml5          = True
  , writerHighlight      = True
  , writerHTMLMathMethod = MathJax "" }

getPost :: MonadIO m => FilePath -> m (Value, TL.Text)
getPost path = do
  (yaml', doc') <- T.breakOn "\n---\n" <$> liftIO (T.readFile path)
  yaml <-
    case Y.decodeEither' (TE.encodeUtf8 yaml') of
      Left err -> fail (Y.prettyPrintParseException err)
      Right value -> return value
  let r = writeHtml pandocWriterOpts . handleError $
        readMarkdown pandocReaderOpts (T.unpack (T.drop 5 doc'))
  return (yaml, renderHtml r)

mkContext :: Value -> Value -> TL.Text -> Value
mkContext (Object m0) (Object m1) content =
  let m = HM.union m0 m1
      r = String (TL.toStrict content)
  in Object (HM.insert "inner" r m)
mkContext _ _ _ = error "context merge failed"
