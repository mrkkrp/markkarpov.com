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

postOut, cssOut, jsOut, imgOut :: FilePath -> FilePath
postOut x = outdir </> x -<.> "html"
cssOut  x = outdir </> x
jsOut   x = outdir </> x
imgOut  x = outdir </> x

postIn, cssIn, jsIn, imgIn :: FilePath -> FilePath
postIn x = dropDirectory1 x -<.> "md"
cssIn    = dropDirectory1
jsIn     = dropDirectory1
imgIn    = dropDirectory1

defaultT, postT :: PName
defaultT = "default"
postT    = "post"

----------------------------------------------------------------------------
-- Build system

main :: IO ()
main = shakeArgs shakeOptions $ do

  action $ do
    getDirFiles postsPattern >>= need . fmap postOut
    getDirFiles cssPattern   >>= need . fmap cssOut
    getDirFiles jsPattern    >>= need . fmap jsOut
    getDirFiles imgPattern   >>= need . fmap imgOut

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

  cssOut cssPattern %> \out ->
    copyFile' (cssIn out) out

  jsOut jsPattern %> \out ->
    copyFile' (jsIn out) out

  imgOut imgPattern %> \out ->
    copyFile' (imgIn out) out

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
