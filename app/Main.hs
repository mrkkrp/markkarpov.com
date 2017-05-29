module Main (main) where

import Data.Aeson
import Data.Yaml
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import Text.Mustache

main :: IO ()
main = shakeArgs shakeOptions { shakeFiles = "_build" } $ do
  return ()
