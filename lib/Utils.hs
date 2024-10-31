module Utils
  ( getMatchingFiles,
    newCache',
    parseDay,
    renderDay,
    renderIso8601,
    parseTags,
  )
where

import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
import Data.Time.Format.ISO8601 qualified as ISO8601
import Development.Shake

getMatchingFiles :: FilePattern -> Action [FilePath]
getMatchingFiles = getDirectoryFiles "" . pure

newCache' :: (() -> Action v) -> Rules (Action v)
newCache' = fmap ($ ()) . newCache

parseDay :: (MonadFail m) => Text -> m Day
parseDay = parseTimeM True defaultTimeLocale "%B %e, %Y" . T.unpack

renderDay :: Day -> String
renderDay = formatTime defaultTimeLocale "%B %e, %Y"

renderIso8601 :: Day -> String
renderIso8601 day =
  ISO8601.formatShow
    (ISO8601.calendarFormat ISO8601.ExtendedFormat)
    day
    <> "T00:00:00Z"

parseTags :: Text -> Set Text
parseTags = Set.fromList . T.words . T.toLower
