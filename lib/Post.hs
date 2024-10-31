{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Post
  ( Post (..),
    published,
    normalizedUpdated,
    tags,
    filterByTag,
    LocalPost,
  )
where

import Data.Aeson
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time
import Route qualified
import Utils (parseDay, parseTags, renderDay, renderIso8601)

-- | Information about post.
data Post
  = -- | 'LocalPost' injection
    InternalPost (Route.WithPage LocalPost)
  | -- | Published, title, URL
    ExternalPost Day Text Text

-- | The instance is only used for atom feed.
instance ToJSON Post where
  toJSON (InternalPost local) = toJSON local
  toJSON (ExternalPost published' title url) =
    object
      [ "title" .= title,
        "published" .= renderDay published',
        "published_iso8601" .= renderIso8601 published',
        "updated" .= renderDay published',
        "updated_iso8601" .= renderIso8601 published',
        "desc" .= ("" :: Text),
        "url" .= url
      ]

-- | Return the 'Day' on which the post was published.
published :: Post -> Day
published = \case
  InternalPost local -> localPublished (Route.unWithPage local)
  ExternalPost published' _ _ -> published'

-- | Return the 'Day' on which the past was updated.
normalizedUpdated :: Post -> Day
normalizedUpdated = \case
  InternalPost local -> localNormalizedUpdated (Route.unWithPage local)
  ExternalPost published' _ _ -> published'

-- | Get all tags from a collection of 'Post's.
tags :: [Post] -> Set Text
tags = foldl' f Set.empty
  where
    f s = \case
      InternalPost local -> Set.union s (localTags (Route.unWithPage local))
      ExternalPost {} -> s

-- | Filter a collection of 'Post's by a tag.
filterByTag :: Text -> [Post] -> [Post]
filterByTag tag = filter f
  where
    f = \case
      InternalPost local -> tag `elem` localTags (Route.unWithPage local)
      ExternalPost {} -> False

-- | Information about a post that is hosted on my site.
data LocalPost = LocalPost
  { localTitle :: !Text,
    localPublished :: !Day,
    localUpdated :: !(Maybe Day),
    localDesc :: !Text,
    localFile :: !FilePath,
    localTags :: Set Text
  }
  deriving (Eq, Show)

instance FromJSON LocalPost where
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
    return LocalPost {..}

instance ToJSON LocalPost where
  toJSON localPost@LocalPost {..} =
    object
      [ "title" .= localTitle,
        "published" .= renderDay localPublished,
        "published_iso8601" .= renderIso8601 localPublished,
        "updated" .= fmap renderDay localUpdated,
        "updated_iso8601" .= renderIso8601 (localNormalizedUpdated localPost),
        "desc" .= localDesc,
        "file" .= ("/" ++ localFile)
      ]

-- | Return the 'Day' on which the local post was updated, or if it wasn't
-- updated yet, return its creation date.
localNormalizedUpdated :: LocalPost -> Day
localNormalizedUpdated LocalPost {..} =
  fromMaybe localPublished localUpdated
