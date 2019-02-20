module NoobsNotification.Types (
  Image(..)
) where

import Data.Aeson   (FromJSON, ToJSON)
import Data.Text    (Text)
import GHC.Generics (Generic)

data Image = Image {
  name            :: Text,
  version         :: Text,
  torrentDownload :: Text
} deriving (Show, Generic)

instance FromJSON Image where
instance ToJSON Image where
