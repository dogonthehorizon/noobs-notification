module NoobsNotification.Types where

import           Data.Text (Text)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data Image = Image {
  name            :: Text,
  version         :: Text,
  torrentDownload :: Text
} deriving (Show, Generic)

instance FromJSON Image where
instance ToJSON Image where
