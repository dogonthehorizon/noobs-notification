module NoobsNotification.Types where

import           Data.Text (Text)

type TorrentDownload = Text
type Version         = Text
type Name            = Text

data Image = Image Name Version TorrentDownload deriving (Show)

type File = Text
