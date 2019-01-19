module NoobsNotification.Scraper (
  scrape
) where

import           Data.Text         (Text)
import qualified Data.Text         as T
import           Text.HTML.Scalpel (Scraper, attr, chroot, chroots, hasClass,
                                    scrapeURL, text, (//), (@:))

type TorrentDownload = Text
type Version         = Text
type Name            = Text

data Image = Image Name Version TorrentDownload deriving (Show)

scrape :: Text -> IO (Maybe [Image])
scrape url = scrapeURL (T.unpack url) images

images :: Scraper Text [Image]
images = chroots ("div" @: [hasClass "image-info"]) noobsImage

noobsImage :: Scraper Text Image
noobsImage = do
  name <- text $ "h3"
  version <- imageVersion
  url <- torrentDownload
  return $ Image name version url

imageVersion :: Scraper Text Version
imageVersion = do
  version <- text $ "div" @: [hasClass "image-details"] // "strong"
  return version

torrentDownload :: Scraper Text TorrentDownload
torrentDownload = do
  url <- attr "href" $ "a" @: [hasClass "dl-torrent"]
  return url
