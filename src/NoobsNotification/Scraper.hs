module NoobsNotification.Scraper (
  scrape
) where

import           Data.Text               (Text)
import qualified Data.Text               as T
import           NoobsNotification.Types
import           Text.HTML.Scalpel       (Scraper, attr, chroot, chroots,
                                          hasClass, scrapeURL, text, (//), (@:))

scrape :: Text -> IO (Maybe [Image])
scrape url = scrapeURL (T.unpack url) images

images :: Scraper Text [Image]
images = chroots ("div" @: [hasClass "image-info"]) noobsImage

noobsImage :: Scraper Text Image
noobsImage = do
  name <- text "h3"
  version <- imageVersion
  url <- torrentDownload
  return $ Image name version url

imageVersion :: Scraper Text Version
imageVersion = do
  text $ "div" @: [hasClass "image-details"] // "strong"

torrentDownload :: Scraper Text TorrentDownload
torrentDownload = do
  attr "href" $ "a" @: [hasClass "dl-torrent"]
