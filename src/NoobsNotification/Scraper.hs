{-# LANGUAGE DuplicateRecordFields #-}

module NoobsNotification.Scraper (
  scrape
) where

import           Data.Text               (Text)
import qualified Data.Text               as T
import           NoobsNotification.Types
import           Text.HTML.Scalpel       (Scraper, attr, chroots, hasClass,
                                          scrapeURL, text, (//), (@:))

scrape :: Text -> IO (Maybe [Image])
scrape url = scrapeURL (T.unpack url) images

images :: Scraper Text [Image]
images = chroots ("div" @: [hasClass "image-info"]) noobsImage

noobsImage :: Scraper Text Image
noobsImage =
  Image
    <$> text "h3"
    <*> imageVersion
    <*> NoobsNotification.Scraper.torrentDownload

imageVersion :: Scraper Text Text
imageVersion =
  text $ "div" @: [hasClass "image-details"] // "strong"

torrentDownload :: Scraper Text Text
torrentDownload =
  attr "href" $ "a" @: [hasClass "dl-torrent"]
