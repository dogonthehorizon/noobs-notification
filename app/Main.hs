module Main where

import qualified Data.Text.IO               as T
import           NoobsNotification.Database
import           NoobsNotification.Scraper  (scrape)
import           NoobsNotification.Types

isNewerImage :: Monad m => Image -> m Bool
isNewerImage (Image name currentVersion _) = do
  contents <- getImageFile name
  case contents of
    Left _ -> fail "something went wrong"
    Right (Image _ previousVersion _) ->
      return $ previousVersion > currentVersion

main :: IO ()
main = do
  res <- scrape "https://www.raspberrypi.org/downloads/noobs/"
  case res of
    Nothing -> return ()
    Just images -> do
      r <- (flip traverse) images $ \i -> do
        newerImage <- isNewerImage i
        if newerImage
           then putStrLn $ show i
           else do
             writeResult <- writeImageFile i
             case writeResult of
                  Left err -> do
                    T.putStrLn err
                    return ()
                  Right _ ->
                    return ()
      return $ foldl mappend mempty r
