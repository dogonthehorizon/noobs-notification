module Main where

import NoobsNotification.Scraper (scrape)

main :: IO ()
main = do
  res <- scrape "https://www.raspberrypi.org/downloads/noobs/"
  putStrLn $ show res
