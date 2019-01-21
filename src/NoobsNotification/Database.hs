module NoobsNotification.Database (
  getImageFile,
  writeImageFile
) where

import           Data.Text               (Text)
import           NoobsNotification.Types

import Network.AWS.S3.PutObject (putObject)

getImageFile :: Monad m => Text -> m (Either Text Image)
getImageFile _ = return . Right $
  Image {
    name = "test",
    version = "3.0.1",
    torrentDownload = "bar"
  }

-- TODO don't specialize to IO
writeImageFile :: Image -> IO (Either Text ())
writeImageFile image @ Image { name } = do
  putStrLn $ "wrote: " ++ show image
  return (Right ())
