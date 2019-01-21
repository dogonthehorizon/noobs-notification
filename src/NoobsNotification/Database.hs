module NoobsNotification.Database (
  getImageFile,
  writeImageFile
) where

import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import NoobsNotification.Types

getImageFile :: Monad m => Name -> m (Either Text Image)
getImageFile _ = return $ Right (Image "test" "3.0.1" "bar")

-- TODO don't specialize to IO
writeImageFile :: Image -> IO (Either Text ())
writeImageFile i = do
  putStrLn $ "wrote: " ++ show i
  return (Right ())
