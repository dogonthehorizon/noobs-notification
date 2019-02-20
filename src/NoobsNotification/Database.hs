module NoobsNotification.Database (
  readImage,
  writeImage
) where

import Network.AWS.Types (Error)
import Control.Monad.Catch (MonadCatch, catch)
import Control.Lens.Basic       (view)
import Control.Monad            (void)
import Control.Monad.IO.Class   (MonadIO)
import Data.Aeson               (decode, encode)
import Data.Conduit.Binary      (sinkLbs)
import Data.Text                (Text)
import Network.AWS              (MonadAWS, send, sinkBody)
import Network.AWS.Data.Body    (toBody)
import Network.AWS.S3.GetObject (getObject, gorsBody)
import Network.AWS.S3.PutObject (putObject)
import Network.AWS.S3.Types     (BucketName (..), ObjectKey (..))
import NoobsNotification.Types

readImage :: (MonadCatch m, MonadIO m, MonadAWS m) => Text -> Text -> m (Maybe Image)
readImage bucketName name = do
  catch go' (\(_ :: Error) -> return Nothing)
  where go' = do
                val <- send $ getObject (BucketName bucketName) (ObjectKey name)
                decode <$> sinkBody (view gorsBody val) sinkLbs

writeImage :: MonadAWS m => Text -> Image -> m ()
writeImage bucketName image @ Image { name } =
  void . send $
  putObject (BucketName bucketName) (ObjectKey name)
    (toBody . encode $ image)
