module NoobsNotification.Database (
  readImage,
  writeImage
) where

import Control.Lens.Basic       (view)
import Control.Monad            (void)
import Control.Monad.Catch      (MonadCatch, catch)
import Data.Aeson               (decode, encode)
import Data.Conduit.Binary      (sinkLbs)
import Data.Text                (Text)
import Network.AWS              (MonadAWS, send, sinkBody)
import Network.AWS.Data.Body    (toBody)
import Network.AWS.S3.GetObject (getObject, gorsBody)
import Network.AWS.S3.PutObject (putObject)
import Network.AWS.S3.Types     (BucketName (..), ObjectKey (..))
import Network.AWS.Types        (Error)
import NoobsNotification.Types

readImage :: (MonadCatch m, MonadAWS m) => Text -> Text -> m (Maybe Image)
readImage bucketName name =
    catch go' (\(_ :: Error) -> return Nothing)
  where
    go' = do
        val <- send $ getObject (BucketName bucketName) (ObjectKey name)
        decode <$> sinkBody (view gorsBody val) sinkLbs

writeImage :: MonadAWS m => Text -> Image -> m ()
writeImage bucketName image@Image { name } = void . send $ putObject
    (BucketName bucketName)
    (ObjectKey name)
    (toBody . encode $ image)
