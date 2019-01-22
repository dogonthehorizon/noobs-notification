module Main where

import AWS.Lambda.Context         (HasLambdaContext (withContext))
import AWS.Lambda.Runtime         (mRuntimeWithContext)
import Control.Monad.Catch        (MonadCatch, MonadThrow)
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Control.Monad.Reader       (MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.Trans.AWS    (runAWST)
import Data.Aeson                 (Value (..))
import Data.Text                  (Text)
import Data.Traversable           (for)
import Network.AWS                (AWS, Credentials (Discover), MonadAWS,
                                   liftAWS, newEnv, runResourceT)
import NoobsNotification.Database (readImage, writeImage)
import NoobsNotification.Scraper  (scrape)
import NoobsNotification.Types    (Image (..))
import System.Envy                (FromEnv (fromEnv), decodeEnv, env)


data Environment = Environment {
  bucketName :: Text
} deriving (Show)

instance FromEnv Environment where
  fromEnv = Environment <$> env "NOOBS_BUCKET_NAME"

-- Ignore the lambda context since we dont' actually need it.
-- Kind of a bummer that we have to define this; is there a better way?
instance HasLambdaContext Environment where
  withContext _ e = e

newtype NoobsNotification a = NoobsNotification {
  runNoobsNotification :: ReaderT Environment AWS a
} deriving (Applicative, Functor, Monad, MonadIO, MonadReader Environment,
            MonadCatch, MonadThrow, MonadAWS)

isNewerImage :: Image -> NoobsNotification Bool
isNewerImage Image { name, version = currentVersion } = do
  bucketName <- asks bucketName
  contents <- readImage bucketName name
  case contents of
    Nothing -> return False
    Just Image { version = previousVersion} ->
      return $ previousVersion > currentVersion

handler :: Value -> NoobsNotification Value
handler _ = do
  bucket <- asks bucketName
  scrapeResults <- liftIO $ scrape  "https://www.raspberrypi.org/downloads/noobs/"
  case scrapeResults of
    Nothing -> fail "We couldn't get results from the rpi site"
    Just images -> do
      _ <- for images $ \i -> do
        newerImage <- isNewerImage i
        if newerImage
           then do
             liftAWS $ writeImage bucket i
             liftIO $ print i -- TODO notify us of a newer version
             return Null
           else return Null
      return Null

main :: IO ()
main = do
  result <- decodeEnv :: IO (Either String Environment)
  case result of
    Left err -> fail err
    Right env -> do
      awsEnv <- newEnv Discover
      runResourceT . runAWST awsEnv $
        runReaderT (runNoobsNotification (mRuntimeWithContext handler)) env
