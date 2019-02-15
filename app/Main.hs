module Main where

import AWS.Lambda.Context         (HasLambdaContext (withContext))
import AWS.Lambda.Runtime         (mRuntimeWithContext)
import Control.Monad.Catch        (MonadCatch, MonadThrow)
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Control.Monad.Reader       (local, MonadReader, ReaderT, asks, runReaderT)
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
import Katip (Verbosity(V2), Severity(InfoS), ColorStrategy(ColorIfTerminal), KatipContext, Katip, Namespace(..), LogContexts, LogEnv)
import qualified Katip as K
import Control.Monad.Catch (bracket)
import System.IO (stdout)


data Environment = Environment {
  bucketName   :: Text
}

data Context = Context {
  environment  :: Environment,
  logNamespace :: Namespace,
  logContext   :: LogContexts,
  logEnv       :: LogEnv
}

instance FromEnv Environment where
  fromEnv = Environment <$> env "NOOBS_BUCKET_NAME"

-- Ignore the lambda context since we dont' actually need it.
-- Kind of a bummer that we have to define this; is there a better way?
instance HasLambdaContext Context where
  withContext _ e = e

newtype NoobsNotification a = NoobsNotification {
  runNoobsNotification :: ReaderT Context AWS a
} deriving (Applicative, Functor, Monad, MonadIO, MonadReader Context,
            MonadCatch, MonadThrow, MonadAWS)

instance Katip NoobsNotification where
  getLogEnv = asks logEnv
  localLogEnv f = local (\s -> s { logEnv = f (logEnv s)})

instance KatipContext NoobsNotification where
  getKatipContext = asks logContext
  localKatipContext f = local (\s -> s { logContext = f (logContext s)})
  getKatipNamespace = asks logNamespace
  localKatipNamespace f = local (\s -> s { logNamespace = f (logNamespace s)})

isNewerImage :: Image -> NoobsNotification Bool
isNewerImage Image { name, version = currentVersion } = do
  Environment { bucketName } <- asks environment
  $(K.logTM) InfoS $ K.logStr $ "Reading " <> name <> " from " <> bucketName <> " from S3." 
  contents <- readImage bucketName name
  case contents of
    Nothing -> return False
    Just Image { version = previousVersion} -> do
      $(K.logTM) InfoS $ K.logStr $ "Found " <> previousVersion <> " for  " <> name
      return $ previousVersion > currentVersion

handler :: Value -> NoobsNotification Value
handler _ = do
  $(K.logTM) InfoS "Starting Noobs Notification"
  Environment { bucketName } <- asks environment
  scrapeResults <- liftIO $ scrape  "https://www.raspberrypi.org/downloads/noobs/"
  case scrapeResults of
    Nothing -> do
      $(K.logTM) InfoS "Got no results from RPI.org"
      fail ""
    Just images -> do
      _ <- for images $ \i -> do
        newerImage <- isNewerImage i
        if newerImage
           then do
             $(K.logTM) InfoS $ K.logStr $ "Writing " <> show i

             liftAWS $ writeImage bucketName i
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

      handleScribe <- K.mkHandleScribe ColorIfTerminal stdout InfoS V2
      let mkLogEnv = K.registerScribe "stdout" handleScribe K.defaultScribeSettings =<< K.initLogEnv "NoobsNotification" "production"
      bracket mkLogEnv K.closeScribes $ \le -> do
        let ctx = Context env mempty mempty le
        runResourceT . runAWST awsEnv $
          runReaderT (runNoobsNotification (mRuntimeWithContext handler)) ctx
