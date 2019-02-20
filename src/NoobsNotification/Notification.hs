module NoobsNotification.Notification (publishNotification) where

import Control.Lens.Basic (set)
import Data.Aeson ((.=), object)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Network.AWS.SNS.Publish (publish, pTopicARN)
import Network.AWS (MonadAWS, send)
import NoobsNotification.Types (Image(..))
import Text.Mustache (renderMustache, compileMustacheText)
import Control.Monad (void)

template :: Text
template = "A new version of {{ imageName }} has been released:\
           \ \n\t URL: {{ imageUrl }}"

publishNotification :: (MonadAWS m) => Text -> Image -> m ()
publishNotification topicArn Image { name, torrentDownload } = 
  case (compileMustacheText "tpl" template) of
    Right tpl -> do
      void . send . set pTopicARN (Just topicArn) . publish . toStrict $
        renderMustache tpl $ object [
          "imageName" .= name,
          "imageUrl"  .= torrentDownload
        ]
    Left _ -> fail "lol"
