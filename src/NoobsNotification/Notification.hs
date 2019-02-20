module NoobsNotification.Notification (publishNotification) where

import Data.Aeson ((.=), object)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Network.AWS.SNS.Publish (publish)
import Network.AWS (MonadAWS, send)
import NoobsNotification.Types (Image(..))
import Text.Mustache (renderMustache, compileMustacheText)
import Control.Monad (void)

template :: Text
template = "A new version of {{ imageName }} has been released:\
           \ \n\t URL: {{ imageUrl }}"

publishNotification :: (MonadAWS m) => Image -> m ()
publishNotification Image { name, torrentDownload } = 
  case (compileMustacheText "tpl" template) of
    Right tpl -> do
      void . send . publish . toStrict $
        renderMustache tpl $ object [
          "imageName" .= name,
          "imageUrl"  .= torrentDownload
        ]
    Left _ -> fail "lol"
