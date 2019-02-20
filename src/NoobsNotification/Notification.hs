module NoobsNotification.Notification (publishNotification) where

import Control.Lens.Basic      (set)
import Control.Monad           (void)
import Data.Aeson              (object, (.=))
import Data.Text               (Text)
import Data.Text.Lazy          (toStrict)
import Network.AWS             (MonadAWS, send)
import Network.AWS.SNS.Publish (pTopicARN, publish)
import NoobsNotification.Types (Image (..))
import Text.Mustache           (compileMustacheText, renderMustache)

template :: Text
template
    = "A new version of {{ imageName }} has been released:\
           \ \n\t URL: {{ imageUrl }}"

publishNotification :: (MonadAWS m) => Text -> Image -> m ()
publishNotification topicArn Image { name, torrentDownload } =
    case compileMustacheText "tpl" template of
        Right tpl ->
            void
            . send
            . set pTopicARN (Just topicArn)
            . publish
            . toStrict
            $ renderMustache tpl
            $ object ["imageName" .= name, "imageUrl" .= torrentDownload]
        Left _ -> fail "Unable to parse mustache template for notification"
