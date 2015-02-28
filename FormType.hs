{-# LANGUAGE ExistentialQuantification #-}
module FormType where 

import Import
import Yesod.Form.Bootstrap3
import Yesod.Auth (requireAuth)
import Control.Monad
import Data.Default
import Text.Julius (rawJS)
import Data.Text (pack, unpack)

formLayout :: BootstrapFormLayout
formLayout = BootstrapHorizontalForm (ColSm 0) (ColSm 2) (ColSm 0) (ColSm 6)

formWrapper f = do
    (Entity _ User{userName=uname}) <- requireAuth
    time <- liftIO getCurrentTime
    return $ renderBootstrap3 formLayout ((void . runDB . insert) <$> (f uname time))

data FormDef = FormDef {
      formDefName :: Text
    , formDefMsg :: AppMessage
    , formDefForm :: Handler (Form (Handler ()))
    }



jqueryDayField :: (RenderMessage site FormMessage) => JqueryDaySettings -> Field (HandlerT site IO) Day
jqueryDayField = flip jqueryDayField' "date"

-- | Use jQuery's datepicker as the underlying implementation.
--
-- Since 1.4.3
jqueryDatePickerDayField :: (RenderMessage site FormMessage) => JqueryDaySettings -> Field (HandlerT site IO) Day
jqueryDatePickerDayField = flip jqueryDayField' "text"

jqueryDayField' :: (RenderMessage site FormMessage) => JqueryDaySettings -> Text -> Field (HandlerT site IO) Day
jqueryDayField' jds inputType = Field
    { fieldParse = parseHelper $ maybe
                  (Left MsgInvalidDay)
                  Right
              . readMay
              . unpack
    , fieldView = \theId name attrs val isReq -> do
        toWidget [shamlet|
$newline never
<input id="#{theId}" name="#{name}" *{attrs} type="#{inputType}" :isReq:required="" value="#{showVal val}">
|]
        toWidget [julius|
$(function(){
    var i = document.getElementById("#{rawJS theId}");
    if (i.type != "date") {
        $(i).datepicker({
            dateFormat:'yy-mm-dd',
            changeMonth:#{jsBool $ jdsChangeMonth jds},
            changeYear:#{jsBool $ jdsChangeYear jds},
            numberOfMonths:#{rawJS $ mos $ jdsNumberOfMonths jds},
            yearRange:#{toJSON $ jdsYearRange jds}
        });
    }
});
|]
    , fieldEnctype = UrlEncoded
    }
  where
    showVal = either id (pack . show)
    jsBool True = toJSON True
    jsBool False = toJSON False
    mos (Left i) = show i
    mos (Right (x, y)) = concat
        [ "["
        , show x
        , ","
        , show y
        , "]"
        ]

readMay :: Read a => String -> Maybe a
readMay s = case reads s of
                (x, _):_ -> Just x
                [] -> Nothing

data JqueryDaySettings = JqueryDaySettings
    { jdsChangeMonth :: Bool
    , jdsChangeYear :: Bool
    , jdsYearRange :: String
    , jdsNumberOfMonths :: Either Int (Int, Int)
    }

instance Default JqueryDaySettings where
    def = JqueryDaySettings
        { jdsChangeMonth = False
        , jdsChangeYear = False
        , jdsYearRange = "c-10:c+10"
        , jdsNumberOfMonths = Left 1
        }
