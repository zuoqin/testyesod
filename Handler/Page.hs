module Handler.Page where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))
--import Network.HTTP.Simple
--import Data.Text
--import Data.List
import qualified Data.ByteString.Char8 as L8

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.

--doStuffWithNormalString :: String -> Text
--doStuffWithNormalString s = Data.Text.pack " kkhkhkjj"


--getzhpage :: IO String
--getzhpage = do
--    rsp <- Network.HTTP.simpleHTTP (Network.HTTP.getRequest "http://www.zerohedge.com/?page=1")
          -- fetch document and return it (as a 'String'.)
--    fmap (Data.List.take 100) (getResponseBody rsp)
    --res

getPageR :: Handler Html
getPageR = do
    --datas <- liftIO getzhpage
    --let stories = (doStuffWithNormalString datas) :: Text

    --req' <- liftIO $ parseUrl "http://localhost:3000/"
    --let req = req' { method = "POST", requestBody = RequestBodyLBS valueBS }
    --let stories = httpLBS "http://www.zerohedge.com/?page=1"

    $logDebug "Trying to read data file"
    edata <- liftIO $ try $ readFile "E:/DEV/Haskell/testyesod/README.md"
    case edata :: Either IOException ByteString of
        Left e -> do
            $logError $ "Could not read datafile.txt"
            defaultLayout [whamlet|An error occurred|]
        Right str -> do
            $logInfo "Reading of data file succeeded"
            let ls = Import.lines (L8.unpack str)
            when (Import.length ls < 5) $ $logWarn "Less than 5 lines of data"
            defaultLayout
                [whamlet|
                    <ol>
                        $forall l <- ls
                            <li>#{l}
                |]

    -- let handlerName = "getPageR" :: Text    

    -- defaultLayout $ do
    --     aDomId <- newIdent
    --     setTitle "Page 1"
    --     $(widgetFile "storiespage")
