module Handler.Page where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))
import Network.HTTP

import Data.Text (pack, concat, unpack)
import Data.List (foldl)
import qualified Data.ByteString.Char8 as L8
--import Prelude

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.

doStuffWithNormalString :: IO String
doStuffWithNormalString = simpleHTTP (Network.HTTP.getRequest "http://www.zerohedge.com/?page=1") >>= fmap (take 10000) . getResponseBody


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

    --manager <- liftIO $ newManager defaultManagerSettings
    --req' <- liftIO $ parseRequest "http://www.zerohedge.com/?page=1"
    --let req = req' { method = "POST", requestBody = RequestBodyLBS valueBS }
    --let stories = httpLBS "http://www.zerohedge.com/?page=1"

    $logDebug "Trying to read data file"
    --edata <- liftIO $ try $ readFile "E:/DEV/Haskell/testyesod/README.md"
    --request <- parseRequest "http://httpbin.org/post"

    --Network.HTTP.Conduit.withManager $ \manager -> do
    --req' <- liftIO $ parseUrl "http://www.zerohedge.com/?page=1"
    --let req = req' { method = "GET" }
    --res <- http req  manager
    --resValue <- responseBody res
--theResponse <- liftIO $ try $ responseBody . httpLbs "http://www.zerohedge.com/?page=1"

    --let edata = liftIO $ try $ doStuffWithNormalString
    edata <- liftIO $ try $ doStuffWithNormalString
    -- let edata = map (\x -> Data.Text.pack x) edata1
    -- edata <- liftIO $ try $ readFile "E:/DEV/Haskell/testyesod/README.md"
    case edata :: Either IOException String of
        Left e -> do
            $logError $ "Could not read datafile.txt"
            defaultLayout [whamlet|An error occurred|]
        Right str -> do
            $logInfo "Reading of data file succeeded"
            let ls = Import.lines (Data.Text.pack str)
            when (Import.length ls < 50) $ $logWarn "Less than 5 lines of data"
            defaultLayout
                [whamlet|
                    <ol>
                        $forall l <- ls
                            <li>#{l}
                |]

    --let ls = Import.lines (Data.Text.unpack (Data.Text.concat edata))
    --when (Import.length ls < 5) $ $logWarn "Less than 5 lines of data"
    -- defaultLayout
    --     [whamlet|
    --         <ol>
    --             $forall l <- edata1
    --                 <li>#{l}
    --     |]
    -- let handlerName = "getPageR" :: Text    

    -- defaultLayout $ do
    --     aDomId <- newIdent
    --     setTitle "Page 1"
    --     $(widgetFile "storiespage")
