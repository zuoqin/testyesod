module Handler.Story where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))
import Network.HTTP

import Data.Text (pack, concat, unpack)
import Data.List (foldl, findIndices)

import Data.List.Split as Split
import qualified Data.ByteString.Char8 as L8
import Data.ByteString.Base64

decodeURL :: String -> String
decodeURL theURL = do
    case decode (L8.pack theURL) of
        Left err -> err
        Right p -> (L8.unpack p)

doStuffWithNormalString :: String -> IO String
doStuffWithNormalString storyId = do
    let story = simpleHTTP (Network.HTTP.getRequest ("http://www.zerohedge.com" ++ (decodeURL storyId))) >>= fmap (take 100000) . getResponseBody
    story
--getzhpage :: IO String
--getzhpage = do
--    rsp <- Network.HTTP.simpleHTTP (Network.HTTP.getRequest "http://www.zerohedge.com/?page=1")
          -- fetch document and return it (as a 'String'.)
--    fmap (Data.List.take 100) (getResponseBody rsp)
    --res
indexAt :: Int -> [a] -> a
-- indexAt _ [] = error "Empty List!"
indexAt y (x:xs)  | y <= 0 = x
                 | otherwise = indexAt (y-1) xs


-- buildStories :: String -> [Story] -> [Story]
-- buildStories pagehtml stories = do
--     --let sections = Split.splitOn "views-row views-row-1 views-row-odd views-row-first" pagehtml

--     --let storieshtml = indexAt 2 sections
--     let bstr = L8.pack pagehtml
--     let ind1 = length (fst (L8.breakSubstring (L8.pack "<h2 class=\"title teaser-title\">") bstr))

--     let ind2 = (length (fst (L8.breakSubstring (L8.pack "a href=\"") (drop (ind1 + 10) bstr)))) + ind1 + 10

--     let ind3 = indexAt 0 (findIndices (`elem` ['>']) (drop ind2 (L8.unpack bstr)))

--     let reference = take (ind3 -7) (drop (ind2 + 7) bstr)


--     let ind1 = (length (fst (L8.breakSubstring (L8.pack "teaser-text") (drop (ind3 + ind2) bstr)))) + ind3 + ind2
--     let ind2 = length (fst (L8.breakSubstring (L8.pack "</span>") (drop ind1 bstr)))
--     let introduction = take (ind2 + 35) (drop (ind1 - 14) pagehtml)


--     let theStory = indexAt 1 (Split.splitOn "<h2 class=\"title teaser-title\">" pagehtml)

--     let pos1 = indexAt 0 (findIndices (`elem` ['>']) (drop 30 theStory))
--     let pos2 = indexAt 0 (findIndices (`elem` ['<']) (drop (pos1 + 31) theStory))

--     let theTitle = take pos2 (drop (pos1 + 31) theStory)

--     let newstories = stories ++ [(Story (L8.unpack reference) theTitle introduction)]

--     let newhtml = drop (ind1 + 50) pagehtml
--     let cnt = length newstories
--     if (cnt > 10) then newstories else buildStories newhtml newstories


getStoryR :: String -> Handler Html
getStoryR link = do
    aDomId <- newIdent

    --let stories = [(Story "khkhh" "yiuyiuyiu")]
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
    edata <- liftIO $ try $ (doStuffWithNormalString link)
    -- let edata = map (\x -> Data.Text.pack x) edata1
    -- edata <- liftIO $ try $ readFile "E:/DEV/Haskell/testyesod/README.md"
    case edata :: Either IOException String of
        Left e -> do
            $logError $ "Could not read datafile.txt"
            defaultLayout [whamlet|An error occurred|]
        Right str -> do
            $logInfo "Reading of data file succeeded"


            let sections = Split.splitOn "<div class=\"content\">" str

            let storyhtml = indexAt 2 sections
            let bstr = L8.pack storyhtml
            let ind1 = length (fst (L8.breakSubstring (L8.pack "<div class=\"node-links\">") bstr))

            let contents = take ind1 (drop 0 storyhtml)
            

            let sections = Split.splitOn "<main>" str

            let storyhtml = indexAt 1 sections
            let bstr = L8.pack storyhtml
            let ind1 = length (fst (L8.breakSubstring (L8.pack "<h1 class=\"title\">") bstr))
            let ind2 = length (fst (L8.breakSubstring (L8.pack "</h1>") (drop ind1 bstr)))

            let title = take ind2 (drop (ind1 + 18) storyhtml)



            --let story = Story (title contents)

            let ls = Import.lines (Data.Text.pack str)
            when (Import.length ls < 50) $ $logWarn "Less than 5 lines of data"

            
            defaultLayout $(widgetFile "storypage")
            --    [whamlet|
            --        <ol>
            --            $forall l <- ls
            --                <li>#{l}
            --    |]

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
