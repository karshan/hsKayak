{-# LANGUAGE OverloadedStrings #-}
import Control.Lens ((&), (.~), (^.))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT)
import qualified Control.Monad.Trans.State as S (get, put)
import qualified Data.ByteString.Char8 as BS (ByteString, pack)
import qualified Data.ByteString.Lazy.Char8 as LBS (ByteString, unpack)
import Data.Maybe (fromMaybe, mapMaybe, listToMaybe)
import Data.Monoid ((<>))
import Data.List (isPrefixOf)
import Network.Connection (TLSSettings(..))
import Network.HTTP.Client (CookieJar)
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.Wreq (Options, Response, postWith, getWith, proxy, manager, httpProxy, responseCookieJar, responseBody, cookies)
import qualified Network.Wreq as W (FormParam((:=)), defaults)
import Network.Wreq.Types (Postable)
import Text.XML.HXT.Core
import Data.Tree.NTree.TypeDefs (NTree)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Text.XML.Light

(^=) :: BS.ByteString -> BS.ByteString -> W.FormParam
(^=) = (W.:=)

defaults :: Options
defaults = W.defaults & proxy .~ httpProxy "localhost" 8080 & manager .~ Left (mkManagerSettings (TLSSettingsSimple True False False) Nothing)

post :: (Postable a) => String -> a -> IO LBS.ByteString
post url params = (^. responseBody) `fmap` postWith defaults url params

get :: String -> IO LBS.ByteString
get url = (^. responseBody) `fmap` getWith defaults url

css :: ArrowXml a => String -> a (NTree XNode) XmlTree
css tag = multi (hasName tag)

getPrice :: String -> String -> String -> String -> IO (Maybe [String])
getPrice orig dest dep ret = do
    originsid <- getOriginsId `fmap` get ("http://www.kayak.com/flights/" ++ orig ++ "-" ++ dest ++ "/" ++ dep ++ "/" ++ ret)
    now <- round `fmap` getPOSIXTime :: IO Int
    results <- post ("http://www.kayak.com/s/jspoll?ss=1&poll=1&final=false&updateStamp=" ++ show now) [ "lmname" ^= ""
                                                                                                       , "lmname2" ^= ""
                                                                                                       , "c" ^= "15" -- This choses how many results
                                                                                                       , "s" ^= "price"
                                                                                                       , "searchid" ^= originsid
                                                                                                       , "itd" ^= ""
                                                                                                       , "poll" ^= "1"
                                                                                                       , "seo" ^= "false"
                                                                                                       , "vw" ^= "list"
                                                                                                       , "urlViewState" ^= ""
                                                                                                       , "streaming" ^= "false"
                                                                                                       ]
    return $ getIteneraries results

safeHead = listToMaybe

contentDiv :: [Content] -> Maybe Element
contentDiv = safeHead . mapMaybe (filterElement (myhasAttr' "id" (=="content_div"))) . onlyElems

resultRowToLegdatas :: Element -> [String]
resultRowToLegdatas e = map mygetText $ filterElements (myhasAttr' "id" ("legdata" `isPrefixOf`)) e

mygetText :: Element -> String
mygetText e = fromMaybe "mygetText Failed" (do
    c <- safeHead $ elContent e
    d <- safeHead $ onlyText $ return c
    return $ cdData d)

resultRowToPrice :: Element -> String
resultRowToPrice = maybe "resultRowToPrice Failed" mygetText . safeHead . filterElements (myhasAttr' "class" (=="results_price bookitprice"))

toq :: String -> Text.XML.Light.QName
toq s = QName s Nothing Nothing

myhasAttr' :: String -> (String -> Bool) -> Element -> Bool
myhasAttr' k f e = Just True == f `fmap` lookup (toq k) (map (attrKey &&& attrVal) $ elAttribs e)

getIteneraries :: LBS.ByteString -> Maybe [String]
getIteneraries = f . LBS.unpack
    where
        f x = do
            c <- contentDiv $ parseXML x
            return $ map (show . (resultRowToPrice &&& resultRowToLegdatas)) $ elChildren c

getOriginsId :: LBS.ByteString -> BS.ByteString
getOriginsId = BS.pack . f . LBS.unpack
    where
        f = head . runLA (hread >>> css "input" >>> hasAttrValue "name" (== "originsid") >>> getAttrValue "value")

main :: IO ()
main = do
    iteneraries <- getPrice "IAD" "YYZ" "2014-08-29" "2014-09-01" 
    putStr $ maybe "getPrice failed" unlines iteneraries
