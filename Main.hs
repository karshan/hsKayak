{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative ((<$>))
import Control.Lens ((&), (.~), (^.))
import qualified Data.ByteString.Char8 as BS (ByteString, pack)
import qualified Data.ByteString.Lazy.Char8 as LBS (ByteString, unpack)
import Data.Maybe (fromMaybe, mapMaybe, listToMaybe)
import Data.List (find, isPrefixOf)
import Network.Connection (TLSSettings(..))
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.Wreq (Options, postWith, getWith, proxy, manager, httpProxy, responseBody)
import qualified Network.Wreq as W (FormParam((:=)), defaults)
import Network.Wreq.Types (Postable)
import Data.Tree.NTree.TypeDefs (NTree)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Text.XML.Light
import Text.XML.HXT.Core

-- Utils
(^=) :: BS.ByteString -> BS.ByteString -> W.FormParam
(^=) = (W.:=)

defOpts :: Options
defOpts = W.defaults

proxyDefaults :: Options
proxyDefaults = W.defaults & proxy .~ httpProxy "localhost" 8080 & manager .~ Left (mkManagerSettings (TLSSettingsSimple True False False) Nothing)

post :: (Postable a) => String -> a -> IO LBS.ByteString
post url params = (^. responseBody) `fmap` postWith defOpts url params

get :: String -> IO LBS.ByteString
get url = (^. responseBody) `fmap` getWith defOpts url

css :: ArrowXml a => String -> a (NTree XNode) XmlTree
css tag = multi (hasName tag)

safeHead :: [a] -> Maybe a
safeHead = listToMaybe

toQName :: String -> Text.XML.Light.QName
toQName s = QName s Nothing Nothing

hasAttrVal :: String -> String -> Element -> Bool
hasAttrVal k v e = findAttr (toQName k) e == Just v

hasAttrValBy :: String -> (String -> Bool) -> Element -> Bool
hasAttrValBy k p e = maybe False p $ findAttr (toQName k) e
-- End Utils

data Itenerary = Itenerary { price :: String, legs :: [String] } deriving Show

resultRowToItenerary :: Element -> Maybe Itenerary
resultRowToItenerary r = do
    price' <- strContent <$> filterElement (hasAttrVal "class" "results_price bookitprice") r
    let legs' = map strContent $ filterElements (hasAttrValBy "id" ("legdata" `isPrefixOf`)) r
    return $ Itenerary price' legs'

getIteneraries :: String -> Maybe [Itenerary]
getIteneraries x = do
    c <- find (hasAttrVal "id" "content_div") $ onlyElems $ parseXML x
    let resultRows = elChildren c
    return $ mapMaybe resultRowToItenerary resultRows

getOriginsId :: String -> String
getOriginsId = fromMaybe (error "getOriginsId") . (=<<) (findAttr (toQName "value")) . safeHead . mapMaybe (filterElement (hasAttrVal "name" "originsid")) . onlyElems . parseXML

getPrice :: String -> String -> String -> String -> IO (Maybe [Itenerary])
getPrice orig dest dep ret = do
    originsid <- (BS.pack . getOriginsId . LBS.unpack) `fmap` get ("http://www.kayak.com/flights/" ++ orig ++ "-" ++ dest ++ "/" ++ dep ++ "/" ++ ret)
    now <- round `fmap` getPOSIXTime :: IO Int
    results <- post ("http://www.kayak.com/s/jspoll?ss=1&poll=1&final=false&updateStamp=" ++ show now)
                [ "lmname" ^= "", "lmname2" ^= "", "c" ^= "15", "s" ^= "price", "searchid" ^= originsid, "itd" ^= "", "poll" ^= "1", "seo" ^= "false", "vw" ^= "list", "urlViewState" ^= "", "streaming" ^= "false"]
    return $ getIteneraries $ LBS.unpack results

main :: IO ()
main = do
    iteneraries <- getPrice "SJC" "ORD" "2014-08-30" "2014-09-01" 
    print iteneraries
