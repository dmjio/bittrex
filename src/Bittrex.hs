{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
module Bittrex
  ( -- * Public
    -- * Account
    -- * Market
  ) where

import           Control.Lens
import           Crypto.Hash.SHA512
import           Data.Aeson
import           Data.Aeson.Lens         (key, nth)
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as B
import qualified Data.ByteString.Char8   as BC
import qualified Data.ByteString.Lazy    as L
import           Data.Char
import           Data.List
import           Data.Monoid
import           Data.Proxy
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Data.Time
import           Data.Time.Clock.POSIX
import           Debug.Trace
import           GHC.Generics
import           Network.HTTP.Client.TLS
import           Network.Wreq

-- | We provide a simple RESTful API.
-- All calls are GETs and should be called via https.
-- We will support n-1 versions of the API.
-- Our current stable API is v1 and v1.1

-- | https://bittrex.com/api/{version}/{method}?param=value

-- https://bittrex.com/api/v1.1/public/getcurrencies

-- getWith opts "https://api.github.com/search/repositories"
type Params = [(T.Text,String)]

data CurrencyName
  = BTC|LTC|DOGE|VTC|PPC|FTC|RDD|NXT|DASH|POT|BLK|EMC2|XMY|AUR|UTC|MZC|EFL|GLD|FAIR|SLR|PTC|GRS|NLG|RBY|XWC|MONA|BITS|OC|THC|ENRG|SFR|ERC|NAUT|VRC|CURE|BLC|XC|XDQ|XBB|HYPER|CCN|XMR|CLOAK|BSD|CRYPT|START|KORE|XDN|TRK|TRUST|NAV|XST|APEX|BTCD|VIA|TRI|UNO|PINK|IOC|MAX|LXC|BOB|CANN|FC2|SSD|J|SYS|NEOS|DGB|ROOT|BTS|BURST|TIT|BSTY|PXI|DGC|SLG|STV|EXCL|SWIFT|NET|GHC|DOPE|BLOCK|ABY|VIOR|BYC|UFO|XMG|XQN|BLITZ|VPN|BAY|DTC|AM|METAL|SPR|VTR|XPY|XRP|GAME|GP|NXS|COVAL|FSC2|SOON|HZ|XCP|BITB|XTC|XVG|GEO|FLDC|GEMZ|GRC|XCO|MTR|FLO|U|NBT|XEM|MUE|XVC|A8BIT|CLAM|XSEED|NTRN|SLING|DMD|GAM|UNIT|GRT|VIRAL|SPHR|ARB|OK|ADC|SNRG|PKB|TES|CPC|AEON|BITZ|ETH|GCR|TX|BCY|PRIME|EXP|NEU|SWING|INFX|OMNI|USDT|AMP|AGRS|XLM|SPRTS|YBC|BTA|MEC|BITCNY|AMS|SCRT|SCOT|CLUB|VOX|MND|EMC|FCT|MAID|FRK|EGC|SLS|ORB|STEPS|RADS|DCR|SAFEX|PIVX|WARP|CRBIT|MEME|STEEM|A2GIVE|LSK|KR|PDC|DGD|BRK|WAVES|RISE|LBC|SBD|BRX|DRACO|ETC|UNIQ|STRAT|UNB|SYNX|TRIG|EBST|VRM|XAUR|SEQ|SNGLS|REP|SHIFT|ARDR|XZC|NEO|ZEC|ZCL|IOP|DAR|GOLOS|GBG|UBQ|HKG|KMD|SIB|ION|LMC|QWARK|CRW|SWT|TIME|MLN|TKS|ARK|DYN|MUSIC|DTB|INCNT|GBYTE|GNT|NXC|EDG|LGD|TRST|WINGS|RLC|GNO|GUP|LUN|APX|TKN|HMQ|ANT|ZEN|SC|BAT|A1ST|QRL|CRB|TROLL|PTOY|MYST|CFI|BNT|NMR|SNT|DCT|XEL|MCO|ADT|FUN|PAY|MTL|STORJ|ADX|OMG|CVC|PART|QTUM|BCC|DNT|ADA|MANA|SALT|TIX|RCN|VIB|MER|POWR|BTG|ENGs

data APIType
  = PublicAPI
  | AccountAPI
  | MarketAPI
  deriving (Eq)

instance Show APIType where
  show PublicAPI  = "public"
  show AccountAPI = "account"
  show MarketAPI  = "market"

defOpts :: APIOpts
defOpts = APIOpts PublicAPI [] "v1.1" mempty mempty mempty

data APIOpts
  = APIOpts
  { apiType :: APIType
  , qParams :: Params
  , version :: String
  , path :: String
  , apiKey :: ByteString
  , apiSecret :: ByteString
  } deriving (Show, Eq)

callAPI :: FromJSON v => APIOpts -> IO (Either ErrorMessage v)
callAPI APIOpts {..} = do
  nonce <- getPOSIXTime
  let addParam (k,v) o = o & param k .~ [T.pack $ camelToDash v]
      addAuth = apiType `elem` [AccountAPI, MarketAPI]
      opts = addHeader $ foldr addParam defaults (qParams ++ otherParams)
      otherParams = concat
        [ [ ("apikey", BC.unpack apiKey) | addAuth ]
        , [ ("nonce", show nonce)        | addAuth ]
        ]
      addHeader o =
          if addAuth
            then o & header "apisign" .~ [ hmac apiSecret urlForHash ]
            else o
      urlForHash = BC.pack $
        url <> mconcat [ "?apikey=" <> BC.unpack apiKey
                       , "&nonce=" <> show nonce
                       ]
      url = intercalate "/" [ "https://bittrex.com/api"
                            , version
                            , map toLower (show apiType)
                            , path
                            ]
  r <- getWith opts url
  let Just success = r ^? responseBody . key "success"
      Just result  = r ^? responseBody . key "result"
      Just msg     = r ^? responseBody . key "message"
  pure $ if success == Bool True
           then case fromJSON result of
                  Error s  -> Left (DecodeFailure s result)
                  Success m -> Right m
           else case fromJSON msg of
                  Error s -> Left (OtherError msg)
                  Success s -> Left s
     where
       camelToDash :: String -> String
       camelToDash [] = []
       camelToDash ('_':xs) = '-':camelToDash xs
       camelToDash (x:xs) = toLower x:camelToDash xs

data ErrorMessage
  = INVALID_MARKET
  | MARKET_NOT_PROVIDED
  | APIKEY_NOT_PROVIDED
  | OtherError Value
  | DecodeFailure String Value
  deriving (Show, Eq, Generic)

instance FromJSON ErrorMessage

data MarketName
  = BTC_ADA
  deriving (Show)

data Ticker = Ticker
  { bid :: Double
  , ask :: Double
  , last :: Double
  } deriving (Generic, Show)

instance FromJSON Ticker where
  parseJSON = withObject "Ticker" $ \o ->
    Ticker <$> o .: "Bid"
           <*> o .: "Ask"
           <*> o .: "Last"

-- | Public - Public information available without an API key

-- /public/getmarkets
-- https://bittrex.com/api/v1.1/public/getmarkets
-- Used to get the open and available trading markets at Bittrex along with other meta data.
getMarkets :: IO (Either ErrorMessage [Market])
getMarkets = callAPI defOpts { path = "getmarkets" }

data Market
  = Market
  { marketCurrency :: Text
  , baseCurrency :: Text
  , marketCurrencyLong :: Text
  , baseCurrencyLong :: Text
  , minTradeSize :: Double
  , marketName :: Text
  , isActive :: Bool
  , created :: Text
  } deriving (Show, Eq)

instance FromJSON Market where
  parseJSON = withObject "Market" $ \o ->
    Market <$> o .: "MarketCurrency"
           <*> o .: "BaseCurrency"
           <*> o .: "MarketCurrencyLong"
           <*> o .: "BaseCurrencyLong"
           <*> o .: "MinTradeSize"
           <*> o .: "MarketName"
           <*> o .: "IsActive"
           <*> o .: "Created"

-- /public/getcurrencies
-- Used to get all supported currencies at Bittrex along with other meta data.
-- https://bittrex.com/api/v1.1/public/getticker
getCurrencies :: IO (Either ErrorMessage [Currency])
getCurrencies = callAPI defOpts { path = "getcurrencies" }

data Currency
  = Currency
  { currency :: Text
  , currencyLong :: Text
  , minConfirmation :: Int
  , txFee :: Double
  , currencyIsActive :: Bool
  , coinType :: Text
  , baseAddress :: Maybe Text
  } deriving (Show, Eq)

instance FromJSON Currency where
  parseJSON = withObject "Currency" $ \o ->
    Currency
      <$> o .: "Currency"
      <*> o .: "CurrencyLong"
      <*> o .: "MinConfirmation"
      <*> o .: "TxFee"
      <*> o .: "IsActive"
      <*> o .: "CoinType"
      <*> o .: "BaseAddress"

-- /public/getticker
-- Used to get the current tick values for a market.
-- https://bittrex.com/api/v1.1/public/getticker
getTicker :: MarketName -> IO (Either ErrorMessage Ticker)
getTicker market =
  callAPI defOpts {
      qParams = [("market", show market)]
    , path = "getticker"
    }

-- /public/getmarketsummaries
-- Used to get the last 24 hour summary of all active exchanges
-- https://bittrex.com/api/v1.1/public/getmarketsummaries
getMarketSummaries :: IO (Either ErrorMessage Value)
getMarketSummaries = callAPI defOpts {  path = "getmarketsummaries" }


-- /public/getmarketsummary
-- Used to get the last 24 hour summary of all active exchanges
-- https://bittrex.com/api/v1.1/public/getmarketsummary?market=btc-ltc
-- params::, a string literal for the market (ex: BTC-LTC)
getMarketSummary :: MarketName -> IO (Either ErrorMessage Value)
getMarketSummary market =
  callAPI defOpts {
      qParams = [("market", show market)]
    , path = "getmarketsummary"
    }

-- /public/getorderbook
-- Used to get retrieve the orderbook for a given market
-- Parameters
  -- market	required	a string literal for the market (ex: BTC-LTC)
  -- type	required        buy, sell or both to identify the type of orderbook to return.
getOrderBook :: MarketName -> OrderBookType -> IO (Either ErrorMessage OrderBook)
getOrderBook market orderBookType =
  callAPI defOpts {
      path = "getorderbook"
    , qParams = [ ("market", show market)
                , ("type", show orderBookType)
                ]
    }

data OrderBookType
  = Buy
  | Sell
  | Both
  deriving (Show, Eq)

data OrderBookEntry
  = OrderBookEntry
  { quantity :: Double
  , rate :: Double
  } deriving (Show, Eq)

instance FromJSON OrderBook where
  parseJSON = withObject "OrderBook" $ \o ->
    OrderBook <$> o .: "buy"
              <*> o .: "sell"

data OrderBook
  = OrderBook
  { buy :: [OrderBookEntry]
  , sell :: [OrderBookEntry]
  } deriving (Show, Eq)

instance FromJSON OrderBookEntry where
  parseJSON = withObject "OrderBookEntry" $ \o ->
    OrderBookEntry <$> o .: "Quantity"
                   <*> o .: "Rate"

k :: ByteString
k = "{\"buy\":[{\"Quantity\":12.37000000,\"Rate\":0.02525000}],\"sell\":[{\"Quantity\":32.55412402,\"Rate\":0.02540000}]}"

Right m = eitherDecode (L.fromStrict k) :: Either String OrderBook

-- /public/getmarkethistory
-- Used to retrieve the latest trades that have occured for a specific market.
--  market required  string literal for the market (ex: BTC-LTC)
-- https://bittrex.com/api/v1.1/public/getmarkethistory?market=BTC-DOGE
getMarketHistory :: MarketName -> IO (Either ErrorMessage [MarketHistory])
getMarketHistory market =
  callAPI defOpts {
      path = "getmarkethistory"
    , qParams = pure ("market", show market)
    }

data MarketHistory
  = MarketHistory
  { marketHistoryId :: Integer
  , marketHistoryTimeStamp :: Text
  , marketHistoryQuantity :: Double
  , marketHistoryPrice :: Double
  , marketHistoryTotal :: Double
  , marketHistoryFillType :: Text
  , marketHistoryOrderType :: Text
  } deriving (Show, Eq)

instance FromJSON MarketHistory where
  parseJSON = withObject "MarketHistory" $ \o ->
    MarketHistory <$> o .: "Id"
                  <*> o .: "TimeStamp"
                  <*> o .: "Quantity"
                  <*> o .: "Price"
                  <*> o .: "Total"
                  <*> o .: "FillType"
                  <*> o .: "OrderType"


-- data CoinType
--  = BITCOIN|NXT|BITCOINEX|NXT_MS|CRYPTO_NOTE_PAYMENTID|BITSHAREX|NXT_ASSET|COUNTERPARTY|BITCOIN_STEALTH|RIPPLE|NEM|ETH|OMNI|LUMEN|FACTOM|STEEM|BITCOIN_PERCENTAGE_FEE|LISK|ETH_CONTRACT|WAVES|ANTSHARES|WAVES_ASSET|BYTEBALL|SIA|IOTA|ADA

-- Market Apis


