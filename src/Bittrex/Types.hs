{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
module Bittrex.Types where

import           Data.Aeson
import           Data.ByteString      (ByteString)
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as L
import           Data.Text            (Text)
import qualified Data.Text            as T
import           GHC.Generics

type Params = [(String,String)]
type Rate = Double
type UUID = String

data CurrencyName
  = BTC|LTC|DOGE|VTC|PPC|FTC|RDD|NXT|DASH|POT|BLK|EMC2|XMY|AUR|UTC|MZC|EFL|GLD|FAIR|SLR|PTC|GRS|NLG|RBY|XWC|MONA|BITS|OC|THC|ENRG|SFR|ERC|NAUT|VRC|CURE|BLC|XC|XDQ|XBB|HYPER|CCN|XMR|CLOAK|BSD|CRYPT|START|KORE|XDN|TRK|TRUST|NAV|XST|APEX|BTCD|VIA|TRI|UNO|PINK|IOC|MAX|LXC|BOB|CANN|FC2|SSD|J|SYS|NEOS|DGB|ROOT|BTS|BURST|TIT|BSTY|PXI|DGC|SLG|STV|EXCL|SWIFT|NET|GHC|DOPE|BLOCK|ABY|VIOR|BYC|UFO|XMG|XQN|BLITZ|VPN|BAY|DTC|AM|METAL|SPR|VTR|XPY|XRP|GAME|GP|NXS|COVAL|FSC2|SOON|HZ|XCP|BITB|XTC|XVG|GEO|FLDC|GEMZ|GRC|XCO|MTR|FLO|U|NBT|XEM|MUE|XVC|A8BIT|CLAM|XSEED|NTRN|SLING|DMD|GAM|UNIT|GRT|VIRAL|SPHR|ARB|OK|ADC|SNRG|PKB|TES|CPC|AEON|BITZ|ETH|GCR|TX|BCY|PRIME|EXP|NEU|SWING|INFX|OMNI|USDT|AMP|AGRS|XLM|SPRTS|YBC|BTA|MEC|BITCNY|AMS|SCRT|SCOT|CLUB|VOX|MND|EMC|FCT|MAID|FRK|EGC|SLS|ORB|STEPS|RADS|DCR|SAFEX|PIVX|WARP|CRBIT|MEME|STEEM|A2GIVE|LSK|KR|PDC|DGD|BRK|WAVES|RISE|LBC|SBD|BRX|DRACO|ETC|UNIQ|STRAT|UNB|SYNX|TRIG|EBST|VRM|XAUR|SEQ|SNGLS|REP|SHIFT|ARDR|XZC|NEO|ZEC|ZCL|IOP|DAR|GOLOS|GBG|UBQ|HKG|KMD|SIB|ION|LMC|QWARK|CRW|SWT|TIME|MLN|TKS|ARK|DYN|MUSIC|DTB|INCNT|GBYTE|GNT|NXC|EDG|LGD|TRST|WINGS|RLC|GNO|GUP|LUN|APX|TKN|HMQ|ANT|ZEN|SC|BAT|A1ST|QRL|CRB|TROLL|PTOY|MYST|CFI|BNT|NMR|SNT|DCT|XEL|MCO|ADT|FUN|PAY|MTL|STORJ|ADX|OMG|CVC|PART|QTUM|BCC|DNT|ADA|MANA|SALT|TIX|RCN|VIB|MER|POWR|BTG|ENGs
   deriving (Show)

data APIType
  = PublicAPI
  | AccountAPI
  | MarketAPI
  deriving (Eq)

instance Show APIType where
  show PublicAPI  = "public"
  show AccountAPI = "account"
  show MarketAPI  = "market"

data APIOpts
  = APIOpts
  { apiType :: APIType
  , qParams :: Params
  , version :: String
  , path    :: String
  , keys    :: APIKeys
  } deriving (Show, Eq)

data ErrorMessage
  = BittrexError BittrexError
  | DecodeFailure String Value
  deriving (Show, Eq, Generic)

data BittrexError
  = INVALID_MARKET
  | MARKET_NOT_PROVIDED
  | APIKEY_NOT_PROVIDED
  | APIKEY_INVALID
  | INVALID_SIGNATURE
  | NONCE_NOT_PROVIDED
  deriving (Show, Eq, Generic)

instance FromJSON ErrorMessage
instance FromJSON BittrexError

data MarketName
  = BTC_ADA
  | BTC_LTC
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

type Quantity = String
type Address = String
type PaymentId = String


-- data CoinType
--  = BITCOIN|NXT|BITCOINEX|NXT_MS|CRYPTO_NOTE_PAYMENTID|BITSHAREX|NXT_ASSET|COUNTERPARTY|BITCOIN_STEALTH|RIPPLE|NEM|ETH|OMNI|LUMEN|FACTOM|STEEM|BITCOIN_PERCENTAGE_FEE|LISK|ETH_CONTRACT|WAVES|ANTSHARES|WAVES_ASSET|BYTEBALL|SIA|IOTA|ADA

-- | API Keys
data APIKeys = APIKeys
  { apiKey :: String
  , secretKey :: String
  } deriving (Show, Eq)

