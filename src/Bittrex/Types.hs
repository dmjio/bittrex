{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
module Bittrex.Types where

import           Bittrex.Util         (parse)
import           Data.Aeson
import           Data.Aeson.Types     hiding (parse)
import           Data.ByteString      (ByteString)
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as L
import           Data.Scientific
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Time
import           GHC.Generics
import           Text.Read            (readMaybe)

type Params = [(String,String)]
type Rate = Scientific

data APIType
  = PublicAPI
  | AccountAPI
  | MarketAPI
  deriving (Eq)

newtype Time = Time UTCTime
  deriving (Show, Eq)

instance FromJSON Time where
  parseJSON = withText "Time" $ \t -> do
    pure $ Time $ parse (T.unpack t)

instance Show APIType where
  show AccountAPI = "account"
  show PublicAPI  = "public"
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
  | INVALID_PERMISSION
  | INVALID_CURRENCY
  | WITHDRAWAL_TOO_SMALL
  | CURRENCY_DOES_NOT_EXIST
  deriving (Show, Eq, Generic)

instance FromJSON ErrorMessage
instance FromJSON BittrexError

data MarketName
  = BTC_LTC
  | BTC_DOGE
  | BTC_VTC
  | BTC_PPC
  | BTC_FTC
  | BTC_RDD
  | BTC_NXT
  | BTC_DASH
  | BTC_POT
  | BTC_BLK
  | BTC_EMC2
  | BTC_XMY
  | BTC_AUR
  | BTC_EFL
  | BTC_GLD
  | BTC_SLR
  | BTC_PTC
  | BTC_GRS
  | BTC_NLG
  | BTC_RBY
  | BTC_XWC
  | BTC_MONA
  | BTC_THC
  | BTC_ENRG
  | BTC_ERC
  | BTC_VRC
  | BTC_CURE
  | BTC_XMR
  | BTC_CLOAK
  | BTC_START
  | BTC_KORE
  | BTC_XDN
  | BTC_TRUST
  | BTC_NAV
  | BTC_XST
  | BTC_BTCD
  | BTC_VIA
  | BTC_PINK
  | BTC_IOC
  | BTC_CANN
  | BTC_SYS
  | BTC_NEOS
  | BTC_DGB
  | BTC_BURST
  | BTC_EXCL
  | BTC_SWIFT
  | BTC_DOPE
  | BTC_BLOCK
  | BTC_ABY
  | BTC_BYC
  | BTC_XMG
  | BTC_BLITZ
  | BTC_BAY
  | BTC_FAIR
  | BTC_SPR
  | BTC_VTR
  | BTC_XRP
  | BTC_GAME
  | BTC_COVAL
  | BTC_NXS
  | BTC_XCP
  | BTC_BITB
  | BTC_GEO
  | BTC_FLDC
  | BTC_GRC
  | BTC_FLO
  | BTC_NBT
  | BTC_MUE
  | BTC_XEM
  | BTC_CLAM
  | BTC_DMD
  | BTC_GAM
  | BTC_SPHR
  | BTC_OK
  | BTC_SNRG
  | BTC_PKB
  | BTC_CPC
  | BTC_AEON
  | BTC_ETH
  | BTC_GCR
  | BTC_TX
  | BTC_BCY
  | BTC_EXP
  | BTC_INFX
  | BTC_OMNI
  | BTC_AMP
  | BTC_AGRS
  | BTC_XLM
  | USDT_BTC
  | BTC_CLUB
  | BTC_VOX
  | BTC_EMC
  | BTC_FCT
  | BTC_MAID
  | BTC_EGC
  | BTC_SLS
  | BTC_RADS
  | BTC_DCR
  | BTC_BSD
  | BTC_XVG
  | BTC_PIVX
  | BTC_XVC
  | BTC_MEME
  | BTC_STEEM
  | BTC_2GIVE
  | BTC_LSK
  | BTC_PDC
  | BTC_BRK
  | BTC_DGD
  | ETH_DGD
  | BTC_WAVES
  | BTC_RISE
  | BTC_LBC
  | BTC_SBD
  | BTC_BRX
  | BTC_ETC
  | ETH_ETC
  | BTC_STRAT
  | BTC_UNB
  | BTC_SYNX
  | BTC_TRIG
  | BTC_EBST
  | BTC_VRM
  | BTC_SEQ
  | BTC_REP
  | BTC_SHIFT
  | BTC_ARDR
  | BTC_XZC
  | BTC_NEO
  | BTC_ZEC
  | BTC_ZCL
  | BTC_IOP
  | BTC_GOLOS
  | BTC_UBQ
  | BTC_KMD
  | BTC_GBG
  | BTC_SIB
  | BTC_ION
  | BTC_LMC
  | BTC_QWARK
  | BTC_CRW
  | BTC_SWT
  | BTC_MLN
  | BTC_ARK
  | BTC_DYN
  | BTC_TKS
  | BTC_MUSIC
  | BTC_DTB
  | BTC_INCNT
  | BTC_GBYTE
  | BTC_GNT
  | BTC_NXC
  | BTC_EDG
  | BTC_LGD
  | BTC_TRST
  | ETH_GNT
  | ETH_REP
  | USDT_ETH
  | ETH_WINGS
  | BTC_WINGS
  | BTC_RLC
  | BTC_GNO
  | BTC_GUP
  | BTC_LUN
  | ETH_GUP
  | ETH_RLC
  | ETH_LUN
  | ETH_GNO
  | BTC_APX
  | BTC_HMQ
  | ETH_HMQ
  | BTC_ANT
  | ETH_TRST
  | ETH_ANT
  | BTC_SC
  | ETH_BAT
  | BTC_BAT
  | BTC_ZEN
  | BTC_1ST
  | BTC_QRL
  | ETH_1ST
  | ETH_QRL
  | BTC_CRB
  | ETH_CRB
  | ETH_LGD
  | BTC_PTOY
  | ETH_PTOY
  | BTC_MYST
  | ETH_MYST
  | BTC_CFI
  | ETH_CFI
  | BTC_BNT
  | ETH_BNT
  | BTC_NMR
  | ETH_NMR
  | ETH_LTC
  | ETH_XRP
  | BTC_SNT
  | ETH_SNT
  | BTC_DCT
  | BTC_XEL
  | BTC_MCO
  | ETH_MCO
  | BTC_ADT
  | ETH_ADT
  | BTC_FUN
  | ETH_FUN
  | BTC_PAY
  | ETH_PAY
  | BTC_MTL
  | ETH_MTL
  | BTC_STORJ
  | ETH_STORJ
  | BTC_ADX
  | ETH_ADX
  | ETH_DASH
  | ETH_SC
  | ETH_ZEC
  | USDT_ZEC
  | USDT_LTC
  | USDT_ETC
  | USDT_XRP
  | BTC_OMG
  | ETH_OMG
  | BTC_CVC
  | ETH_CVC
  | BTC_PART
  | BTC_QTUM
  | ETH_QTUM
  | ETH_XMR
  | ETH_XEM
  | ETH_XLM
  | ETH_NEO
  | USDT_XMR
  | USDT_DASH
  | ETH_BCC
  | USDT_BCC
  | BTC_BCC
  | BTC_DNT
  | ETH_DNT
  | USDT_NEO
  | ETH_WAVES
  | ETH_STRAT
  | ETH_DGB
  | ETH_FCT
  | USDT_OMG
  | BTC_ADA
  | BTC_MANA
  | ETH_MANA
  | BTC_SALT
  | ETH_SALT
  | BTC_TIX
  | ETH_TIX
  | BTC_RCN
  | ETH_RCN
  | BTC_VIB
  | ETH_VIB
  | BTC_MER
  | BTC_POWR
  | ETH_POWR
  | BTC_BTG
  | ETH_BTG
  | USDT_BTG
  | ETH_ADA
  | BTC_ENG
  | ETH_ENG
  deriving (Show, Eq, Generic, Read)

instance FromJSON MarketName where
  parseJSON = withText "MarketName" $ \s ->
    case readMaybe $ T.unpack (T.replace "-" "_" s) of
      Nothing -> error $ "Couldn't parse MarketName: " ++ T.unpack s
      Just k -> pure k

data Ticker
  = Ticker
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

data MarketHistory
  = MarketHistory
  { marketHistoryId :: Integer
  , marketHistoryTimeStamp :: Time
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

type Quantity = Scientific
type Address = String
type PaymentId = String

-- | API Keys
data APIKeys = APIKeys
  { apiKey :: String
  , secretKey :: String
  } deriving (Show, Eq)

data WithdrawalHistory
  = WithdrawalHistory
  { whPaymentUuid :: Text
  , whCurrency :: Text
  , whAmount :: Scientific
  , whAddress :: Text
  , whOpened :: Text
  , whAuthorized :: Bool
  , whPendingPayment :: Bool
  , whTxCost :: Scientific
  , whTxId :: Text
  , whCanceled :: Bool
  , whInvalidAddress :: Bool
  } deriving (Show, Eq, Generic)

instance FromJSON WithdrawalHistory where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 2
  }

data DepositHistory
  = DepositHistory
  { dhCurrency :: Text
  , dhAmount :: Scientific
  , dhLastUpdated :: Text
  , dhConfirmations :: Scientific
  , dhId :: Scientific
  , dhTxId :: Text
  , dhCryptoAddress :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON DepositHistory where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 2
  }

type CurrencyName = Text

data DepositAddress
  = DepositAddress
  { daCurrency :: Text
  , daAddress :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON DepositAddress where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 2
  }

newtype UUID = UUID Text
  deriving (Show, Eq)

instance FromJSON UUID where
  parseJSON = withObject "UUID" $ \o ->
    UUID <$> o .: "uuid"

data Balance
  = Balance
  { bCurrency :: Text
  , bBalance :: Scientific
  , bAvailable :: Scientific
  , bPending :: Scientific
  , bCryptoAddress :: Text
  , bUuid :: Maybe Text
  } deriving (Show, Eq, Generic)

instance FromJSON Balance where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 1
  }

data OrderType
  = SELL
  | BUY
  | LIMIT_SELL
  | LIMIT_BUY
  deriving (Show, Generic, Eq)

instance FromJSON OrderType

data OpenOrder
  = OpenOrder
  { ooUuid :: Maybe Text
  , ooOrderUuid :: Text
  , ooExchange :: Text
  , ooOrderType :: OrderType
  , ooQuantity :: Scientific
  , ooQuantityRemaining :: Scientific
  , ooLimit :: Scientific
  , ooCommissionPaid :: Scientific
  , ooPrice :: Scientific
  , ooPricePerUnit :: Maybe Scientific
  , ooOpened :: Text
  , ooClosed :: Maybe Text
  , ooCancelInitiated :: Bool
  , ooImmediateOrCancel :: Bool
  , ooIsConditional :: Bool
  , ooCondition :: Maybe Text
  , ooConditionTarget :: Maybe Text
  } deriving (Show, Eq, Generic)

instance FromJSON OpenOrder where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 2
  }

data OrderHistory
  = OrderHistory
    { ohOrderUuid :: Text
    , ohExchange :: Text
    , ohTimeStamp :: Time
    , ohOrderType :: OrderType
    , ohLimit :: Scientific
    , ohQuantity :: Scientific
    , ohQuantityRemaining :: Scientific
    , ohCommission :: Scientific
    , ohPrice :: Scientific
    , ohPricePerUnit :: Maybe Scientific
    , ohIsConditional :: Bool
    , ohImmediateOrCancel :: Bool
    } deriving (Show, Eq, Generic)

instance FromJSON OrderHistory where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 2
  }

data Order
  = Order
    { oAccountId :: Maybe Text
    , oOrderUuid :: Text
    , oExchange :: Text
    , oOrderType :: OrderType
    , oQuantity :: Scientific
    , oQuantityRemaining :: Scientific
    , oLimit :: Scientific
    , oReserved :: Scientific
    , oReservedRemaining :: Scientific
    , oCommissionReserved :: Scientific
    , oCommissionReserveRemaining :: Scientific
    , oCommissionPaid :: Scientific
    , oPrice :: Scientific
    , oPricePerUnit :: Maybe Scientific
    , oOpen :: Text
    , oIsOpen :: Bool
    , oSentinal :: Text
    , oTimeStamp :: Time
    , oCommission :: Scientific
    , oIsConditional :: Bool
    , oImmediateOrCancel :: Bool
    , oCancelInitiated :: Bool
    , oCondition :: Text
    } deriving (Show, Eq, Generic)

instance FromJSON Order where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 1
  }

data MarketSummary
  = MarketSummary
  { mhMarketName :: MarketName
  , mhHigh :: Scientific
  , mhLow :: Scientific
  , mhVolume :: Scientific
  , mhLast :: Scientific
  , mhBaseVolume :: Scientific
  , mhTimeStamp :: Time
  , mhBid :: Scientific
  , mhAsk :: Scientific
  , mhOpenBuyOrders :: Scientific
  , mhOpenSellOrders :: Scientific
  , mhPrevDay :: Scientific
  , mhCreated :: Text
  , mhDisplayMarketName :: Maybe Text
  } deriving (Show, Eq, Generic)

instance FromJSON MarketSummary where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 2
  }
