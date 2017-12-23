{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
module Bittrex.Types where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString      (ByteString)
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as L
import           Data.Scientific
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Time
import           GHC.Generics

type Params = [(String,String)]
type Rate = Scientific

data APIType
  = PublicAPI
  | AccountAPI
  | MarketAPI
  deriving (Eq)

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
    , ohTimeStamp :: Text
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
    , oTimeStamp :: Text
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
