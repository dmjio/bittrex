--------------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}

--------------------------------------------------------------------------------

module Bittrex.Types where

--------------------------------------------------------------------------------

import           Data.Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.Fixed
import           Data.Scientific
import           Data.Text        (Text)
import qualified Data.Text        as Text
import           Data.Time
import           Data.Time.Format
import           Flow             ((.>))
import           GHC.Generics     (Generic)
import           Text.Read        (readMaybe)

--------------------------------------------------------------------------------

data E8

instance HasResolution E8 where
  resolution _ = 10 ^ 8

--------------------------------------------------------------------------------

type Params = [(String, String)]

--------------------------------------------------------------------------------

newtype Time
  = Time UTCTime
  deriving (Eq, Show)

instance FromJSON Time where
  parseJSON = withText "Time" $ \t -> pure (Time (parse (Text.unpack t)))
    where
      parse :: String -> UTCTime
      parse = parseTimeOrError True defaultTimeLocale
              $ iso8601DateFormat (Just "%H:%M:%S%Q")

--------------------------------------------------------------------------------

data APIType
  = PublicAPI
  | AccountAPI
  | MarketAPI
  deriving (Eq)

instance Show APIType where
  show AccountAPI = "account"
  show PublicAPI  = "public"
  show MarketAPI  = "market"

--------------------------------------------------------------------------------

data APIOpts
  = APIOpts
    { apiOptsAPIType     :: !APIType
    , apiOptsQueryParams :: !Params
    , apiOptsVersion     :: !Text
    , apiOptsPath        :: !Text
    , apiOptsKeys        :: !APIKeys
    }
  deriving (Eq, Show)

--------------------------------------------------------------------------------

-- FIXME: this is a weird hack

data ErrorMessage
  = BittrexError !BittrexError
  | DecodeFailure !String !Aeson.Value
  deriving (Eq, Show, Generic)

-- instance FromJSON ErrorMessage where
--   parseJSON value = do
--     case Aeson.fromJSON value of
--       (Aeson.Error  msg) -> pure (DecodeFailure msg value)
--       (Aeson.Success be) -> pure (BittrexError be)

--------------------------------------------------------------------------------

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
  | ADDRESS_GENERATING
  deriving (Eq, Show, Generic)

instance FromJSON BittrexError where
  parseJSON (String "INVALID_MARKET")          = pure INVALID_MARKET
  parseJSON (String "MARKET_NOT_PROVIDED")     = pure MARKET_NOT_PROVIDED
  parseJSON (String "APIKEY_NOT_PROVIDED")     = pure APIKEY_NOT_PROVIDED
  parseJSON (String "APIKEY_INVALID")          = pure APIKEY_INVALID
  parseJSON (String "INVALID_SIGNATURE")       = pure INVALID_SIGNATURE
  parseJSON (String "NONCE_NOT_PROVIDED")      = pure NONCE_NOT_PROVIDED
  parseJSON (String "INVALID_PERMISSION")      = pure INVALID_PERMISSION
  parseJSON (String "INVALID_CURRENCY")        = pure INVALID_CURRENCY
  parseJSON (String "WITHDRAWAL_TOO_SMALL")    = pure WITHDRAWAL_TOO_SMALL
  parseJSON (String "CURRENCY_DOES_NOT_EXIST") = pure CURRENCY_DOES_NOT_EXIST
  parseJSON (String "ADDRESS_GENERATING")      = pure ADDRESS_GENERATING

instance ToJSON BittrexError where
  toJSON = show .> toJSON

--------------------------------------------------------------------------------

data MarketName
  = NewMarket Text
  | MarketName MarketName'
  deriving (Eq, Show)

instance FromJSON MarketName where
  parseJSON = withText "Market Name" $ \t -> do
    case readMaybe (Text.unpack (Text.replace "-" "_" t)) of
      Nothing  -> pure (NewMarket t)
      (Just k) -> pure (MarketName k)

--------------------------------------------------------------------------------

data MarketName'
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
  | USDT_ADA
  | USDT_XVG
  | USDT_NXT
  | BTC_UKG
  | ETH_UKG
  deriving (Eq, Show, Read, Generic)

--------------------------------------------------------------------------------

newtype Bid
  = Bid (Fixed E8)
  deriving (Eq, Num, Show, Ord, Generic)

deriving instance FromJSON Bid

--------------------------------------------------------------------------------

newtype Ask
  = Ask (Fixed E8)
  deriving (Eq, Num, Show, Ord, Generic)

deriving instance FromJSON Ask

--------------------------------------------------------------------------------

newtype Last
  = Last (Fixed E8)
  deriving (Eq, Num, Show, Ord, Generic)

deriving instance FromJSON Last

--------------------------------------------------------------------------------

newtype High
  = High (Fixed E8)
  deriving (Eq, Num, Show, Ord, Generic)

deriving instance FromJSON High

--------------------------------------------------------------------------------

newtype Low
  = Low (Fixed E8)
  deriving (Eq, Num, Show, Ord, Generic)

deriving instance FromJSON Low

--------------------------------------------------------------------------------

newtype Volume
  = Volume (Fixed E8)
  deriving (Eq, Num, Show, Ord, Generic)

deriving instance FromJSON Volume

--------------------------------------------------------------------------------

newtype BaseVolume
  = BaseVolume (Fixed E8)
  deriving (Eq, Num, Show, Ord, Generic)

deriving instance FromJSON BaseVolume

--------------------------------------------------------------------------------

newtype PrevDay
  = PrevDay (Fixed E8)
  deriving (Eq, Num, Show, Ord, Generic)

deriving instance FromJSON PrevDay

--------------------------------------------------------------------------------

newtype Quantity
  = Quantity (Fixed E8)
  deriving (Eq, Num, Show, Ord, Generic)

deriving instance FromJSON Quantity

--------------------------------------------------------------------------------

newtype Rate
  = Rate (Fixed E8)
  deriving (Eq, Num, Show, Ord, Generic)

deriving instance FromJSON Rate

--------------------------------------------------------------------------------

newtype Price
  = Price (Fixed E8)
  deriving (Eq, Num, Show, Ord, Generic)

deriving instance FromJSON Price

--------------------------------------------------------------------------------

newtype Total
  = Total (Fixed E8)
  deriving (Eq, Num, Show, Ord, Generic)

deriving instance FromJSON Total

--------------------------------------------------------------------------------

newtype QuantityRemaining
  = QuantityRemaining (Fixed E8)
  deriving (Eq, Num, Show, Ord, Generic)

deriving instance FromJSON QuantityRemaining

--------------------------------------------------------------------------------

newtype Limit
  = Limit (Fixed E8)
  deriving (Eq, Num, Show, Ord, Generic)

deriving instance FromJSON Limit

--------------------------------------------------------------------------------

newtype CommissionPaid
  = CommissionPaid (Fixed E8)
  deriving (Eq, Num, Show, Ord, Generic)

deriving instance FromJSON CommissionPaid

--------------------------------------------------------------------------------

-- FIXME: rename this...

newtype Balance'
  = Balance' (Fixed E8)
  deriving (Eq, Num, Show, Ord, Generic)

deriving instance FromJSON Balance'

--------------------------------------------------------------------------------

newtype Available
  = Available (Fixed E8)
  deriving (Eq, Num, Show, Ord, Generic)

deriving instance FromJSON Available

--------------------------------------------------------------------------------

newtype Pending
  = Pending (Fixed E8)
  deriving (Eq, Num, Show, Ord, Generic)

deriving instance FromJSON Pending

--------------------------------------------------------------------------------

newtype Reserved
  = Reserved (Fixed E8)
  deriving (Eq, Num, Show, Ord, Generic)

deriving instance FromJSON Reserved

--------------------------------------------------------------------------------

newtype ReserveRemaining
  = ReserveRemaining (Fixed E8)
  deriving (Eq, Num, Show, Ord, Generic)

deriving instance FromJSON ReserveRemaining

--------------------------------------------------------------------------------

newtype CommissionReserved
  = CommissionReserved (Fixed E8)
  deriving (Eq, Num, Show, Ord, Generic)

deriving instance FromJSON CommissionReserved

--------------------------------------------------------------------------------

newtype CommissionReserveRemaining
  = CommissionReserveRemaining (Fixed E8)
  deriving (Eq, Num, Show, Ord, Generic)

deriving instance FromJSON CommissionReserveRemaining

--------------------------------------------------------------------------------

newtype TxCost
  = TxCost (Fixed E8)
  deriving (Eq, Num, Show, Ord, Generic)

deriving instance FromJSON TxCost

--------------------------------------------------------------------------------

newtype Amount
  = Amount (Fixed E8)
  deriving (Eq, Num, Show, Ord, Generic)

deriving instance FromJSON Amount

--------------------------------------------------------------------------------

newtype Commission
  = Commission (Fixed E8)
  deriving (Eq, Num, Show, Ord, Generic)

deriving instance FromJSON Commission

--------------------------------------------------------------------------------

data Ticker
  = Ticker
    { tickerBid  :: !Bid
    , tickerAsk  :: !Ask
    , tickerLast :: !Last
    }
  deriving (Show, Generic)

instance FromJSON Ticker where
  parseJSON = withObject "Ticker" $ \o -> do
    tickerBid  <- o .: "Bid"
    tickerAsk  <- o .: "Ask"
    tickerLast <- o .: "Last"
    pure (Ticker {..})

--------------------------------------------------------------------------------

newtype MinTradeSize
  = MinTradeSize (Fixed E8)
  deriving (Eq, Num, Show, Ord, Generic)

deriving instance FromJSON MinTradeSize

--------------------------------------------------------------------------------

newtype TxFee
  = TxFee (Fixed E8)
  deriving (Eq, Num, Show, Ord, Generic)

deriving instance FromJSON TxFee

--------------------------------------------------------------------------------

data Market
  = Market
    { marketMarketCurrency     :: !Text
    , marketBaseCurrency       :: !Text
    , marketMarketCurrencyLong :: !Text
    , marketBaseCurrencyLong   :: !Text
    , marketMinTradeSize       :: !MinTradeSize
    , marketName               :: !MarketName
    , marketIsActive           :: !Bool
    , marketCreated            :: !Time
    }
  deriving (Eq, Show)

instance FromJSON Market where
  parseJSON = withObject "Market" $ \o -> do
    marketMarketCurrency     <- o .: "MarketCurrency"
    marketBaseCurrency       <- o .: "BaseCurrency"
    marketMarketCurrencyLong <- o .: "MarketCurrencyLong"
    marketBaseCurrencyLong   <- o .: "BaseCurrencyLong"
    marketMinTradeSize       <- o .: "MinTradeSize"
    marketName               <- o .: "MarketName"
    marketIsActive           <- o .: "IsActive"
    marketCreated            <- o .: "Created"
    pure (Market {..})

--------------------------------------------------------------------------------

data Currency
  = Currency
    { currencyName            :: !Text
    , currencyNameLong        :: !Text
    , currencyMinConfirmation :: !Int
    , currencyTxFee           :: !TxFee
    , currencyIsActive        :: !Bool
    , currencyCoinType        :: !Text
    , currencyBaseAddress     :: !(Maybe Text)
    }
  deriving (Eq, Show)

instance FromJSON Currency where
  parseJSON = withObject "Currency" $ \o -> do
    currencyName            <- o .:  "Currency"
    currencyNameLong        <- o .:  "CurrencyLong"
    currencyMinConfirmation <- o .:  "MinConfirmation"
    currencyTxFee           <- o .:  "TxFee"
    currencyIsActive        <- o .:  "IsActive"
    currencyCoinType        <- o .:  "CoinType"
    currencyBaseAddress     <- o .:? "BaseAddress"
    pure (Currency {..})

--------------------------------------------------------------------------------

data OrderBookEntry
  = OrderBookEntry
    { orderBookEntryQuantity :: !Quantity
    , orderBookEntryRate     :: !Rate
    }
  deriving (Eq, Show)

instance FromJSON OrderBookEntry where
  parseJSON = withObject "OrderBookEntry" $ \o -> do
    orderBookEntryQuantity <- o .: "Quantity"
    orderBookEntryRate     <- o .: "Rate"
    pure (OrderBookEntry {..})

--------------------------------------------------------------------------------

data OrderBook
  = OrderBook
    { orderBookBuy  :: ![OrderBookEntry]
    , orderBookSell :: ![OrderBookEntry]
    }
  deriving (Eq, Show)

instance FromJSON OrderBook where
  parseJSON = withObject "OrderBook" $ \o -> do
    orderBookBuy  <- o .: "buy"
    orderBookSell <- o .: "sell"
    pure (OrderBook {..})

--------------------------------------------------------------------------------

data MarketHistory
  = MarketHistory
    { marketHistoryId        :: !Integer -- FIXME
    , marketHistoryTimeStamp :: !Time
    , marketHistoryQuantity  :: !Quantity
    , marketHistoryPrice     :: !Price
    , marketHistoryTotal     :: !Total
    , marketHistoryFillType  :: !Text
    , marketHistoryOrderType :: !Text
    }
  deriving (Eq, Show)

instance FromJSON MarketHistory where
  parseJSON = withObject "MarketHistory" $ \o -> do
    marketHistoryId        <- o .: "Id"
    marketHistoryTimeStamp <- o .: "TimeStamp"
    marketHistoryQuantity  <- o .: "Quantity"
    marketHistoryPrice     <- o .: "Price"
    marketHistoryTotal     <- o .: "Total"
    marketHistoryFillType  <- o .: "FillType"
    marketHistoryOrderType <- o .: "OrderType"
    pure (MarketHistory {..})

--------------------------------------------------------------------------------

-- | API Keys
data APIKeys
  = APIKeys
    { apiKey    :: !String -- FIXME: should be Text??
    , secretKey :: !String -- FIXME: should be Text??
    }
  deriving (Eq, Show)

--------------------------------------------------------------------------------

type Address = String -- FIXME: should be Text??

--------------------------------------------------------------------------------

type PaymentId = String -- FIXME: should be Text??

--------------------------------------------------------------------------------

data WithdrawalHistory
  = WithdrawalHistory
    { withdrawalHistoryPaymentUUID    :: !Text
    , withdrawalHistoryCurrency       :: !Text
    , withdrawalHistoryAmount         :: !Amount
    , withdrawalHistoryAddress        :: !Text
    , withdrawalHistoryOpened         :: !Text
    , withdrawalHistoryAuthorized     :: !Bool
    , withdrawalHistoryPendingPayment :: !Bool
    , withdrawalHistoryTxCost         :: !Scientific
    , withdrawalHistoryTxId           :: !Text
    , withdrawalHistoryCanceled       :: !Bool
    , withdrawalHistoryInvalidAddress :: !Bool
    }
  deriving (Eq, Show, Generic)

instance FromJSON WithdrawalHistory where
  parseJSON = withObject "WithdrawalHistory" $ \o -> do
    withdrawalHistoryPaymentUUID    <- o .: "PaymentUuid"
    withdrawalHistoryCurrency       <- o .: "Currency"
    withdrawalHistoryAmount         <- o .: "Amount"
    withdrawalHistoryAddress        <- o .: "Address"
    withdrawalHistoryOpened         <- o .: "Opened"
    withdrawalHistoryAuthorized     <- o .: "Authorized"
    withdrawalHistoryPendingPayment <- o .: "PendingPayment"
    withdrawalHistoryTxCost         <- o .: "TxCost"
    withdrawalHistoryTxId           <- o .: "TxId"
    withdrawalHistoryCanceled       <- o .: "Canceled"
    withdrawalHistoryInvalidAddress <- o .: "InvalidAddress"
    pure (WithdrawalHistory {..})

--------------------------------------------------------------------------------

data DepositHistory
  = DepositHistory
    { depositHistoryCurrency      :: !Text
    , depositHistoryAmount        :: !Scientific
    , depositHistoryLastUpdated   :: !Text
    , depositHistoryConfirmations :: !Scientific
    , depositHistoryId            :: !Scientific
    , depositHistoryTxId          :: !Text
    , depositHistoryCryptoAddress :: !Text
    }
  deriving (Eq, Show, Generic)

instance FromJSON DepositHistory where
  parseJSON = withObject "DepositHistory" $ \o -> do
    depositHistoryCurrency      <- o .: "Currency"
    depositHistoryAmount        <- o .: "Amount"
    depositHistoryLastUpdated   <- o .: "LastUpdated"
    depositHistoryConfirmations <- o .: "Confirmations"
    depositHistoryId            <- o .: "Id"
    depositHistoryTxId          <- o .: "TxId"
    depositHistoryCryptoAddress <- o .: "CryptoAddress"
    pure (DepositHistory {..})

--------------------------------------------------------------------------------

type CurrencyName = Text

--------------------------------------------------------------------------------

data DepositAddress
  = DepositAddress
    { depositAddressCurrency :: !Text
    , depositAddressAddress  :: !Text
    }
  deriving (Eq, Show, Generic)

instance FromJSON DepositAddress where
  parseJSON = withObject "DepositAddress" $ \o -> do
    depositAddressCurrency <- o .: "Currency"
    depositAddressAddress  <- o .: "Address"
    pure (DepositAddress {..})

--------------------------------------------------------------------------------

newtype UUID
  = UUID Text
  deriving (Eq, Show)

instance FromJSON UUID where
  parseJSON = withObject "UUID" $ \o -> do
    UUID <$> o .: "uuid"

--------------------------------------------------------------------------------

data Balance
  = Balance
    { balanceCurrency      :: !Text
    , balanceBalance       :: !Balance'
    , balanceAvailable     :: !Available
    , balancePending       :: !Pending
    , balanceCryptoAddress :: !(Maybe Text)
    , balanceUUID          :: !(Maybe Text)
    }
  deriving (Eq, Show, Generic)

instance FromJSON Balance where
  parseJSON = withObject "Balance" $ \o -> do
    balanceCurrency      <- o .:  "Currency"
    balanceBalance       <- o .:  "Balance"
    balanceAvailable     <- o .:  "Available"
    balancePending       <- o .:  "Pending"
    balanceCryptoAddress <- o .:? "CryptoAddress"
    balanceUUID          <- o .:? "Uuid"
    pure (Balance {..})

--------------------------------------------------------------------------------

data OrderType
  = SELL
  | BUY
  | LIMIT_SELL
  | LIMIT_BUY
  deriving (Eq, Show, Generic)

instance FromJSON OrderType where
  parseJSON (String "SELL")       = pure SELL
  parseJSON (String "BUY")        = pure BUY
  parseJSON (String "LIMIT_SELL") = pure LIMIT_SELL
  parseJSON (String "LIMIT_BUY")  = pure LIMIT_BUY

--------------------------------------------------------------------------------

data OpenOrder
  = OpenOrder
    { openOrderUUID              :: !(Maybe Text)
    , openOrderOrderUUID         :: !Text
    , openOrderExchange          :: !Text
    , openOrderOrderType         :: !OrderType
    , openOrderQuantity          :: !Quantity
    , openOrderQuantityRemaining :: !QuantityRemaining
    , openOrderLimit             :: !Limit
    , openOrderCommissionPaid    :: !CommissionPaid
    , openOrderPrice             :: !Price
    , openOrderPricePerUnit      :: !(Maybe Price)
    , openOrderOpened            :: !Time
    , openOrderClosed            :: !(Maybe Time)
    , openOrderCancelInitiated   :: !Bool
    , openOrderImmediateOrCancel :: !Bool
    , openOrderIsConditional     :: !Bool
    , openOrderCondition         :: !(Maybe Text)
    , openOrderConditionTarget   :: !(Maybe Text)
    }
  deriving (Eq, Show, Generic)

instance FromJSON OpenOrder where
  parseJSON = withObject "OpenOrder" $ \o -> do
    openOrderUUID              <- o .:? "Uuid"
    openOrderOrderUUID         <- o .:  "OrderUuid"
    openOrderExchange          <- o .:  "Exchange"
    openOrderOrderType         <- o .:  "OrderType"
    openOrderQuantity          <- o .:  "Quantity"
    openOrderQuantityRemaining <- o .:  "QuantityRemaining"
    openOrderLimit             <- o .:  "Limit"
    openOrderCommissionPaid    <- o .:  "CommissionPaid"
    openOrderPrice             <- o .:  "Price"
    openOrderPricePerUnit      <- o .:? "PricePerUnit"
    openOrderOpened            <- o .:  "Opened"
    openOrderClosed            <- o .:? "Closed"
    openOrderCancelInitiated   <- o .:  "CancelInitiated"
    openOrderImmediateOrCancel <- o .:  "ImmediateOrCancel"
    openOrderIsConditional     <- o .:  "IsConditional"
    openOrderCondition         <- o .:? "Condition"
    openOrderConditionTarget   <- o .:? "ConditionTarget"
    pure (OpenOrder {..})

--------------------------------------------------------------------------------

data OrderHistory
  = OrderHistory
    { orderHistoryOrderUUID         :: !Text
    , orderHistoryExchange          :: !Text
    , orderHistoryTimeStamp         :: !Time
    , orderHistoryOrderType         :: !OrderType
    , orderHistoryLimit             :: !Limit
    , orderHistoryQuantity          :: !Quantity
    , orderHistoryQuantityRemaining :: !QuantityRemaining
    , orderHistoryCommission        :: !Commission
    , orderHistoryPrice             :: !Price
    , orderHistoryPricePerUnit      :: !(Maybe Price)
    , orderHistoryIsConditional     :: !Bool
    , orderHistoryCondition         :: !Text
    , orderHistoryConditionTarget   :: !(Maybe Text)
    , orderHistoryImmediateOrCancel :: !Bool
    }
  deriving (Eq, Show, Generic)

instance FromJSON OrderHistory where
  parseJSON = withObject "OrderHistory" $ \o -> do
    orderHistoryOrderUUID         <- o .:  "OrderUuid"
    orderHistoryExchange          <- o .:  "Exchange"
    orderHistoryTimeStamp         <- o .:  "TimeStamp"
    orderHistoryOrderType         <- o .:  "OrderType"
    orderHistoryLimit             <- o .:  "Limit"
    orderHistoryQuantity          <- o .:  "Quantity"
    orderHistoryQuantityRemaining <- o .:  "QuantityRemaining"
    orderHistoryCommission        <- o .:  "Commission"
    orderHistoryPrice             <- o .:  "Price"
    orderHistoryPricePerUnit      <- o .:? "PricePerUnit"
    orderHistoryIsConditional     <- o .:  "IsConditional"
    orderHistoryCondition         <- o .:  "Condition"
    orderHistoryConditionTarget   <- o .:? "ConditionTarget"
    orderHistoryImmediateOrCancel <- o .:  "ImmediateOrCancel"
    pure (OrderHistory {..})

--------------------------------------------------------------------------------

data Order
  = Order
    { orderAccountID                  :: !(Maybe Text)
    , orderOrderUUID                  :: !Text
    , orderExchange                   :: !Text
    , orderOrderType                  :: !OrderType
    , orderQuantity                   :: !Quantity
    , orderQuantityRemaining          :: !QuantityRemaining
    , orderLimit                      :: !Limit
    , orderReserved                   :: !Reserved
    , orderReservedRemaining          :: !ReserveRemaining
    , orderCommissionReserved         :: !CommissionReserved
    , orderCommissionReserveRemaining :: !CommissionReserveRemaining
    , orderCommissionPaid             :: !CommissionPaid
    , orderPrice                      :: !Price
    , orderPricePerUnit               :: !(Maybe Price)
    , orderOpened                     :: !Time
    , orderClosed                     :: !(Maybe Time)
    , orderIsOpen                     :: !Bool
    , orderSentinel                   :: !Text
    , orderCommission                 :: !Commission
    , orderIsConditional              :: !Bool
    , orderCancelInitiated            :: !Bool
    , orderImmediateOrCancel          :: !Bool
    , orderCondition                  :: !Text
    , orderConditionTarget            :: !(Maybe Text)
    }
  deriving (Eq, Show, Generic)

instance FromJSON Order where
  parseJSON = withObject "Order" $ \o -> do
    orderAccountID                  <- o .:? "AccountId"
    orderOrderUUID                  <- o .:  "OrderUuid"
    orderExchange                   <- o .:  "Exchange"
    orderOrderType                  <- o .:  "OrderType"
    orderQuantity                   <- o .:  "Quantity"
    orderQuantityRemaining          <- o .:  "QuantityRemaining"
    orderLimit                      <- o .:  "Limit"
    orderReserved                   <- o .:  "Reserved"
    orderReservedRemaining          <- o .:  "ReservedRemaining"
    orderCommissionReserved         <- o .:  "CommissionReserved"
    orderCommissionReserveRemaining <- o .:  "CommissionReserveRemaining"
    orderCommissionPaid             <- o .:  "CommissionPaid"
    orderPrice                      <- o .:  "Price"
    orderPricePerUnit               <- o .:? "PricePerUnit"
    orderOpened                     <- o .:  "Opened"
    orderClosed                     <- o .:? "Closed"
    orderIsOpen                     <- o .:  "IsOpen"
    orderSentinel                   <- o .:  "Sentinel"
    orderCommission                 <- o .:  "Commission"
    orderIsConditional              <- o .:  "IsConditional"
    orderCancelInitiated            <- o .:  "CancelInitiated"
    orderImmediateOrCancel          <- o .:  "ImmediateOrCancel"
    orderCondition                  <- o .:  "Condition"
    orderConditionTarget            <- o .:? "ConditionTarget"
    pure (Order {..})

--------------------------------------------------------------------------------

data MarketSummary
  = MarketSummary
    { marketSummaryMarketName        :: !MarketName
    , marketSummaryHigh              :: !High
    , marketSummaryLow               :: !Low
    , marketSummaryVolume            :: !Volume
    , marketSummaryLast              :: !Last
    , marketSummaryBaseVolume        :: !BaseVolume
    , marketSummaryTimeStamp         :: !Time
    , marketSummaryBid               :: !Bid
    , marketSummaryAsk               :: !Ask
    , marketSummaryOpenBuyOrders     :: !Int
    , marketSummaryOpenSellOrders    :: !Int
    , marketSummaryPrevDay           :: !PrevDay
    , marketSummaryCreated           :: !Time
    , marketSummaryDisplayMarketName :: !(Maybe Text)
    }
  deriving (Eq, Show, Generic)

instance FromJSON MarketSummary where
  parseJSON = withObject "MarketSummary" $ \o -> do
    marketSummaryMarketName        <- o .:  "MarketName"
    marketSummaryHigh              <- o .:  "High"
    marketSummaryLow               <- o .:  "Low"
    marketSummaryVolume            <- o .:  "Volume"
    marketSummaryLast              <- o .:  "Last"
    marketSummaryBaseVolume        <- o .:  "BaseVolume"
    marketSummaryTimeStamp         <- o .:  "TimeStamp"
    marketSummaryBid               <- o .:  "Bid"
    marketSummaryAsk               <- o .:  "Ask"
    marketSummaryOpenBuyOrders     <- o .:  "OpenBuyOrders"
    marketSummaryOpenSellOrders    <- o .:  "OpenSellOrders"
    marketSummaryPrevDay           <- o .:  "PrevDay"
    marketSummaryCreated           <- o .:  "Created"
    marketSummaryDisplayMarketName <- o .:? "DisplayMarketName"
    pure (MarketSummary {..})

--------------------------------------------------------------------------------
