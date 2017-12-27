{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE DeriveGeneric       #-}
module Bittrex.API
  (
    -- * Overview
    -- $intro

    -- * Public
    getMarkets
  , getCurrencies
  , getTicker
  , getMarketSummaries
  , getMarketSummary
  , getOrderBookBuys
  , getOrderBookSells
  , getMarketHistory
    -- * Market
  , buyLimit
  , sellLimit
  , cancel
  , getOpenOrders
    -- * Account
  , getBalances
  , getBalance
  , getDepositAddress
  , withdraw
  , getOrder
  , getOrderHistory
  , getWithdrawalHistory
  , getDepositHistory
  ) where

import           Data.Aeson
import           Data.Char
import           Data.Monoid
import           Data.Scientific
import           Data.Text        (Text)
import qualified Data.Text        as T

import           Bittrex.Types
import           Bittrex.Util
import           Bittrex.Internal

-- $intro
-- Bittrex provides a simple and powerful REST API to allow you to programatically perform nearly all actions you can from our web interface.
--
-- All requests use the application/json content type and go over https.
-- The base url is https://bittrex.com/api/{version}/.
--
-- All requests are GET requests and all responses come in a default response object with the result in the result field. Always check the success flag to ensure that your API call succeeded.
--
-- We are currently restricting orders to 500 open orders and 200,000 orders a day. We reserve the right to change these settings as we tune the system. If you are affected by these limits as an active trader, please email support@bittrex.com.
--
-- If you have any questions, feedback or recommendation for API support you can post a question in our support center.
--

-- | Used to get the open and available trading markets at Bittrex along with other meta data.
getMarkets
  :: IO (Either ErrorMessage [Market])
getMarkets = callAPI defOpts { path = "getmarkets" }

-- | Used to get all supported currencies at Bittrex along with other meta data.
getCurrencies
  :: IO (Either ErrorMessage [Currency])
getCurrencies = callAPI defOpts { path = "getcurrencies" }

-- | Used to get the current tick values for a market.
getTicker
  :: MarketName -- ^ a string literal for the market (ex: `BTC-LTC`)
  -> IO (Either ErrorMessage Ticker)
getTicker market =
  callAPI defOpts {
      qParams = [("market", camelToDash $ show market)]
    , path = "getticker"
    }

-- | Used to get the last 24 hour summary of all active exchanges
getMarketSummaries
  :: IO (Either ErrorMessage [MarketSummary])
getMarketSummaries =
  callAPI defOpts { path = "getmarketsummaries" }

-- | Used to get the last 24 hour summary of all active exchanges
getMarketSummary
  :: MarketName -- ^ a string literal for the market (ex: `BTC-LTC`)
  -> IO (Either ErrorMessage [MarketSummary])
getMarketSummary market =
  callAPI defOpts {
      qParams = pure ("market", camelToDash $ show market)
    , path = "getmarketsummary"
    }

-- | Used to get retrieve the orderbook for a given market
getOrderBookBuys
  :: MarketName -- ^ a string literal for the market (ex: `BTC-LTC`)
--  -> OrderBookType -- ^ buy, sell or both to identify the type of orderbook to return.
  -> IO (Either ErrorMessage [OrderBookEntry])
getOrderBookBuys market =
  callAPI defOpts {
      path = "getorderbook"
    , qParams = [ ("market", camelToDash $ show market)
                , ("type", "buy")
                ]
    }

-- | Used to get retrieve the orderbook for a given market
getOrderBookSells
  :: MarketName -- ^ a string literal for the market (ex: `BTC-LTC`)
--  -> OrderBookType -- ^ buy, sell or both to identify the type of orderbook to return.
  -> IO (Either ErrorMessage [OrderBookEntry])
getOrderBookSells market =
  callAPI defOpts {
      path = "getorderbook"
    , qParams = [ ("market", camelToDash $ show market)
                , ("type", "sell")
                ]
    }

-- | Used to retrieve the latest trades that have occured for a specific market.
getMarketHistory
  :: MarketName -- ^ string literal for the market (ex: `BTC-LTC`)
  -> IO (Either ErrorMessage [MarketHistory])
getMarketHistory market =
  callAPI defOpts {
      path = "getmarkethistory"
    , qParams = pure ("market", camelToDash $ show market)
    }

-- | Get all orders that you currently have opened. A specific market can be requested
getOpenOrders
  :: APIKeys    -- ^ Bittrex API credentials
  -> MarketName -- ^ String literal for the market (ie. BTC-LTC)
  -> IO (Either ErrorMessage [OpenOrder])
getOpenOrders keys market =
  callAPI defOpts {
      path      = "getopenorders"
    , apiType   = MarketAPI
    , qParams   = pure ("market", camelToDash $ show market)
    , keys      = keys
    }

-- | Used to place a buy order in a specific market. Use buylimit to place limit orders. Make sure you have the proper permissions set on your API keys for this call to work
buyLimit
  :: APIKeys    -- ^ Bittrex API credentials
  -> MarketName -- ^ A string literal for the market (ex: BTC-LTC)
  -> Quantity   -- ^ The amount to purchase
  -> Rate       -- ^ The rate at which to place the order.
  -> IO (Either ErrorMessage UUID)
buyLimit keys market quantity rate =
  callAPI defOpts {
      path      = "buylimit"
    , keys      = keys
    , apiType   = MarketAPI
    , qParams   = [ ("market", camelToDash $ show market )
                  , ("quantity", show quantity )
                  , ("rate", show rate )
                  ]
    }

-- | Used to place an sell order in a specific market. Use selllimit to place limit orders.
-- Make sure you have the proper permissions set on your API keys for this call to workn
sellLimit
  :: APIKeys    -- ^ Bittrex API credentials
  -> MarketName -- ^ A string literal for the market (ex: BTC-LTC)
  -> Quantity   -- ^ The amount to purchase
  -> Rate       -- ^ The rate at which to place the order
  -> IO (Either ErrorMessage UUID)
sellLimit keys market quantity rate =
  callAPI defOpts {
      path      = "selllimit"
    , keys      = keys
    , apiType   = MarketAPI
    , qParams   = [ ("market", camelToDash $ show market )
                  , ("quantity", show quantity )
                  , ("rate", show rate )
                  ]
    }

-- | Used to cancel a buy or sell order.
cancel
  :: APIKeys -- ^ Bittrex API credentials
  -> UUID    -- ^ uuid of buy or sell order
  -> IO (Either ErrorMessage (Maybe Text))
cancel keys (UUID uuid) =
  callAPI defOpts {
      path      = "cancel"
    , keys      = keys
    , apiType   = MarketAPI
    , qParams   = [ ("uuid", T.unpack uuid ) ]
    }

-- | Used to retrieve all balances from your account
getBalances
  :: APIKeys -- ^ Bittrex API credentials
  -> IO (Either ErrorMessage [Balance])
getBalances keys =
  callAPI defOpts {
      path    = "getbalances"
    , keys    = keys
    , apiType = AccountAPI
    }

-- | Used to retrieve the balance from your account for a specific currency.
getBalance
  :: APIKeys -- ^ Bittrex API credentials
  -> CurrencyName -- ^ a string literal for the currency (ex: LTC)
  -> IO (Either ErrorMessage Balance)
getBalance keys currency =
  callAPI defOpts {
      path    = "getbalance"
    , keys    = keys
    , apiType = AccountAPI
    , qParams = pure ("currency", T.unpack currency )
    }

-- | Used to retrieve or generate an address for a specific currency.
-- If one does not exist, the call will fail and return ADDRESS_GENERATING until one is available.
getDepositAddress
  :: APIKeys -- ^ Bittrex API credentials
  -> CurrencyName -- ^ a string literal for the currency (ie. BTC)
  -> IO (Either ErrorMessage DepositAddress)
getDepositAddress keys currency =
  callAPI defOpts {
      path    = "getdepositaddress"
    , apiType = AccountAPI
    , keys    = keys
    , qParams = pure ("currency", T.unpack currency )
    }

-- | Used to retrieve your withdrawal history.
getWithdrawalHistory
  :: APIKeys -- ^ Bittrex API credentials
  -> CurrencyName
  -> IO (Either ErrorMessage [WithdrawalHistory])
getWithdrawalHistory keys currency =
  callAPI defOpts {
      path    = "getwithdrawalhistory"
    , apiType = AccountAPI
    , keys    = keys
    , qParams = pure ( "currency"
                     , show currency
                     )
    }

-- | Used to retrieve your order history.
getOrderHistory
  :: APIKeys -- ^ Bittrex API credentials
  -> Maybe MarketName -- ^ a string literal for the market (ie. BTC-LTC). If ommited, will return for all markets
  -> IO (Either ErrorMessage [OrderHistory] )
getOrderHistory keys market =
  callAPI defOpts {
      path    = "getorderhistory"
    , apiType = AccountAPI
    , keys    = keys
    , qParams = [ ("market", camelToDash (show m) )
                | Just m <- pure market
                ]
    }

-- | Used to retrieve a single order by uuid.
getOrder
  :: APIKeys -- ^ Bittrex API credentials
  -> UUID    -- ^ the uuid of the buy or sell order
  -> IO (Either ErrorMessage Order)
getOrder keys (UUID uuid) =
  callAPI defOpts {
      path = "getorder"
    , keys = keys
    , apiType = AccountAPI
    , qParams   = [ ("uuid", T.unpack uuid) ]
    }

-- | Used to withdraw funds from your account. note: please account for txfee.
withdraw
  :: APIKeys         -- ^ Bittrex API credentials
  -> CurrencyName    -- ^ A string literal for the currency (ie. BTC)
  -> Quantity        -- ^ The quantity of coins to withdraw
  -> Address         -- ^ The address where to send the funds.
  -> Maybe PaymentId -- ^ used for CryptoNotes/BitShareX/Nxt optional field (memo/paymentid)
  -> IO (Either ErrorMessage UUID)
withdraw keys currency quantity address payment =
  callAPI defOpts {
      path      = "withdraw"
    , keys      = keys
    , apiType   = AccountAPI
    , qParams   = [ ("currency", T.unpack currency )
                  , ("quantity", show quantity )
                  , ("address", address )
                  ] <> [ ("paymentid", show p )
                       | Just p <- pure payment
                       ]
    }

-- | Used to retrieve your deposit history.
getDepositHistory
  :: APIKeys
  -- ^ Bittrex API credentials
  -> Maybe CurrencyName
  -- ^ A string literal for the currecy (ie. BTC). If `Nothing`, will return for all currencies
  -> IO (Either ErrorMessage [DepositHistory])
getDepositHistory keys currency =
  callAPI defOpts {
      path = "getdeposithistory"
    , apiType = AccountAPI
    , keys = keys
    , qParams   = [ ("currency", show c)
                  | Just c <- pure currency
                  ]
    }
