--------------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

--------------------------------------------------------------------------------

module Bittrex.API
  ( -- * Overview
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

--------------------------------------------------------------------------------

import           Data.Aeson
import           Data.Char
import           Data.Monoid
import           Data.Scientific
import           Data.Text        (Text)
import qualified Data.Text        as Text

import           Bittrex.Internal
import           Bittrex.Types
import           Bittrex.Util

--------------------------------------------------------------------------------

-- $intro
-- Bittrex provides a simple and powerful REST API to allow you to
-- programmatically perform nearly all actions you can from our web interface.
--
-- All requests use the @application/json@ content type and go over HTTPS.
-- The base url is <https://bittrex.com/api/{version}/>.
--
-- All requests are GET requests and all responses come in a default response
-- object with the result in the result field. Always check the success flag to
-- ensure that your API call succeeded.
--
-- We are currently restricting orders to 500 open orders and 200,000 orders
-- a day. We reserve the right to change these settings as we tune the system.
-- If you are affected by these limits as an active trader, please email
-- <mailto:support@bittrex.com support@bittrex.com>.
--
-- If you have any questions, feedback or recommendation for API support you
-- can post a question in our support center.

--------------------------------------------------------------------------------

-- | Used to get the open and available trading markets at Bittrex along with
--   other meta data.
getMarkets
  :: IO (Either ErrorMessage [Market])
getMarkets
  = callAPI (defOpts { apiOptsPath = "getmarkets" })

--------------------------------------------------------------------------------

-- | Used to get all supported currencies at Bittrex along with other meta data.
getCurrencies
  :: IO (Either ErrorMessage [Currency])
getCurrencies
  = callAPI (defOpts { apiOptsPath = "getcurrencies" })

--------------------------------------------------------------------------------

-- | Used to get the current tick values for a market.
getTicker
  :: MarketName
  -- ^ A string literal for the market (e.g.: @BTC-LTC@).
  -> IO (Either ErrorMessage Ticker)
getTicker market
  = callAPI (defOpts { apiOptsQueryParams = [("market", toMarket market)]
                     , apiOptsPath        = "getticker"
                     })

--------------------------------------------------------------------------------

-- | This function returns a summary of the last 24 hours for all
--   active exchanges.
getMarketSummaries
  :: IO (Either ErrorMessage [MarketSummary])
getMarketSummaries
  = callAPI (defOpts { apiOptsPath = "getmarketsummaries" })

--------------------------------------------------------------------------------

-- | This function returns a summary of the last 24 hours for the given market.
getMarketSummary
  :: MarketName
  -- ^ A string literal for the market (e.g.: @BTC-LTC@).
  -> IO (Either ErrorMessage [MarketSummary])
getMarketSummary market
  = callAPI (defOpts { apiOptsQueryParams = pure ("market", toMarket market)
                     , apiOptsPath        = "getmarketsummary"
                     })

--------------------------------------------------------------------------------

-- | Used to get retrieve the orderbook for a given market
getOrderBookBuys
  :: MarketName
  -- ^ A string literal for the market (e.g.: @BTC-LTC@).
  -- -> OrderBookType
  -- -- ^ buy, sell or both to identify the type of orderbook to return.
  -> IO (Either ErrorMessage [OrderBookEntry])
getOrderBookBuys market
  = callAPI (defOpts { apiOptsPath        = "getorderbook"
                     , apiOptsQueryParams = [ ("market", toMarket market)
                                            , ("type", "buy")
                                            ]
                     })

--------------------------------------------------------------------------------

-- | Used to get retrieve the orderbook for a given market
getOrderBookSells
  :: MarketName
  -- ^ A string literal for the market (e.g.: @BTC-LTC@).
  -- -> OrderBookType
  -- -- ^ buy, sell or both to identify the type of orderbook to return.
  -> IO (Either ErrorMessage [OrderBookEntry])
getOrderBookSells market
  = callAPI (defOpts { apiOptsPath        = "getorderbook"
                     , apiOptsQueryParams = [ ("market", toMarket market)
                                            , ("type", "sell")
                                            ]
                     })

-- | Used to retrieve the latest trades that have occured for a specific market.
getMarketHistory
  :: MarketName
  -- ^ A string literal for the market (e.g.: @BTC-LTC@).
  -> IO (Either ErrorMessage [MarketHistory])
getMarketHistory market
  = callAPI (defOpts { apiOptsPath        = "getmarkethistory"
                     , apiOptsQueryParams = pure ("market", toMarket market)
                     })

-- | Get all orders that you currently have opened.
--   A specific market can be requested using the 'MarketName' parameter.
getOpenOrders
  :: APIKeys
  -- ^ Your Bittrex API credentials.
  -> MarketName
  -- ^ String literal for the market (e.g.: @BTC-LTC@).
  -> IO (Either ErrorMessage [OpenOrder])
getOpenOrders keys market
  = callAPI (defOpts { apiOptsPath        = "getopenorders"
                     , apiOptsAPIType     = MarketAPI
                     , apiOptsQueryParams = pure ("market", toMarket market)
                     , apiOptsKeys        = keys
                     })

-- | Used to place a buy order in a specific market. Use buylimit to place limit orders. Make sure you have the proper permissions set on your API keys for this call to work
buyLimit
  :: APIKeys
  -- ^ Your Bittrex API credentials.
  -> MarketName
  -- ^ A string literal for the market (e.g.: @BTC-LTC@).
  -> Quantity
  -- ^ The amount to purchase.
  -> Rate
  -- ^ The rate at which to place the order.
  -> IO (Either ErrorMessage UUID)
buyLimit keys market quantity rate
  = callAPI (defOpts { apiOptsPath        = "buylimit"
                     , apiOptsKeys        = keys
                     , apiOptsAPIType     = MarketAPI
                     , apiOptsQueryParams = [ ("market", toMarket market)
                                            , ("quantity", show quantity)
                                            , ("rate", show rate)
                                            ]
                     })

-- | Used to place an sell order in a specific market. Use selllimit to place limit orders.
-- Make sure you have the proper permissions set on your API keys for this call to workn
sellLimit
  :: APIKeys
  -- ^ Your Bittrex API credentials.
  -> MarketName
  -- ^ A string literal for the market (e.g.: @BTC-LTC@)
  -> Quantity
  -- ^ The amount to purchase
  -> Rate
  -- ^ The rate at which to place the order
  -> IO (Either ErrorMessage UUID)
sellLimit keys market quantity rate
  = callAPI (defOpts { apiOptsPath        = "selllimit"
                     , apiOptsKeys        = keys
                     , apiOptsAPIType     = MarketAPI
                     , apiOptsQueryParams = [ ("market", toMarket market)
                                            , ("quantity", show quantity)
                                            , ("rate", show rate)
                                            ]
                     })

-- | Used to cancel a buy or sell order.
cancel
  :: APIKeys
  -- ^ Your Bittrex API credentials.
  -> UUID
  -- ^ The UUID of the buy or sell order.
  -> IO (Either ErrorMessage (Maybe Text))
cancel keys (UUID uuid)
  = callAPI (defOpts { apiOptsPath        = "cancel"
                     , apiOptsKeys        = keys
                     , apiOptsAPIType     = MarketAPI
                     , apiOptsQueryParams = [ ("uuid", Text.unpack uuid) ]
                     })

-- | Used to retrieve all balances from your account
getBalances
  :: APIKeys
  -- ^ Your Bittrex API credentials.
  -> IO (Either ErrorMessage [Balance])
getBalances keys
  = callAPI (defOpts { apiOptsPath    = "getbalances"
                     , apiOptsKeys    = keys
                     , apiOptsAPIType = AccountAPI
                     })

-- | Used to retrieve the balance from your account for a specific currency.
getBalance
  :: APIKeys
  -- ^ Your Bittrex API credentials.
  -> CurrencyName
  -- ^ A string literal for the currency (e.g.: @LTC@).
  -> IO (Either ErrorMessage Balance)
getBalance keys curr
  = callAPI (defOpts { apiOptsPath        = "getbalance"
                     , apiOptsKeys        = keys
                     , apiOptsAPIType     = AccountAPI
                     , apiOptsQueryParams = pure ("currency", Text.unpack curr)
                     })

-- | Used to retrieve or generate an address for a specific currency.
--   If one does not exist, the call will fail and return 'ADDRESS_GENERATING'
--   until one is available.
getDepositAddress
  :: APIKeys
  -- ^ Your Bittrex API credentials.
  -> CurrencyName
  -- ^ A string literal for the currency (e.g.: @BTC@).
  -> IO (Either ErrorMessage DepositAddress)
getDepositAddress keys curr
  = callAPI (defOpts { apiOptsPath        = "getdepositaddress"
                     , apiOptsAPIType     = AccountAPI
                     , apiOptsKeys        = keys
                     , apiOptsQueryParams = pure ("currency", Text.unpack curr)
                     })

-- | Used to retrieve your withdrawal history.
getWithdrawalHistory
  :: APIKeys
  -- ^ Your Bittrex API credentials.
  -> CurrencyName
  -- ^ A string literal for the currency (e.g.: @BTC@).
  -> IO (Either ErrorMessage [WithdrawalHistory])
getWithdrawalHistory keys curr
  = callAPI (defOpts { apiOptsPath        = "getwithdrawalhistory"
                     , apiOptsAPIType     = AccountAPI
                     , apiOptsKeys        = keys
                     , apiOptsQueryParams = pure ("currency", show curr)
                     })

-- | Used to retrieve your order history.
getOrderHistory
  :: APIKeys
  -- ^ Your Bittrex API credentials.
  -> Maybe MarketName
  -- ^ A string literal for the market (e.g.: @BTC-LTC@).
  --   If this is 'Nothing', all markets will be returned.
  -> IO (Either ErrorMessage [OrderHistory])
getOrderHistory keys market
  = callAPI (defOpts { apiOptsPath        = "getorderhistory"
                     , apiOptsAPIType     = AccountAPI
                     , apiOptsKeys        = keys
                     , apiOptsQueryParams = [ ("market", toMarket m)
                                            | Just m <- pure market ]
                     })

-- | Used to retrieve a single order by uuid.
getOrder
  :: APIKeys
  -- ^ Your Bittrex API credentials.
  -> UUID
  -- ^ The UUID of the buy or sell order.
  -> IO (Either ErrorMessage Order)
getOrder keys (UUID uuid)
  = callAPI (defOpts { apiOptsPath        = "getorder"
                     , apiOptsKeys        = keys
                     , apiOptsAPIType     = AccountAPI
                     , apiOptsQueryParams = [ ("uuid", Text.unpack uuid) ]
                     })

-- | Used to withdraw funds from your account. note: please account for txfee.
withdraw
  :: APIKeys
  -- ^ Your Bittrex API credentials.
  -> CurrencyName
  -- ^ A string literal for the currency (e.g.: @BTC@).
  -> Quantity
  -- ^ The quantity of coins to withdraw
  -> Address
  -- ^ The address where to send the funds.
  -> Maybe PaymentId
  -- ^ used for the optional field representing the memo / payment ID on
  --   CryptoNotes, BitShareX, and Nxt transactions.
  -> IO (Either ErrorMessage UUID)
withdraw keys curr quantity address payment
  = callAPI (defOpts { apiOptsPath        = "withdraw"
                     , apiOptsKeys        = keys
                     , apiOptsAPIType     = AccountAPI
                     , apiOptsQueryParams = [ ("currency", Text.unpack curr)
                                            , ("quantity", show quantity)
                                            , ("address", address)
                                            ] <> [ ("paymentid", show p)
                                                 | Just p <- pure payment
                                                 ]
                     })

-- | Used to retrieve your deposit history.
getDepositHistory
  :: APIKeys
  -- ^ Your Bittrex API credentials.
  -> Maybe CurrencyName
  -- ^ A string literal for the currency (e.g.: @BTC@).
  --   If this is 'Nothing', all currencies will be returned.
  -> IO (Either ErrorMessage [DepositHistory])
getDepositHistory keys curr
  = callAPI (defOpts { apiOptsPath        = "getdeposithistory"
                     , apiOptsAPIType     = AccountAPI
                     , apiOptsKeys        = keys
                     , apiOptsQueryParams = [ ("currency", show c)
                                            | Just c <- pure curr
                                            ]
                     })

--------------------------------------------------------------------------------
