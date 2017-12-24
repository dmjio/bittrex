bittrex
==============================
![Hackage](https://img.shields.io/hackage/v/bittrex.svg)
![Hackage Dependencies](https://img.shields.io/hackage-deps/v/bittrex.svg)
![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)
![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)
[![Build Status](https://travis-ci.org/dmjio/bittrex.svg?branch=master)](https://travis-ci.org/dmjio/bittrex)

# About

Haskell bindings to the [Bittrex](https://bittrex.com/) cryptocurrency exchange.

# Usage

```haskell
module Main where

import Bittrex
import Control.Monad

main :: IO ()
main = do
  [api,secret] <- lines <$> readFile "/Users/david/.bittrex"
  let keys = APIKeys api secret

  -- Public Usage
  putStrLn "Markets"
  Right ms <- getMarkets
  forM_ ms $ \m ->
    print (marketName m)

  putStrLn "Currencies"
  Right cs <- getCurrencies
  mapM_ print cs

  putStrLn "Ticker for BTC-DOGE market"
  t <- getTicker BTC_DOGE
  print t

  putStrLn "Summary of BTC-DOGE market"
  Right t <- getMarketSummary BTC_DOGE
  print t

  putStrLn "Order book for BTC-DOGE market"
  book <- getOrderBook BTC_DOGE Both
  print book

  putStrLn "Market history for BTC-DOGE"
  Right history <- getMarketHistory BTC_DOGE
  forM_ history print

  -- Market usage
  putStrLn "Retrieve your open orders"
  Right openOrders <- getOpenOrders keys BTC_DOGE
  forM_ openOrders print

  -- Account usage,
  putStrLn "Check your balances (for all currencies)"
  Right balances <- getBalances keys
  forM_ balances print
```

# License

[BSD3](LICENSE) © David Johnson
