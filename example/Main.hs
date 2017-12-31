module Main where

import           Bittrex
import           Control.Monad
import           Data.Function      ((&))
import           Data.Maybe
import           Data.Text          (Text)
import qualified Data.Text.IO       as T
import           System.Environment (getEnv, lookupEnv)

main :: IO ()
main = do
  -- Get Bittrex API credentials

  homeDirectory <- getEnv "HOME"
  credentialFile <- fromMaybe (homeDirectory ++ "/.bittrex")
                    <$> lookupEnv "BITTREX_CREDENTIAL_FILE"
  [api, secret] <- lines <$> readFile credentialFile
  let keys = APIKeys api secret

  -- Define helper functions

  let putHeader x = putStrLn ("\ESC[1m" ++ x ++ "\ESC[0m")
  let printInd x = let indent = "    "
                       wrap line = indent ++ "\ESC[34m" ++ line ++ "\ESC[0m"
                       shown = unlines $ map wrap $ lines $ show x
                   in putStrLn shown

  -- Public API example

  putHeader "Markets"
  Right ms <- getMarkets
  forM_ ms (printInd . marketName)

  putHeader "Currencies"
  Right cs <- getCurrencies
  mapM_ printInd cs

  putHeader "Ticker for BTC-DOGE market"
  t <- getTicker (MarketName BTC_DOGE)
  printInd t

  putHeader "Summary of BTC-DOGE market"
  Right t <- getMarketSummary (MarketName BTC_DOGE)
  printInd t

  putHeader "Get market summaries"
  Right t <- getMarketSummaries
  printInd t

  putHeader "Order book sells for for BTC-DOGE market"
  book <- getOrderBookSells (MarketName BTC_DOGE)
  printInd book

  putHeader "Order book buys for for BTC-DOGE market"
  book <- getOrderBookBuys (MarketName BTC_DOGE)
  printInd book

  putHeader "Market history for BTC-DOGE"
  Right history <- getMarketHistory (MarketName BTC_DOGE)
  forM_ history printInd

  -- Market API example

  putHeader "Retrieve your open orders"
  Right openOrders <- getOpenOrders keys (MarketName BTC_DOGE)
  forM_ openOrders printInd

  -- Account API example

  putHeader "Check your balances (for all currencies)"
  Right balances <- getBalances keys
  forM_ balances printInd
