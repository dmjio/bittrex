module Bittrex.Util ( toMarket ) where

import           Data.Text        (Text)
import qualified Data.Text        as T

import           Bittrex.Types

camelToDash :: String -> String
camelToDash [] = []
camelToDash ('_':xs) = '-':camelToDash xs
camelToDash (x:xs) = x:camelToDash xs

toMarket :: MarketName -> String
toMarket (NewMarket t) = T.unpack t
toMarket (MarketName k) = camelToDash (show k)
