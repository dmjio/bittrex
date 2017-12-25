module Bittrex.Util ( camelToDash, parse ) where

import Data.Time
import Data.Time.Format

camelToDash :: String -> String
camelToDash [] = []
camelToDash ('_':xs) = '-':camelToDash xs
camelToDash (x:xs) = x:camelToDash xs

parse :: String -> UTCTime
parse =
  parseTimeOrError True defaultTimeLocale $
    iso8601DateFormat (Just "%H:%M:%S%Q")
