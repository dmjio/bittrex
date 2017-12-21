module Bittrex.Util ( camelToDash ) where

camelToDash :: String -> String
camelToDash [] = []
camelToDash ('_':xs) = '-':camelToDash xs
camelToDash (x:xs) = x:camelToDash xs
