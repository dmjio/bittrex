{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Bittrex.Internal where

import           Bittrex.Types
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens       (key, nth)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy  as L
import           Data.Char
import           Data.Digest.Pure.SHA
import           Data.List
import           Data.List.Split       (splitOn)
import           Data.Monoid
import           Data.Time.Clock.POSIX
import           Debug.Trace
import           Network.Wreq

-- | Default API options
defOpts :: APIOpts
defOpts = APIOpts PublicAPI [] "v1.1" mempty (APIKeys mempty mempty)

-- | Internal, used for dispatching an API call
callAPI
  :: FromJSON v
  => APIOpts
  -> IO (Either ErrorMessage v)
callAPI APIOpts {..} = do
  let APIKeys {..} = keys
  nonce <- head . splitOn "." . show <$> getPOSIXTime
  let addAuth = apiType `elem` [AccountAPI, MarketAPI]
      authParams = concat
        [ [ ("apikey", apiKey) | addAuth ]
        , [ ("nonce", nonce)   | addAuth ]
        ]
      addHeader o =
          if addAuth
            then o & header "apisign" .~ [
               BC.pack $ showDigest $
                 hmacSha512 (L.fromStrict (BC.pack secretKey)) $
                   L.fromStrict (BC.pack urlForHash)
               ]
            else o
      urlForHash = init $
        mconcat [ url
                , "?"
                , go =<< do qParams ++ authParams
                ]
      go (k,v) = k <> "=" <> v <> "&"
      url = intercalate "/" [ "https://bittrex.com/api"
                            , version
                            , toLower <$> show apiType
                            , path
                            ]
  r <- getWith (addHeader defaults) urlForHash
  let Just (Bool success) = r ^? responseBody . key "success"
      Just result = r ^? responseBody . key "result"
      Just msg = r ^? responseBody . key "message"
  pure $ if success
           then case fromJSON result of
                  Error s   -> Left (DecodeFailure s result)
                  Success m -> Right m
           else case fromJSON msg of
                  Success m -> Left (BittrexError m)
                  Error s   -> Left (DecodeFailure s msg)

