--------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

--------------------------------------------------------------------------------

module Bittrex.Internal where

--------------------------------------------------------------------------------

import           Bittrex.Types
import qualified Control.Lens          as Lens
import           Data.Aeson
import           Data.Aeson.Lens       (key, nth)
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Lazy  as LBS
import           Data.Char             (toLower)
import           Data.Digest.Pure.SHA
import           Data.List             (intercalate)
import           Data.List.Split       (splitOn)
import           Data.Monoid
import qualified Data.Text             as Text
import           Data.Time.Clock.POSIX
import           Debug.Trace
import           Flow                  ((.>), (|>))
import           Network.Wreq

--------------------------------------------------------------------------------

-- | Default API options
defOpts :: APIOpts
defOpts = APIOpts PublicAPI [] "v1.1" mempty (APIKeys mempty mempty)

-- | Internal, used for dispatching an API call
callAPI
  :: (FromJSON v)
  => APIOpts
  -> IO (Either ErrorMessage v)
callAPI (APIOpts {..}) = do
  let (APIKeys {..}) = apiOptsKeys
  nonce <- head . splitOn "." . show <$> getPOSIXTime
  let addAuth = apiOptsAPIType `elem` [AccountAPI, MarketAPI]
  let authParams = [ [("apikey", apiKey) | addAuth]
                   , [("nonce", nonce)   | addAuth]
                   ] |> mconcat
  let url = [ "https://bittrex.com/api"
            , Text.unpack apiOptsVersion
            , toLower <$> show apiOptsAPIType
            , Text.unpack apiOptsPath
            ] |> intercalate "/"
  let go (k, v) = k <> "=" <> v <> "&"
  let urlForHash = [ url, "?", go =<< do apiOptsQueryParams ++ authParams
                   ] |> mconcat |> init
  let addHeader o = let secretLBS = LBS.fromStrict (BSC8.pack secretKey)
                        apisign = BSC8.pack
                                  $ showDigest
                                  $ hmacSha512 secretLBS
                                  $ LBS.fromStrict (BSC8.pack urlForHash)
                    in if addAuth
                       then o |> Lens.set (header "apisign") [apisign]
                       else o
  r <- getWith (addHeader defaults) urlForHash
  let Just (Bool success) = r |> Lens.preview (responseBody . key "success")
  let Just result = r |> Lens.preview (responseBody . key "result")
  let Just msg = r |> Lens.preview (responseBody . key "message")
  pure $ if success
         then case fromJSON result of
                Error s   -> Left (DecodeFailure s result)
                Success m -> Right m
         else case fromJSON msg of
                Error s   -> Left (DecodeFailure s msg)
                Success m -> Left (BittrexError m)

--------------------------------------------------------------------------------
