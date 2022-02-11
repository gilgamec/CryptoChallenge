{-# LANGUAGE OverloadedStrings #-}

module TimingTests where

import Bytes ( HasBytes(..), convBytes )
import Bytes.Hex ( mkHex, showHex )
import Random ( randomBytes )
import Hash ( sha1Hash, MAC(..), SHA1MAC, mkHMACSHA1, validateHMACSHA1 )
import Timing ( TimingOracle, TimingResponse(..), getHiresTime
              , insecure_compare )

import Challenge31 ( simpleTimingAttack )

import Test.Framework
import TestFramework

import Control.Concurrent ( forkIO, killThread )
import Control.Exception ( bracket, evaluate )
import Control.DeepSeq ( force )

import qualified Network.Wai as WS
import qualified Network.Wai.Handler.Warp as WS
import qualified Network.HTTP.Client as WC
import qualified Network.HTTP.Types as HTTP

-- This module defines the testing apparatus for the timing attacks
-- explored in Challenges 31 and 32.
-- This consists of a webserver validating requests authenticated by HMAC.
-- However, the comparison of the HMAC signatures
-- uses `insecure_compare` from the `Timing` module,
-- allowing us to insert a tunable delay between byte comparisons
-- and thus exposing response timing as an attack vector.

-- The webserver listens on port 31337.
serverPort :: Int
serverPort = 31337

-- The webserver takes request of the form
--   http://localhost:31337/check?file=filename.txt&signature=hexcode
-- and checks that the HMAC for filename.txt is hexcode, using insecure_compare.
-- If valid, it returns status 200; otherwise, it returns status 500.
webserver :: HasBytes key => key -> Int -> IO ()
webserver key delay = WS.run serverPort webapp
 where
  hmac = toBytes . macHash . mkHMACSHA1 key
  icompare = insecure_compare delay
  webapp request respond = do
    let queryString = WS.queryString request  -- nb. map from Bytes to Maybe Bytes
        filename = maybe "" id $ lookup "file" queryString >>= id
        sig = maybe "" toBytes $ lookup "signature" queryString >>= id >>= mkHex . convBytes
    valid <- hmac filename `icompare` sig
    respond $ if valid
              then WS.responseLBS HTTP.status200 [] "Signature accepted."
              else WS.responseLBS HTTP.status500 [] "Signature not valid."

-- The function `makeURL` builds the URL corresponding to
-- a request for the given HMAC.
makeURL :: SHA1MAC FilePath -> String
makeURL MAC{ macMessage = filename, macHash = hash } = concat
  [ "http://localhost:", show serverPort, "/check?"
  , "file=", filename
  , "&signature=", showHex hash ]

-- The function `makeQuery` queries the webserver with the given URL,
-- then reports the response code (200 or 500).
makeQuery :: WC.Manager -> String -> IO HTTP.Status
makeQuery manager url = do
  request <- WC.parseRequest url
  response <- WC.httpLbs request manager
  pure (WC.responseStatus response)

-- The function `timeRequest` calls `makeQuery`, timing its execution.
timeRequest :: WC.Manager -> TimingOracle IO (SHA1MAC FilePath)
timeRequest manager mac = do
  -- We force the creation of the URL so we're timing webserver response,
  -- not bytestring manipulation.
  url <- evaluate (force $ makeURL mac)
  t1 <- getHiresTime
  s <- makeQuery manager url
  if s == HTTP.status200
    then pure RequestSucceeded
    else RequestFailed . subtract t1 <$> getHiresTime

-- Now that we have our oracle, we can perform a timing attack.
-- The function 'timingTest' starts a web server with a given comparison delay
-- and runs a given timing attack on it.
timingTest :: Int
           -> (TimingOracle IO (SHA1MAC FilePath) -> FilePath ->
               IO (SHA1MAC FilePath))
           -> Assertion
timingTest delay attack = do
  key <- randomBytes 16
  let targetFile = "/etc/ssh/id_rsa"
  putStrLn $ "Valid hash is "++showHex (macHash $ mkHMACSHA1 key targetFile)
  manager <- WC.newManager WC.defaultManagerSettings
  mac <- bracket (forkIO $ webserver key delay) killThread $ const $
           attack (timeRequest manager) targetFile
  putStrLn $ "Computed hash is "++showHex (macHash mac)
  validateHMACSHA1 key mac @? "timing attack unsuccessful"

-- The actual tests performed.
main = flip defaultMainWithArgs ["--threads=1"]
  [ testGroup "Challenge 31"
    [ testCase "webserver correctly distinguishes valid from invalid MACs" $ do
        key <- randomBytes 16
        let filename = "/etc/ssh/id_rsa"
            validMAC = mkHMACSHA1 key filename
            invalidMAC = MAC{ macMessage = filename
                            , macHash = sha1Hash ("FAKE SIGNATURE" :: String) }
            delay = 0
        manager <- WC.newManager WC.defaultManagerSettings
        bracket (forkIO $ webserver key delay) killThread $ const $ do
          makeQuery manager (makeURL validMAC) >>= (@?= HTTP.status200)
          makeQuery manager (makeURL invalidMAC) >>= (@?= HTTP.status500)
    -- As suggested in the Challenge, we use a 50ms delay.
    -- This works fine, but takes about 40 minutes.
    , testCase "simple timing attack works with delay of 50ms" $
        timingTest 50000 simpleTimingAttack
    ]
  ]
