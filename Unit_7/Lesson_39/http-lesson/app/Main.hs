{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as L
import Data.ByteString.Lazy.Char8 qualified as LC
import Network.HTTP.Simple
import Network.HTTP.Types

-- Q39.2
main :: IO ()
main = do
  response <- httpLBS request
  let status = getResponseStatus response
  if (statusCode status) == 200
    then do
      print "saving request to file"
      let jsonBody = getResponseBody response
      L.writeFile "data.json" jsonBody
    else do print "request failed with error"
            print (statusCode status)
            print (statusMessage status)

myToken :: BC.ByteString
myToken = "ISvqAgPrdPJkkJrXhhbiEXLjssNTGSAe"

noaaHost :: BC.ByteString
noaaHost = "www.ncdc.noaa.gov"

apiPath :: BC.ByteString
apiPath = "/cdo-web/api/v2/datasets"

buildRequest :: BC.ByteString -> BC.ByteString -> BC.ByteString -> BC.ByteString -> Request
buildRequest token host method path =
  setRequestMethod method $
    setRequestHost host $
      setRequestHeader "token" [token] $
        setRequestPath path $
          setRequestSecure True $
            setRequestPort 443 $
              defaultRequest

request :: Request
request = buildRequest myToken noaaHost "GET" apiPath

-- Q39.1
buildRequestNOSSL :: BC.ByteString -> BC.ByteString -> BC.ByteString -> BC.ByteString -> Request
buildRequestNOSSL token host method path =
  setRequestMethod method $
    setRequestHost host $
      setRequestHeader "token" [token] $
        setRequestPath path $
          setRequestSecure False $
            setRequestPort 443 $
              defaultRequest

