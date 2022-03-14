#!/usr/bin/env stack
-- stack --resolver snapshot.yaml script

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM, when)
import Data.Extensible
import qualified Data.List as List
import Data.Maybe (listToMaybe)
import Data.String (fromString)
import qualified Data.Yaml as YAML
import GHC.Generics (Generic)
import GHC.OverloadedLabels (fromLabel)
import Lens.Micro ((^.))
import Network.HTTP.Req (runReq, (/:), req)
import qualified Network.HTTP.Req as Req
import System.Environment (getArgs)
import System.Exit (exitFailure)

type Snapshot = Record 
 '[ "packages" >: [String]
  ]

type PackageVersions = Record
 '[ "normal-version" >: [String]
  ]

main :: IO ()
main = listToMaybe <$> getArgs >>= \case
  Nothing -> 
    fail "please input snapshot path"
  Just path -> do
    snapshot <- YAML.decodeFileThrow path :: IO Snapshot
    haveUpdates <- forM (snapshot ^. #packages) $ \package -> do
      let (nameAndVersion, _) = List.span (/= '@') package
          packageName = reverse $ List.drop 1 $ List.dropWhile (/= '-') $ reverse nameAndVersion
      latestVersion <- fetchLatestVersion packageName
      if packageName `elem` ignores || nameAndVersion == (packageName ++ "-" ++ latestVersion) then
        pure False
      else do
        putStrLn $ packageName ++ " have new version: " ++ latestVersion
        pure True
    when (or haveUpdates) $
      exitFailure
  where
    ignores = 
      [ "hashable"
      , "mmorph"
      ]

fetchLatestVersion :: String -> IO String
fetchLatestVersion packageName = 
  runReq Req.defaultHttpConfig $ do
    resp <- req Req.GET url Req.NoReqBody Req.jsonResponse opt
    let versions = Req.responseBody resp :: PackageVersions
    pure $ head (versions ^. fromLabel @"normal-version")
  where
    url = Req.https "hackage.haskell.org" /: "package" /: fromString packageName /: "preferred"
    opt = Req.header "accept" "application/json"
