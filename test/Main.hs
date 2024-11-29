module Main (main) where

import           Prelude                                  (Bool (True), IO, ($))
import           Server                                   (Context, InputData, K, N, PlonkKYC, kycData, verifyKYCData)
import           Test.Tasty                               (defaultMain)
import           Test.Tasty.HUnit                         (assertEqual, testCase)

import           ZkFold.Base.Protocol.NonInteractiveProof (HaskellCore, verify)
import           ZkFold.Prelude                           (readFileJSON)

main :: IO ()
main = defaultMain $
    testCase "KYC example test case" $ do
    json <- readFileJSON "example-json/kyc-data.json" :: IO (InputData N K Context)
    let !(s, i, p) = verifyKYCData (kycData json)
    assertEqual "Verify result must be true" True $ verify @(PlonkKYC 29 512) @HaskellCore s i p
