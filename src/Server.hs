{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Server where

import           Data.Aeson                                  (FromJSON, ToJSON)
import qualified Data.ByteString                             as BS
import           GHC.Generics                                (Generic, Par1, U1 (..), unPar1, (:*:) (..))
import           GHC.TypeNats
import           Prelude                                     (Int, error, return, undefined, ($), (.), (<$>))
import           RustFunctions
import           Servant                                     (Handler, JSON, Post, ReqBody, Server, type (:>))
import           System.IO.Unsafe
import           Test.QuickCheck                             (arbitrary, generate)

import           ZkFold.Base.Algebra.Basic.Class             (FromConstant (..), ToConstant (..))
import           ZkFold.Base.Algebra.Basic.Field
import           ZkFold.Base.Algebra.EllipticCurve.BLS12_381
import           ZkFold.Base.Algebra.EllipticCurve.Class     (EllipticCurve (ScalarField), Point)
import           ZkFold.Base.Data.HFunctor                   (hmap)
import           ZkFold.Base.Data.Vector
import           ZkFold.Base.Protocol.NonInteractiveProof    (NonInteractiveProof (Input, Proof, SetupVerify), prove,
                                                              setupProve, setupVerify)
import           ZkFold.Base.Protocol.Plonk                  (Plonk (Plonk))
import           ZkFold.Base.Protocol.Plonkup.Input          (PlonkupInput (..))
import           ZkFold.Base.Protocol.Plonkup.Proof          (PlonkupProof (..))
import           ZkFold.Base.Protocol.Plonkup.Prover         (PlonkupProverSecret)
import           ZkFold.Base.Protocol.Plonkup.Utils          (getParams)
import           ZkFold.Base.Protocol.Plonkup.Witness        (PlonkupWitnessInput (PlonkupWitnessInput))
import           ZkFold.Symbolic.Apps.KYC                    (KYCData (KYCData), kycExample)
import           ZkFold.Symbolic.Compiler
import           ZkFold.Symbolic.Data.ByteString             hiding (append, concat)
import           ZkFold.Symbolic.Data.Combinators            (Iso (..), RegisterSize (Auto))
import           ZkFold.Symbolic.Interpreter                 (Interpreter)

type N = 18

type K = 8

type F = Zp BLS12_381_Scalar

type Context = (Interpreter F)

data InputData n k c = InputData
  { statementId :: Int,
    kycData     :: KYCData n k Auto c
  }
  deriving (Generic)

instance FromJSON (InputData N K Context)

instance ToJSON (InputData N K Context)

data OutputData = OutputData
  { input :: Input (PlonkKYC 29 512),
    proof :: Proof (PlonkKYC 29 512)
  }
  deriving (Generic)

deriving instance Generic (PlonkupInput (Vector 1) c)

deriving instance Generic (PlonkupProof c)

instance (ToJSON (ScalarField c), ToJSON (Point c)) => ToJSON (PlonkupProof c)

instance (ToJSON (ScalarField c), ToJSON (Point c)) => ToJSON (PlonkupInput (Vector 1) c)

instance (ToJSON (Point BLS12_381_G1)) => ToJSON OutputData

type API = "prove" :> ReqBody '[JSON] (InputData N K Context) :> Post '[JSON] OutputData

type PlonkKYC i n = Plonk (((U1 :*: U1) :*: (U1 :*: U1)) :*: (U1 :*: U1)) (Vector i) n (Vector 1) BLS12_381_G1 BLS12_381_G2 BS.ByteString

kycCheckVerification :: Vector 29 (ScalarField BLS12_381_G1)
                     -> Fr
                     -> PlonkupProverSecret BLS12_381_G1
                     -> (SetupVerify (PlonkKYC 29 512), Input (PlonkKYC 29 512), Proof (PlonkKYC 29 512))
kycCheckVerification witnessInputs x ps =
  let ac = compile @Fr (kycExample @N @K @Auto @2) :: ArithmeticCircuit Fr (((U1 :*: U1) :*: (U1 :*: U1)) :*: (U1 :*: U1)) (((Vector K :*: Vector 1) :*: (Vector 1 :*: Vector N)) :*: (Vector 1 :*: U1)) Par1
      ac2 = hmap (singleton . unPar1) ac
      ac3 =
        hlmap
          ( \a ->
              let (q, q1) = splitAt @K @(N + 3) a
                  (w, w1) = splitAt @N @3 q1
                  (e, e1) = splitAt @1 @2 w1
                  (r, t) = splitAt @1 @1 e1
               in ((q :*: e) :*: (r :*: w)) :*: (t :*: U1)
          )
          ac2

      (omega, k1, k2) = getParams 512
      plonk = Plonk omega k1 k2 ac3 x :: PlonkKYC 29 512
      setupP = setupProve @_ @RustCore plonk
      setupV = setupVerify @_ @RustCore plonk
      witness = (PlonkupWitnessInput @_ @(Vector 29) @BLS12_381_G1 undefined witnessInputs, ps)
      !(input, proof) = prove @(PlonkKYC 29 512) @RustCore setupP witness
   in (setupV, input, proof)


verifyKYCData :: KYCData N K Auto Context
              -> (SetupVerify (PlonkKYC 29 512), Input (PlonkKYC 29 512), Proof (PlonkKYC 29 512))
verifyKYCData (KYCData t id hash value) =
  let
    (x, y) = splitAt @8 @10 (toWords value)
    userCountry = (fromConstant . toConstant <$> y) :: Vector 10 F
    userAge = (fromConstant . toConstant <$> x) :: Vector 8 F
    dataID = fromConstant . toConstant <$> toWords (from id :: ByteString K Context) :: Vector 1 F
    dataHash = fromConstant . toConstant <$> toWords (from hash :: ByteString K Context) :: Vector 1 F
    dataType = fromConstant . toConstant <$> toWords t :: Vector 8 F
    b = dataType
        `append` userAge
        `append` userCountry
        `append` dataID
        `append` dataHash
        `append` dataHash
  in
    kycCheckVerification b (unsafePerformIO $ generate arbitrary) (unsafePerformIO $ generate arbitrary)

server :: Server API
server = serverProve
  where
    serverProve :: InputData N K Context -> Handler OutputData
    serverProve (InputData a kycData) = case a of
      1 -> do
        let (_, i, p) = verifyKYCData kycData
        return $ OutputData i p
      _ -> error "Not implemented statement"
