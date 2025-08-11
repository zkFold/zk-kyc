{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Server where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString as BS
import GHC.Generics (Generic, Par1, U1 (..), (:*:) (..))
import GHC.TypeNats
import Servant (
  Handler,
  JSON,
  Post,
  ReqBody,
  Server,
  type (:<|>) (..),
  type (:>),
 )
import ZkFold.Algebra.Class (one)
import ZkFold.Algebra.EllipticCurve.BLS12_381
import ZkFold.Algebra.EllipticCurve.Class
import ZkFold.Algebra.Field
import ZkFold.Algebra.Number
import ZkFold.Algebra.Polynomial.Univariate (PolyVec)
import ZkFold.ArithmeticCircuit (ArithmeticCircuit (..))
import ZkFold.Data.Vector
import ZkFold.Protocol.NonInteractiveProof (
  NonInteractiveProof (..),
  prove,
  setupProve,
  setupVerify,
  verify,
 )
import ZkFold.Protocol.Plonkup (Plonkup (Plonkup))
import ZkFold.Protocol.Plonkup.Input (PlonkupInput (..))
import ZkFold.Protocol.Plonkup.Proof (PlonkupProof (..))
import ZkFold.Protocol.Plonkup.Prover (PlonkupProverSecret (..))
import ZkFold.Protocol.Plonkup.Utils (getParams, getSecretParams)
import ZkFold.Protocol.Plonkup.Witness (PlonkupWitnessInput (PlonkupWitnessInput))
import ZkFold.Symbolic.Compiler
import ZkFold.Symbolic.Data.Class (arithmetize)
import ZkFold.Symbolic.Data.Combinators (RegisterSize (Auto))
import ZkFold.Symbolic.Interpreter (Interpreter, runInterpreter)
import Prelude

import KYC (KYCData (..), kycExample)

type Constraints = 2 ^ 17

type N = 18

type K = 8

type F = Zp BLS12_381_Scalar

type Context = (Interpreter F)

newtype InputData n k c = InputData {kycData :: KYCData n k Auto c}
  deriving Generic

instance FromJSON (InputData N K Context)

instance ToJSON (InputData N K Context)

data ProverOutput = ProverOutput
  { input :: Input PlonkKYC
  , proof :: Proof PlonkKYC
  }
  deriving Generic

deriving instance Generic (PlonkupInput c)

deriving instance Generic (PlonkupProof c)

instance ToJSON c => ToJSON (JacobianPoint c)

instance (ToJSON (ScalarFieldOf c), ToJSON c) => ToJSON (PlonkupProof c)

instance (ToJSON (ScalarFieldOf c), ToJSON c) => ToJSON (PlonkupInput c)

instance ToJSON ProverOutput

instance FromJSON c => FromJSON (JacobianPoint c)

instance (FromJSON c, FromJSON (ScalarFieldOf c)) => FromJSON (PlonkupInput c)

instance (FromJSON c, FromJSON (ScalarFieldOf c)) => FromJSON (PlonkupProof c)

instance FromJSON ProverOutput

type API =
  "prove" :> ReqBody '[JSON] (InputData N K Context) :> Post '[JSON] ProverOutput
    :<|> "verify" :> ReqBody '[JSON] ProverOutput :> Post '[JSON] Bool

type CompiledInput =
  (((U1 :*: U1) :*: (U1 :*: U1)) :*: (U1 :*: U1))
    :*: (((Vector K :*: Vector 1) :*: (Vector 1 :*: Vector N)) :*: (Vector 1 :*: U1))

type PlonkKYC =
  Plonkup CompiledInput Par1 Constraints BLS12_381_G1_JacobianPoint BLS12_381_G2_JacobianPoint BS.ByteString (PolyVec Fr)

circuit :: ArithmeticCircuit Fr CompiledInput Par1
circuit = compile @Fr (kycExample @N @K @Auto @2)

x :: Fr
x = one

omega :: Fr
k1 :: Fr
k2 :: Fr
(omega, k1, k2) = getParams (value @Constraints)

gs :: Vector (Constraints + 6) BLS12_381_G1_JacobianPoint
h1 :: BLS12_381_G2_JacobianPoint
(gs, h1) = getSecretParams @Constraints @BLS12_381_G1_JacobianPoint @BLS12_381_G2_JacobianPoint x

plonk :: PlonkKYC
plonk = Plonkup omega k1 k2 circuit h1 gs

setupP :: SetupProve PlonkKYC
setupP = setupProve @_ plonk

setupV :: SetupVerify PlonkKYC
setupV = setupVerify @_ plonk

kycCheckVerification
  :: KYCData N K Auto Context
  -> PlonkupProverSecret BLS12_381_G1_JacobianPoint
  -> (Input PlonkKYC, Proof PlonkKYC)
kycCheckVerification kycData ps =
  let
    witnessInputs = runInterpreter $ arithmetize kycData
    hash = runInterpreter $ arithmetize $ kycHash kycData
    paddedWitnessInputs = (((U1 :*: U1) :*: (U1 :*: U1)) :*: (U1 :*: U1)) :*: (witnessInputs :*: (hash :*: U1))
    witness = (PlonkupWitnessInput @CompiledInput @BLS12_381_G1_JacobianPoint paddedWitnessInputs, ps)
    !(input, proof) = prove @PlonkKYC setupP witness
   in
    (input, proof)

server :: Server API
server = serverProve :<|> serverVerify
 where
  serverProve :: InputData N K Context -> Handler ProverOutput
  serverProve (InputData kycData) = pure $ ProverOutput i p
   where
    (i, p) = kycCheckVerification kycData (PlonkupProverSecret $ pure one)

  serverVerify :: ProverOutput -> Handler Bool
  serverVerify (ProverOutput i p) = pure $ verify @PlonkKYC setupV i p
  