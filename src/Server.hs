{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Server where

import Data.Aeson (FromJSON (..), ToJSON (..))
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
import ZkFold.Algebra.Class (one, (+))
import ZkFold.Algebra.EllipticCurve.BLS12_381 hiding (Fr)
import qualified ZkFold.Algebra.EllipticCurve.BLS12_381 as EC
import ZkFold.Algebra.EllipticCurve.Class
import ZkFold.Algebra.Number
import ZkFold.Algebra.Polynomial.Univariate (PolyVec)
import ZkFold.ArithmeticCircuit (ArithmeticCircuit (..))
import ZkFold.Data.Vector
import ZkFold.FFI.Rust.Conversion
import ZkFold.FFI.Rust.Plonkup
import ZkFold.FFI.Rust.Types hiding (length)
import ZkFold.Protocol.NonInteractiveProof (
  NonInteractiveProof (..),
  setupProve,
  setupVerify,
  verify,
 )
import ZkFold.Protocol.NonInteractiveProof.Class ()
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
import Prelude hiding (Num (..), (!!))

import KYC (KYCData (..), N, kycExample)

type Constraints = 2 ^ 17

type K = 8

type Context = (Interpreter Fr)

newtype ProverInput = ProverInput (KYCData N K Auto Context)
  deriving Generic

instance FromJSON ProverInput

data ProverOutput = ProverOutput (Input HaskellPlonkKYC) (Proof HaskellPlonkKYC)
  deriving Generic

data VerifierInput = VerifierInput (Input PlonkKYC) (Proof PlonkKYC)
  deriving Generic

instance FromJSON VerifierInput

deriving instance Generic (PlonkupInput c)

deriving instance Generic (PlonkupProof c)

-- ToJSON

instance ToJSON c => ToJSON (JacobianPoint c)

instance (ToJSON (ScalarFieldOf c), ToJSON c) => ToJSON (PlonkupProof c)

instance (ToJSON (ScalarFieldOf c), ToJSON c) => ToJSON (PlonkupInput c)

instance ToJSON ProverOutput

-- FromJSON

instance {-# OVERLAPPABLE #-} forall h r. (IsRustType r, RustHaskell r h, FromJSON h) => FromJSON r where
  parseJSON a = h2r @r @h <$> parseJSON a

instance FromJSON c => FromJSON (JacobianPoint c)

instance (FromJSON c, FromJSON (ScalarFieldOf c)) => FromJSON (PlonkupInput c)

instance (FromJSON c, FromJSON (ScalarFieldOf c)) => FromJSON (PlonkupProof c)

instance FromJSON ProverOutput

type API =
  "prove" :> ReqBody '[JSON] ProverInput :> Post '[JSON] ProverOutput
    :<|> "verify" :> ReqBody '[JSON] VerifierInput :> Post '[JSON] Bool

type CompiledInput =
  (((U1 :*: U1) :*: (U1 :*: U1)) :*: (U1 :*: U1))
    :*: (((Vector K :*: Vector 1) :*: (Vector 1 :*: Vector N)) :*: (Vector 1 :*: U1))

type PlonkKYC =
  Plonkup CompiledInput Par1 Constraints Rust_BLS12_381_G1_Point Rust_BLS12_381_G2_Point BS.ByteString (RustPolyVec Fr)

type HaskellPlonkKYC =
  Plonkup CompiledInput Par1 Constraints BLS12_381_G1_Point BLS12_381_G2_Point BS.ByteString (PolyVec EC.Fr)

circuit :: ArithmeticCircuit Fr CompiledInput Par1
circuit = compile @Fr (kycExample @K @Auto @2)

plonk :: PlonkKYC
plonk = Plonkup omega k1 k2 circuit h1 gs
 where
  x = one + one
  (omega, k1, k2) = getParams (value @Constraints)
  (!gs, h1) = getSecretParams @Constraints @Rust_BLS12_381_G1_Point @Rust_BLS12_381_G2_Point x

setupP :: SetupProve PlonkKYC
setupP = setupProve @_ plonk

setupV :: SetupVerify PlonkKYC
setupV = setupVerify @_ plonk

kycCheckVerification
  :: KYCData N K Auto Context
  -> PlonkupProverSecret Rust_BLS12_381_G1_Point
  -> (Input HaskellPlonkKYC, Proof HaskellPlonkKYC)
kycCheckVerification kycData ps =
  let
    witnessInputs = runInterpreter $ arithmetize kycData
    hash = runInterpreter $ arithmetize $ kycHash kycData
    paddedWitnessInputs = (((U1 :*: U1) :*: (U1 :*: U1)) :*: (U1 :*: U1)) :*: (witnessInputs :*: (hash :*: U1))
    witness = (PlonkupWitnessInput @CompiledInput @Rust_BLS12_381_G1_Point paddedWitnessInputs, ps)
    (input, proof, _) = rustPlonkupProveNative @CompiledInput @Par1 @Constraints setupP witness
   in
    (input, proof)

server :: Server API
server = serverProve :<|> serverVerify
 where
  serverProve :: ProverInput -> Handler ProverOutput
  serverProve (ProverInput kycData) = pure $ ProverOutput i p
   where
    (i, p) = kycCheckVerification kycData (PlonkupProverSecret $ pure (one + one))

  serverVerify :: VerifierInput -> Handler Bool
  serverVerify (VerifierInput i p) = pure $ verify @PlonkKYC setupV i p
