{-# Language GeneralizedNewtypeDeriving #-}
{-# Language DeriveGeneric #-}
{-# Language ScopedTypeVariables #-}
module Network.Envelope
    ( Envelope
    , VerifiedEnvelope
    , msg
    , verifyEnvelope
    , envelope) where

import Data.ByteString.Lazy as B
import Crypto.Hash
import Data.Monoid ((<>))
import Data.Binary
import GHC.Generics (Generic)

-- | Source uniquely identifies the sender. Should be created by crypto primitives
newtype Source = Source { unSource :: B.ByteString } deriving (Show, Binary)

-- | Signature verifies the message. The signature is calculated by `source <>
-- | encode msg <> secret` where the secret is a predetermined secret value
-- | between servers.
newtype Signature = Signature String deriving (Eq, Generic, Show)

newtype Secret = Secret B.ByteString deriving (Eq, Show, Generic, Binary)

instance Binary Signature

-- | Envelope wraps the inbound/outbound messages with metadata
data Envelope a = Envelope { source :: Source
                           , msg :: a
                           , signature :: Signature }
    deriving (Show, Generic)

instance (Binary a) => Binary (Envelope a)

data VerifiedEnvelope a = VerifiedEnvelope (Envelope a)

verifyEnvelope :: Binary a => Envelope a -> Secret -> Either (Envelope a) (VerifiedEnvelope a)
verifyEnvelope env (Secret secret) =
    let h = hash $ B.toStrict (s <> m <> secret) :: Digest SHA256
        s = unSource . source $ env
        m = encode (msg env)
    in if Signature (show h) == signature env then Right (VerifiedEnvelope env) else Left env

envelope :: Binary a => Source -> a -> Secret -> Envelope a
envelope (Source s) message (Secret secret) =
    let sig = hash $ B.toStrict (s <> m <> secret) :: Digest SHA256
        m = encode message
    in Envelope (Source s) message (Signature (show sig))
