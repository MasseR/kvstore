module Network.Envelope where

import Data.ByteString.Lazy as B

-- | Source uniquely identifies the sender. Should be created by crypto primitives
newtype Source = Source B.ByteString

-- | Signature verifies the message. The signature is calculated by `source <>
-- | encode msg <> secret` where the secret is a predetermined secret value
-- | between servers.
newtype Signature = Signature String

-- | Envelope wraps the inbound/outbound messages with metadata
data Envelope a = Envelope { source :: Source
                           , msg :: a
                           , signature :: Signature}
