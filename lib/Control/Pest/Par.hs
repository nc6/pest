{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module: Control.Pest.Par
-- Description: Implementation of PESTs using the @par@ combinator
--
-- This module provides an implementation of PESTs using the @par@ combinator from
-- 'Control.Parallel'.
module Control.Pest.Par
  ( PEST,

    -- * Constructing and inspecting PESTs
    pest,
    unPest,

    -- * Serialisation
    safeSerialisePEST,
    unsafeSerialisePEST,
    deserialisePEST,
  )
where

import Codec.Serialise (Serialise (decode, encode))
import Codec.Serialise.Decoding
  ( Decoder,
    decodeListLenOf,
    decodeWord64,
    decodeWord8,
  )
import Codec.Serialise.Encoding
  ( Encoding,
    encodeListLen,
    encodeWord64,
    encodeWord8,
  )
import Control.Parallel (par)
import GHC.Fingerprint.Type (Fingerprint (..))
import GHC.IO.Unsafe (unsafePerformIO)
import GHC.StaticPtr
  ( StaticPtr,
    deRefStaticPtr,
    staticKey,
    unsafeLookupStaticPtr,
  )
import NoThunks.Class
  ( NoThunks,
    OnlyCheckWhnfNamed (..),
    unsafeNoThunks,
  )

data PEST value args = PEST
  { fun :: !(StaticPtr (args -> value)),
    args :: !args,
    -- | Value to be computed by the PEST. This value should be such that
    -- evaluating it to WHNF is sufficient for whatever expensive computation is
    -- needed
    value :: WrappedValue value
  }

pest :: StaticPtr (args -> value) -> args -> PEST value args
pest fun args =
  let v = deRefStaticPtr fun args
   in v
        `par` PEST
          { fun = fun,
            args = args,
            value = WrappedValue v
          }

unPest :: PEST value args -> value
unPest = unWrappedValue . value

--------------------------------------------------------------------------------
-- Value wrapper
--------------------------------------------------------------------------------

-- | Wrapper for value inside a PEST
newtype WrappedValue a = WrappedValue {unWrappedValue :: a}
  deriving (NoThunks) via OnlyCheckWhnfNamed "WrappedValue" a

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

-- | Serialise a PEST through serialising the closure.
safeSerialisePEST ::
  Serialise args =>
  (PEST value args -> Encoding)
safeSerialisePEST PEST {fun, args} =
  encodeListLen 3 <> encodeWord8 0 <> encodeStaticPtr fun <> encode args

-- | Serialise a PEST.
--
--   Either the value or the closure will be serialised depending on whether the
--   value has completed evaluation or not. This is marked as unsafe since
--   allows the caller to determine whether the thunk has been evaluated,
--   leading to potentially impure behaviour.
--
--   Compared to 'safeSerialisePEST', this has the advantage that already
--   computed values will not be recomputed when deserialised.
--
--   Note that even if the value has already been computed, we still encode the
--   closure since it allows us to fully repopulate the PEST
unsafeSerialisePEST ::
  Serialise args =>
  Serialise value =>
  (PEST value args -> Encoding)
unsafeSerialisePEST p@PEST {fun, args, value} =
  case unsafeNoThunks value of
    Nothing ->
      encodeListLen 3
        <> encodeWord8 1
        <> encode (unWrappedValue value)
        <> encodeStaticPtr fun
        <> encode args
    Just _ -> safeSerialisePEST p

deserialisePEST ::
  forall args value s.
  (Serialise args, Serialise value) =>
  Decoder s (PEST args value)
deserialisePEST = do
  decodeListLenOf 3
  tag <- decodeWord8
  case tag of
    0 -> pest <$> decodeStaticPtr <*> decode
    1 -> do
      value <- decode
      fun <- decodeStaticPtr
      args <- decode
      pure $ PEST fun args (WrappedValue value)
    n -> fail $ "Invalid token when deserialising PEST: " <> show n

encodeStaticPtr :: StaticPtr a -> Encoding
encodeStaticPtr ptr =
  let Fingerprint a b = staticKey ptr
   in encodeListLen 2 <> encodeWord64 a <> encodeWord64 b

decodeStaticPtr :: Decoder s (StaticPtr (args -> value))
decodeStaticPtr = do
  a <- decodeWord64
  b <- decodeWord64
  case unsafePerformIO (unsafeLookupStaticPtr (Fingerprint a b)) of
    Nothing -> fail "Cannot lookup static pointer when deserialising PEST"
    Just ptr -> pure ptr
