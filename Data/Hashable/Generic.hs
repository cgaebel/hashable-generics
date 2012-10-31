{-# LANGUAGE TypeOperators, FlexibleContexts, BangPatterns #-}
-- | Stability:   stable
--   Portability: GHC
--
--   NOTE: This module exports the Hashable class for convenience. If this
--         becomes a problem, just use a qualified import.
module Data.Hashable.Generic ( gHashWithSalt
                             , Hashable(..)
                             ) where

import Data.Hashable
import Data.Word
import GHC.Generics

-- | "GHC.Generics"-based 'hashWithSalt' implementation
--
-- This provides a generic `hashWithSalt` implementation for one type at a
-- time. If the type of the value 'gHashWithSalt' is asked to hash
-- contains values of other types, those types have to provide
-- 'Hashable' instances. This also means that recursive types can only
-- be used with 'gHashWithSalt' if a 'Hashable' instance has been defined
-- as well (see examples below).
--
-- The typical usage for 'gHashWithSalt' is for reducing boilerplate code
-- when defining 'Hashable' instances for ordinary algebraic
-- datatypes. See the code below for some simple usage examples:
--
-- > {-# LANGUAGE DeriveGeneric #-}
-- >
-- > import Data.Hashable
-- > import Data.Hashable.Generic ( gHashWithSalt )
-- > import GHC.Generics
-- >
-- > -- simple record
-- > data Foo = Foo AccountId Name Address
-- >          deriving Generic
-- >
-- > type Address      = [String]
-- > type Name         = String
-- > newtype AccountId = AccountId Int
-- >
-- > instance Hashable AccountId
-- > instance Hashable Foo where hashWithSalt = gHashWithSalt
-- >
-- > -- recursive list-like type
-- > data N = Z | S N deriving Generic
-- >
-- > instance Hashable N where hashWithSalt = gHashWithSalt
-- >
-- > -- parametric and recursive type
-- > data Bar a = Bar0 | Bar1 a | Bar2 (Bar a)
-- >            deriving Generic
-- >
-- > instance Hashable a => Hashable (Bar a) where hashWithSalt = gHashWithSalt
--
-- Note: The 'GHashable' type-class showing up in the type-signature is
--       used internally and not exported on purpose currently
gHashWithSalt :: (Generic a, GHashable (Rep a)) => Int -> a -> Int
gHashWithSalt salt x = gHashWithSalt_ salt $ from x
{-# INLINE gHashWithSalt #-}

-- | A value with bit pattern (01)* (or 5* in hexa), for any size of Int.
--   It is used as data constructor distinguisher. GHC computes its value during
--   compilation.
--
--   Blatantly stolen from @hashable@.
distinguisher :: Int
distinguisher = fromIntegral $ (maxBound :: Word) `quot` 3
{-# INLINE distinguisher #-}

-- | Hidden internal type class.
class GHashable f where
    gHashWithSalt_ :: Int -> f a -> Int

instance GHashable U1 where
    gHashWithSalt_ !salt U1 = hashWithSalt salt ()
    {-# INLINE gHashWithSalt_ #-}

instance Hashable a => GHashable (K1 i a) where
    gHashWithSalt_ !salt = hashWithSalt salt . unK1
    {-# INLINE gHashWithSalt_ #-}

instance GHashable a => GHashable (M1 i c a) where
    gHashWithSalt_ !salt = gHashWithSalt_ salt . unM1
    {-# INLINE gHashWithSalt_ #-}

instance (GHashable a, GHashable b) => GHashable (a :*: b) where
    gHashWithSalt_ !salt (x :*: y) = gHashWithSalt_ (gHashWithSalt_ salt x) y
    {-# INLINE gHashWithSalt_ #-}

-- This instance is suboptimal (with the salt+1 hackery). Is there a better way
-- to be doing this so that both choices can be unique?
instance (GHashable a, GHashable b) => GHashable (a :+: b) where
    gHashWithSalt_ !salt (L1 x) = gHashWithSalt_ (salt `combine` 0) x
    gHashWithSalt_ !salt (R1 x) = gHashWithSalt_ (salt `combine` distinguisher) x
    {-# INLINE gHashWithSalt_ #-}
