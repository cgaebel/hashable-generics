{-# LANGUAGE TypeOperators, FlexibleContexts, BangPatterns #-}
{-# OPTIONS_GHC -Wall #-}
-- | Stability:   stable
--   Portability: GHC
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
-- > -- Note: Even though gHashWithSalt could be curried, we explicitly list
-- > --       the parameters. If you don't do this, GHC will not inline the
-- > --       definition of gHashWithSalt, and the performance will not match
-- > --       a non-generic implementation. If you use this method, the generic
-- > --       hashWithSalt will generate the exact same code as a hand-rolled
-- > --       one.
-- > --
-- > --       Also, the INLINEABLE pragma is there to help hashable data
-- > --       structures in other modules write more efficient generic hashable
-- > --       instances too. This is the best way to get extremely performant,
-- > --       fully generic hash functions.
-- > instance Hashable AccountId
-- > instance Hashable Foo where
-- >     hashWithSalt s x = gHashWithSalt s x
-- >     {-# INLINEABLE hashWithSalt #-}
-- >
-- > -- recursive list-like type
-- > data N = Z | S N deriving Generic
-- >
-- > instance Hashable N where
-- >     hashWithSalt s x = gHashWithSalt s x
-- >     {-# INLINEABLE hashWithSalt #-}
-- >
-- > -- parametric and recursive type
-- > data Bar a = Bar0 | Bar1 a | Bar2 (Bar a)
-- >            deriving Generic
-- >
-- > instance Hashable a => Hashable (Bar a) where
-- >     hashWithSalt s x = gHashWithSalt s x
-- >     {-# INLINEABLE hashWithSalt #-}
--
-- I intend for 'gHashWithSalt' to be just as fast as a hand-rolled
-- implementation. Benchmarks are currently showing a 1.3x slowdown. Patches
-- to improve performance are welcome!
--
-- Note: The 'GHashable' type-class showing up in the type-signature is
--       used internally and not exported on purpose.
gHashWithSalt :: (Generic a, GHashable (Rep a)) => Int -> a -> Int
gHashWithSalt salt = gHashWithSalt_ salt . from
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
    gHashWithSalt_ = const
    {-# INLINE gHashWithSalt_ #-}

instance Hashable a => GHashable (K1 i a) where
    gHashWithSalt_ salt = hashWithSalt salt . unK1
    {-# INLINE gHashWithSalt_ #-}

instance GHashable a => GHashable (M1 i c a) where
    gHashWithSalt_ salt = gHashWithSalt_ salt . unM1
    {-# INLINE gHashWithSalt_ #-}

instance (GHashable a, GHashable b) => GHashable (a :*: b) where
    gHashWithSalt_ salt (x :*: y) = gHashWithSalt_ (gHashWithSalt_ salt x) y
    {-# INLINE gHashWithSalt_ #-}

instance (GHashable a, GHashable b) => GHashable (a :+: b) where
    gHashWithSalt_ !salt (L1 x) = gHashWithSalt_ (combine salt 0) x
    gHashWithSalt_ !salt (R1 x) = gHashWithSalt_ (combine salt distinguisher) x
    {-# INLINE gHashWithSalt_ #-}
