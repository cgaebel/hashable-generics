{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, BangPatterns #-}
{-# OPTIONS_GHC -Wall #-}
module Main where

import Control.Applicative
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Data.Hashable
import Data.Hashable.Generic
import Data.Word

import GHC.Generics

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ testGroup "Documentation"
            [ testProperty "Simple Record" simpleRecord
            , testProperty "Recursive Type" recursiveType
            , testProperty "Parametric and Recursive Type" paraRecursive
            ]
        ]

-- | A value with bit pattern (01)* (or 5* in hexa), for any size of Int.
--   It is used as data constructor distinguisher. GHC computes its value during
--   compilation.
distinguisher :: Int
distinguisher = fromIntegral $ (maxBound :: Word) `quot` 3
{-# INLINE distinguisher #-}

data FooA = FooA AccountId Name Address
    deriving (Generic, Show)

instance Arbitrary FooA where
    arbitrary = FooA <$> arbitrary
                     <*> arbitrary
                     <*> arbitrary

data FooB = FooB AccountId Name Address

type Address = [String]
type Name = String

newtype AccountId = AccountId Int
    deriving (Hashable, Show)

instance Arbitrary AccountId where
    arbitrary = AccountId <$> arbitrary

instance Hashable FooA where
    hashWithSalt s x = gHashWithSalt s x
    {-# INLINEABLE hashWithSalt #-}

instance Hashable FooB where
    hashWithSalt salt (FooB ac n ad) = hashWithSalt
                                        (hashWithSalt
                                          (hashWithSalt salt ac)
                                          n)
                                        ad

aToB :: FooA -> FooB
aToB (FooA ac n ad) = FooB ac n ad

simpleRecord :: FooA -> Bool
simpleRecord a = let b = aToB a
                  in hash a == hash b

data NA = ZA | SA NA
    deriving (Generic, Show)

data NB = ZB | SB NB

instance Hashable NA where
    hashWithSalt s x = gHashWithSalt s x
    {-# INLINEABLE hashWithSalt #-}

instance Hashable NB where
    hashWithSalt !salt ZB      = combine salt 0
    hashWithSalt !salt (SB xs) = hashWithSalt (salt `combine` distinguisher) xs

instance Arbitrary NA where
    arbitrary = lst2A <$> arbitrary

lst2A :: [()] -> NA
lst2A [] = ZA
lst2A (_:xs) = SA $ lst2A xs

na2nb :: NA -> NB
na2nb ZA = ZB
na2nb (SA x) = SB $ na2nb x

recursiveType :: NA -> Bool
recursiveType a = let b = na2nb a
                   in hash a == hash b

data BarA a = BarA0 | BarA1 a | BarA2 (BarA a)
    deriving (Generic, Show)

data BarB a = BarB0 | BarB1 a | BarB2 (BarB a)

instance Arbitrary a => Arbitrary (BarA a) where
    arbitrary = oneof [ return BarA0
                      , BarA1 <$> arbitrary
                      , BarA2 <$> arbitrary
                      ]

instance Hashable a => Hashable (BarA a) where
    hashWithSalt s x = gHashWithSalt s x
    {-# INLINEABLE hashWithSalt #-}

instance Hashable a => Hashable (BarB a) where
    hashWithSalt !salt BarB0 = salt `combine` 0
    hashWithSalt !salt (BarB1 x) = hashWithSalt (salt `combine` distinguisher `combine` 0) x
    hashWithSalt !salt (BarB2 x) = hashWithSalt (salt `combine` distinguisher `combine` distinguisher) x

barA2B :: BarA a -> BarB a
barA2B BarA0 = BarB0
barA2B (BarA1 x) = BarB1 x
barA2B (BarA2 x) = BarB2 $ barA2B x

paraRecursive :: BarA Int -> Bool
paraRecursive a = let b = barA2B a
                   in hash a == hash b
