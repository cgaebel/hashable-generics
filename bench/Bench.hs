{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -O2 #-}
module Main ( main ) where

import Criterion
import Criterion.Main
import Data.Hashable
import Data.Hashable.Generic
import GHC.Generics

data HandRolled = HR0
                | HR1 (Maybe Int)
                | HR2 HandRolled HandRolled

data GenericRolled = GR0
                   | GR1 (Maybe Int)
                   | GR2 GenericRolled GenericRolled
    deriving Generic

instance Hashable HandRolled where
    hashWithSalt salt HR0       = hashWithSalt salt $ (Left () :: Either () ())
    hashWithSalt salt (HR1 mi)  = hashWithSalt salt $ (Right $ Left mi :: Either () (Either (Maybe Int) ()))
    hashWithSalt salt (HR2 x y) = hashWithSalt salt $ (Right $ Right (x, y) :: Either () (Either () (HandRolled, HandRolled)))

instance Hashable GenericRolled where
    hashWithSalt s x = gHashWithSalt s x

bigHandRolledDS :: HandRolled
bigHandRolledDS = let a = HR0
                      b = HR1 $ Just 1
                      c = HR1 $ Nothing
                      d = HR1 $ Just 3
                      e = HR2 a b
                      f = HR2 c d
                      g = HR2 e e
                      h = HR2 f f
                      i = HR2 e f
                      j = HR2 g h
                      k = HR2 i j
                   in HR2 k k

bigGenericRolledDS :: GenericRolled
bigGenericRolledDS = f bigHandRolledDS
    where
        f HR0       = GR0
        f (HR1 x)   = GR1 x
        f (HR2 x y) = GR2 (f x) (f y)

main :: IO ()
main = defaultMain [ bcompare [ bench "handrolled" $ whnf hash bigHandRolledDS
                              , bench "generic" $ whnf hash bigGenericRolledDS
                              ]
                   ]
