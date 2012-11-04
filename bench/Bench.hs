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
                      l = HR2 k k
                      m = HR2 l l
                      n = HR2 m m
                      o = HR2 n n
                      p = HR2 o o
                      q = HR2 p p
                      r = HR2 q q
                      s = HR2 r r
                      t = HR2 s s
                      u = HR2 t t
                      v = HR2 u u
                      w = HR2 v v
                      x = HR2 w w
                      y = HR2 x x
                      z = HR2 y y
                      za = HR2 z z
                      zb = HR2 za za
                      zc = HR2 zb zb
                      zd = HR2 zc zc
                   in zd

bigGenericRolledDS :: GenericRolled
bigGenericRolledDS = let a = GR0
                         b = GR1 $ Just 1
                         c = GR1 $ Nothing
                         d = GR1 $ Just 3
                         e = GR2 a b
                         f = GR2 c d
                         g = GR2 e e
                         h = GR2 f f
                         i = GR2 e f
                         j = GR2 g h
                         k = GR2 i j
                         l = GR2 k k
                         m = GR2 l l
                         n = GR2 m m
                         o = GR2 n n
                         p = GR2 o o
                         q = GR2 p p
                         r = GR2 q q
                         s = GR2 r r
                         t = GR2 s s
                         u = GR2 t t
                         v = GR2 u u
                         w = GR2 v v
                         x = GR2 w w
                         y = GR2 x x
                         z = GR2 y y
                         za = GR2 z z
                         zb = GR2 za za
                         zc = GR2 zb zb
                         zd = GR2 zc zc
                      in zd

main :: IO ()
main = defaultMain [ bcompare [ bench "handrolled" $ whnf hash bigHandRolledDS
                              , bench "generic" $ whnf hash bigGenericRolledDS
                              ]
                   ]
