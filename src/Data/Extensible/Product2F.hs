{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}

module Data.Extensible.Product2F where

import Data.Typeable
import Data.Extensible.Type
import Data.Extensible.Sum(AltLoc(..))
import Data.Monoid
import Data.Promotion.Prelude.List
import Data.Extensible.Product
import Data.Extensible.Sum2F


data Interp2F g f = Interp2F (forall a' b' . f g a' b' -> g a' b')

data HK2FList k (p :: [(* -> * -> *) -> * -> * -> *]) where
	HK2FCons :: k x -> HK2FList k xs -> HK2FList k (x ': xs)
	HK2FNil  :: HK2FList k '[]

infixr 5 #:
(#:) :: (forall a' b' . x g a' b' -> g a' b') 
	-> HK2FList (Interp2F g) xs 
	-> HK2FList (Interp2F g) (x ': xs)
(#:) fxn rest = HK2FCons (Interp2F fxn) rest

