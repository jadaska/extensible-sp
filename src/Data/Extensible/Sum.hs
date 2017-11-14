{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}


module Data.Extensible.Sum where

import Data.Typeable
import Control.Lens(prism', Prism')
import Control.Monad

data (a :|: b) = DataL a | DataR b deriving (Eq)

instance (Show a, Show b) => Show (a :|: b) where
  show (DataL a) = show a
  show (DataR a) = show a


class SumClass c s where
  peek :: c -> Maybe s
  lft  :: s -> c

type (w :>|: a)  = (SumClass w a)

sumPrism :: (w :>|: a) => Prism' w a 
sumPrism = prism' lft peek


instance SumClass a a where
  peek = Just
  lft  = id

instance {-# INCOHERENT #-} SumClass (a :|: b) b where
  peek (DataR x) = Just x
  peek _ = Nothing
  lft = DataR

instance {-# INCOHERENT #-} (SumClass c a) => SumClass (c :|: b) a where
  peek (DataL x) = peek x
  peek _ = Nothing
  lft = DataL . lft


instance SumClass (Maybe a) a where
   peek = id
   lft  = Just


instance SumClass (Either a b) b where
   peek (Right x) = Just x
   peek _ = Nothing
   lft  = Right




