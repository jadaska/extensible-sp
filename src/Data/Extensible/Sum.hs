{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Data.Extensible.Sum where

import Control.Lens(prism', Prism')

data (a :|: b) = DataL a | DataR b deriving (Show, Eq)

class SumClass c s where
  peek :: c -> Maybe s
  lft  :: s -> c

type (w :>|: a)  = (SumClass w a)

sumPrism :: (w :>|: a) => Prism' w a 
sumPrism = prism' lft peek


instance SumClass a a where
  peek = Just
  lft  = id

instance {-# OVERLAPS #-} SumClass (a :|: b) b where
  peek (DataR x) = Just x
  peek _ = Nothing
  lft = DataR

instance {-# OVERLAPS #-} (SumClass c a) => SumClass (c :|: b) a where
  peek (DataL x) = peek x
  peek _ = Nothing
  lft = DataL . lft


instance SumClass (Maybe a) a where
   peek = id
   lft  = Just


instance SumClass (Either a b) a where
   peek (Left x) = Just x
   peek _ = Nothing
   lft  = Left


