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

data (a :|: b) = DataL a | DataR b deriving (Eq, Ord)

instance (Show a, Show b) => Show (a :|: b) where
  show (DataL a) = show a
  show (DataR a) = show a


-- instance (Ord a, Ord b) => Ord (a :|: b) where
--   compare x y 
--     | Just (x1 :: a) <- peek x,
--       Just (y1 :: a) <- peek y = compare x1 y1

--     | Just (x1 :: b) <- peek x,
--       Just (y1 :: b) <- peek y = compare x1 y1

--     | Just (_ :: a) <- peek x,
--       Just (_ :: b) <- peek y = LT

--     | Just (_ :: b) <- peek x,
--       Just (_ :: a) <- peek y = GT


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




