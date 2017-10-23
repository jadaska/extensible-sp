{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Data.Extensible.Sum1 where

data (f :||: g) a = InL (f a) | InR (g a) deriving (Eq)


instance (Show (f a), Show (g a)) => Show ((f :||: g) a) where
  show (InL x) = show x
  show (InR x) = show x


class Sum1 c s where
  peek1 :: c a -> Maybe (s a)
  lft1  :: s a -> c a

type (w :>||: a) = (Sum1 w a)

instance Sum1 f f where
  peek1 = Just
  lft1 = id

instance {-# OVERLAPS #-} Sum1 (f :||: g) g where
  peek1 (InR f) = Just f
  peek1 _ = Nothing
  lft1 = InR

instance {-# OVERLAPS #-} (c :>||: a) => Sum1 (c :||: b) a where
  peek1 (InL x) = peek1 x
  peek1 _ = Nothing
  lft1 = InL . lft1

ajoin :: (m1 a -> b) -> (m2 a -> b) -> ((m1 :||: m2) a -> b)
ajoin f1 _  (InL m) = f1 m
ajoin _  f2 (InR m) = f2 m







