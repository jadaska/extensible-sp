{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Data.Extensible.Sum2 where

data (f :+: g) a b = InL (f a b) | InR (g a b)

instance (Show (f a b), Show (g a b)) => Show ((f :+: g) a b) where
  show (InL x) = show x
  show (InR x) = show x


class Sum2 c s where
  peek2 :: c a b -> Maybe (s a b)
  lft2  :: s a b -> c a b

type (w :>+: a) = (Sum2 w a)

instance Sum2 f f where
  peek2 = Just
  lft2 = id

instance {-# OVERLAPS #-} Sum2 (f :+: g) g where
  peek2 (InR f) = Just f
  peek2 _ = Nothing
  lft2 = InR

instance {-# OVERLAPS #-} (c :>+: a) => Sum2 (c :+: b) a where
  peek2 (InL x) = peek2 x
  peek2 _ = Nothing
  lft2 = InL . lft2  





