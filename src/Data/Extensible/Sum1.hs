{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Data.Extensible.Sum1 where

data (f :||: g) a b = InL (f a b) | InR (g a b)

class Sum1 c s where
  peek1 :: c a b -> Maybe (s a b)
  lft1  :: s a b -> c a b

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





