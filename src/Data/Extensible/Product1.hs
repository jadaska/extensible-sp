{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Extensible.Product1 where

import Data.Monoid 
import Data.Typeable
import Control.Lens(Lens', lens)


-- | Extensible product typeclass for type
class ProductClass c s where
  grab :: c -> s
  stash  :: s -> c -> c

-- | Short-hand type operator for product class
type (c :>&: a)  = (ProductClass c a)


type NoConstr = (() :: Constraint)

type family (>&) x p where
  (>&) x '[] = NoConstr
  (>&) x (a ': rest) = (x :>&: a, x >& rest)

-- | Convenience lens for manipulating product
prodLens :: (c :>&: a) => Lens' c a 
prodLens = lens grab (flip stash)

-- | Heterogenous list
data HList p  where
  HCons :: Typeable x => x -> HList xs -> HList (x ': xs)
  HNil  :: HList '[]

instance (Show a, Show (HList rest)) => Show (HList (a ': rest)) where
  show (HCons x ys) = show x <> " ++ " <> show ys

instance Show (HList '[]) where
 show HNil = "end"

instance Typeable c => ProductClass (HList (c ': rest)) c where
  grab (HCons x _) = x
  stash x (HCons _ rest) (HCons x rest)

instance ProductClass rest c => ProductClass (HList (b ': rest)) c where
  grab (HCons _ rest) = grab rest
  stash x (HCons y rest) HCons y $ stash x rest 



