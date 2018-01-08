{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Data.Extensible.Product where

import Control.Lens(Lens', lens)
import Data.Promotion.Prelude.List
import Data.Promotion.TH
import Data.Monoid 
import Data.Maybe
import Data.Typeable
import GHC.Exts(Constraint)

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


data HList p  where
  HCons :: Typeable x => x -> HList xs -> HList (x ': xs)
  HNil  :: HList '[]

instance (Show a, Show (HList rest)) => Show (HList (a ': rest)) where
  show (HCons x ys) = show x <> " ++ " <> show ys

instance Show (HList '[]) where
 show HNil = "end"

instance Typeable c => ProductClass (HList (c ': rest)) c where
  grab (HCons x _) = x
  stash x (HCons _ rest) = (HCons x rest)

instance ProductClass (HList rest) c => ProductClass (HList (b ': rest)) c where
  grab (HCons _ rest) = grab rest
  stash x (HCons y rest) = HCons y $ stash x rest 


fromHList :: (forall a . Typeable a => a -> b) -> HList p -> [b]
fromHList _ HNil = []
fromHList f (HCons x rest) = f x : fromHList f rest


class HFilter p where
  hfilter :: Typeable a => HList p -> [a]

instance HFilter '[] where
  hfilter HNil = []

instance HFilter rest => HFilter (b ': rest) where
  hfilter (HCons x rest) = fromMaybe (hfilter rest) $ do 
    y <- cast x
    return $ y : hfilter rest


-- hfilter :: Typeable a => HList (b ': p) -> [a]
-- hfilter HNil = []


data HK1List k p  where
  HK1Cons :: Typeable x => k x -> HK1List k xs -> HK1List k (x ': xs)
  HK1Nil  :: HK1List k '[]

instance Typeable c => ProductClass (HK1List k (c ': rest)) (k c) where
  grab (HK1Cons x _) = x
  stash x (HK1Cons _ rest) = (HK1Cons x rest)

instance ProductClass (HK1List k rest) (k c) => ProductClass (HK1List k (b ': rest)) (k c) where
  grab (HK1Cons _ rest) = grab rest
  stash x (HK1Cons y rest) = HK1Cons y $ stash x rest 


fromHK1List :: (forall a . Typeable a => k a -> b) -> HK1List k p -> [b]
fromHK1List _ HK1Nil = []
fromHK1List f (HK1Cons x rest) = f x : fromHK1List f rest


hk1filter :: (Typeable k, Typeable a) => HK1List k p -> [k a]
hk1filter HK1Nil = []
hk1filter (HK1Cons x rest) = fromMaybe (hk1filter rest) $ do 
  y <- cast x
  return $ y : hk1filter rest


data HK2List k p  where
  HK2Cons :: (Typeable a, Typeable b) => k a b -> HK2List k xs -> HK2List k ((a,b) ': xs)
  HK2Nil  :: HK2List k '[]


instance (Typeable a, Typeable b) => ProductClass (HK2List k ((a,b) ': rest)) (k a b) where
  grab (HK2Cons x _)       = x
  stash x (HK2Cons _ rest) = (HK2Cons x rest)

instance (Typeable a, Typeable b, ProductClass (HK2List k rest) (k a b)) => ProductClass (HK2List k ((x,y) ': rest)) (k a b) where
  grab (HK2Cons _ rest)    = grab rest
  stash x (HK2Cons y rest) = HK2Cons y $ stash x rest 


class FromHK2List p where
  fromHK2List :: (forall a b . (Typeable a, Typeable b) => k a b -> c) -> HK2List k p -> [c]

instance FromHK2List rest => FromHK2List ((a,b) ': rest) where
  fromHK2List f (HK2Cons x rest) = f x : fromHK2List f rest  

instance FromHK2List '[] where
  fromHK2List _ HK2Nil = []  

class HK2Filter k p where
  hk2filter :: forall a b . (Typeable a, Typeable b, Typeable k) => HK2List k p -> [k a b]  

instance HK2Filter k '[] where
  hk2filter HK2Nil = []

instance (Typeable k, HK2Filter k rest) => HK2Filter k ((a,b) ': rest) where
  hk2filter (HK2Cons x  rest) = fromMaybe (hk2filter rest) $ do 
    y <- cast x
    return $ y : hk2filter rest





-- | Legacy product data type
data (a :&: b) = Prod a b deriving Show


-- | cons-like operator for products
(<&) :: a -> b -> a :&: b
(<&) = Prod


instance ProductClass a a where
  grab = id
  stash x _ = x

instance {-# INCOHERENT #-} ProductClass (a :&: b) b where
  grab (Prod x y) = y
  stash y (Prod xx yy) = Prod xx y

instance {-# INCOHERENT #-} ProductClass c a => ProductClass (c :&: b) a where
  grab (Prod x y) = grab x
  stash x (Prod xx yy) = Prod (stash x xx) yy

instance (Monoid a, Monoid b) => Monoid (a :&: b) where
  mempty = Prod mempty mempty
  mappend (Prod x1 x2) (Prod y1 y2) = Prod (x1 <> y1) (x2 <> y2)

instance ProductClass (a,b) a where
  grab = fst
  stash x (_, y) = (x, y)

instance ProductClass (a,b) b where
  grab = snd
  stash y (x, _) = (x, y)

instance ProductClass (a,b,c) a where
  grab (a, _, _) = a
  stash a (_, b, c) = (a,b,c)

instance ProductClass (a,b,c) b where
  grab (_, b, _) = b
  stash b (a, _, c) = (a,b,c)

instance ProductClass (a,b,c) c where
  grab (_, _, c) = c
  stash c (a, b, _) = (a,b,c)

instance ProductClass (a,b,c,d) a where
  grab (a, _, _, _) = a
  stash a (_, b, c, d) = (a,b,c,d)

instance ProductClass (a,b,c,d) b where
  grab (_, b, _, _) = b
  stash b (a, _, c,d) = (a,b,c,d)

instance ProductClass (a,b,c,d) c where
  grab (_, _, c, _) = c
  stash c (a, b, _, d) = (a,b,c,d)

instance ProductClass (a,b,c,d) d where
  grab (_, _, _, d) = d
  stash d (a, b, c, _) = (a,b,c,d)

instance ProductClass (a,b,c,d,e) a where
  grab (a, _, _, _,_) = a
  stash a (_, b, c, d,e) = (a,b,c,d,e)

instance ProductClass (a,b,c,d,e) b where
  grab (_, b, _, _,_) = b
  stash b (a, _, c,d,e) = (a,b,c,d,e)

instance ProductClass (a,b,c,d,e) c where
  grab (_, _, c, _,_) = c
  stash c (a, b, _, d,e) = (a,b,c,d,e)

instance ProductClass (a,b,c,d,e) d where
  grab (_, _, _, d,_) = d
  stash d (a, b, c, _,e) = (a,b,c,d,e)

instance ProductClass (a,b,c,d,e) e where
  grab (_, _, _, _, e) = e
  stash e (a, b, c, d,_) = (a,b,c,d,e)
