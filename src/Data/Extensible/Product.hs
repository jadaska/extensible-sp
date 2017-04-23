{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Data.Extensible.Product where

import Control.Lens(Lens', lens)


data (a :&: b) = Prod a b deriving Show

-- | Extensible product typeclass for type
class ProductClass c s where
  grab :: c -> s
  stash  :: s -> c -> c

-- | Short-hand type operator for product class
type (c :>&: a)  = (ProductClass c a)

-- | Convenience lens for manipulating product
prodLens :: (c :>&: a) => Lens' c a 
prodLens = lens grab (flip stash)

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
