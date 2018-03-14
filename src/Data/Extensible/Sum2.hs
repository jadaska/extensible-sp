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

module Data.Extensible.Sum2 where

import Data.Typeable
import Data.Extensible.Type
import Data.Extensible.Sum(AltLoc(..))
import Data.Monoid
import Data.Promotion.Prelude.List
import Data.Extensible.Product


data Alt2' k (p ::[* -> * -> *]) a b where
  Blank2 :: (Typeable (x :: * -> * -> *)) 
    => Proxy x -> Alt2' k xs a b -> Alt2' k (x ': xs) a b
  Cur2 :: (Typeable x) 
    => x a b -> Alt2' 'Behind xs a b -> Alt2' 'Ahead (x ': xs) a b
  ANil2 :: Alt2' 'Behind '[] a b 

type Alt2 p a b = Alt2' 'Ahead p a b


instance (Show (a b c), Show (Alt2' 'Behind rest b c), Show (Alt2' 'Ahead rest b c)) 
  => Show (Alt2' 'Ahead (a ': rest) b c) where
  show (Cur2 x rest) = show x <> " " <> show rest
  show (Blank2 _ rest) = "* " <> show rest

instance Show (Alt2' 'Behind rest b c) => Show (Alt2' 'Behind (a ': rest) b c) where
  show (Blank2 p rest) = "* " <> show rest

instance Show (Alt2' k '[] b c) where
  show _ = ""


-- | blank tail/head for handling Alt
class BlankTail2 p where
  blankTail2 :: Alt2' 'Behind p a b

instance BlankTail2 '[] where
  blankTail2 = ANil2

instance (Typeable a, BlankTail2 rest) => BlankTail2 (a ': rest) where
  blankTail2 = Blank2 (Proxy :: Proxy a) $ blankTail2


-- class BlankHead2 p where
--   blankHead2 :: Proxy p -> Alt2' 'Ahead q -> Alt2' 'Ahead (p :++ q)

-- instance BlankHead2 '[] where
--   blankHead _ y = unsafeCoerce y

type family (:~) (a :: * -> * -> *) b where
  (:~) a a = 'True
  (:~) a b = 'False

class Sum2' (flag :: Bool) c s where
  peek2' :: Proxy flag -> c a b -> Maybe (s a b)
  lft2'  :: Proxy flag -> s a b -> c a b

instance 
  (   (c :~ d) ~ flag
    , Sum2' flag (Alt2' 'Ahead (d ': rest)) c
  ) => Sum2 (Alt2' 'Ahead (d ': rest)) c where
    peek2 = peek2' (Proxy :: Proxy flag)
    lft2 = lft2' (Proxy :: Proxy flag)    

instance 
  (   BlankTail2 rest
    , Typeable a
  ) 
  => Sum2' 'True (Alt2' 'Ahead (a ': rest)) a where
    peek2' _ (Cur2 x _)      = Just x
    peek2' _ (Blank2 _ rest) = Nothing
    lft2' _ x                 = Cur2 x blankTail2 

instance 
  (   Sum2 (Alt2' 'Ahead rest) b
    , Typeable a
  )
  => Sum2' 'False (Alt2' 'Ahead (a ': rest)) b where
    peek2' _ (Cur2 x _) = Nothing
    peek2' _ (Blank2 _ rest) = peek2 rest
    lft2' _ x = Blank2 (Proxy :: Proxy a) $ lft2 x


class Project2 (p :: [* -> * -> *]) q m where
  project2 :: m p a b -> Maybe (m q a b)

class Project2' (flag :: Bool) (p :: [* -> * -> *]) q m where
  project2' :: Proxy flag -> m p a b -> Maybe (m q a b)

type family ElemIn2 (a :: * -> * -> *) q where
  ElemIn2 _ '[] = 'False
  ElemIn2 a (a ': rest) = 'True
  ElemIn2 b (a ': rest) = ElemIn2 b rest


instance (ElemIn2 a q ~ flag, Project2' flag (a ': rest) q m) => Project2 (a ': rest) q m where
  project2 = project2' (Proxy :: Proxy flag)

instance Project2 '[] q m where
  project2 _ = Nothing

instance (Project2 rest q (Alt2' 'Ahead)) => Project2' 'False (a ': rest) q (Alt2' 'Ahead) where
  project2' _ (Cur2 x _) = Nothing
  project2' _ (Blank2 _ rest) = project2 rest

instance (Sum2 (Alt2' 'Ahead q) a, Project2 rest q (Alt2' 'Ahead)) 
  => Project2' 'True (a ': rest) q (Alt2' 'Ahead) where
  project2' _ (Cur2 x _) = Just $ lft2 x
  project2' _ (Blank2 _ rest) = project2 rest


class Replace2 p f q m where
  replace2 :: (f a b -> m q a b) -> m p a b -> m (q :++ (p :- (f ': q))) a b

instance 
  (   Project2 q (q :++ (p :- (f ': q))) (Alt2' 'Ahead)
    , Project2 p (q :++ (p :- (f ': q))) (Alt2' 'Ahead)
    , Sum2 (Alt2' Ahead p) f
  ) => Replace2 p f q (Alt2' 'Ahead) where
  replace2 fxn x
    | Just y <- peek2 x,
      Just z <- project2 (fxn y) = z
    | Just z <- project2 x       = z


data F2 a b = F2 deriving Show
data G2 a b = G2 deriving Show
data H2 a b = H2 deriving Show
data K2 a b = K2 deriving Show

cmplG2 :: G2 a b -> Alt2 '[H2, K2] a b 
cmplG2 G2 = lft2 K2



-- | legacy anonymous sum type
data (f :+: g) a b = InL (f a b) | InR (g a b)

instance (Show (f a b), Show (g a b)) => Show ((f :+: g) a b) where
  show (InL x) = show x
  show (InR x) = show x


class Sum2 c s where
  peek2 :: c a b -> Maybe (s a b)
  lft2  :: s a b -> c a b

type (w :>+: a) = (Sum2 w a)

type family (>+) x p where
  (>+) x '[] = NoConstr
  (>+) x (a ': rest) = (x :>+: a, x >+ rest)


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





