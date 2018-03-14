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

module Data.Extensible.Sum2F where

import Data.Typeable
import Data.Extensible.Type
import Data.Extensible.Sum(AltLoc(..))
import Data.Monoid
import Data.Promotion.Prelude.List
import Data.Extensible.Product


class Sum2F (c :: (* -> * -> *) -> * -> * -> *) s where
  peek2f :: c f a b -> Maybe (s f a b)
  lft2f  :: s f a b -> c f a b

type (w :>++: a) = (Sum2F w a)



data Alt2F' k (p ::[(* -> * -> *) -> * -> * -> *]) f a b where
  Blank2F :: () -- (Typeable (x :: (* -> * -> *) -> * -> * -> * )) 
    => Proxy x -> Alt2F' k xs f a b -> Alt2F' k (x ': xs) f a b
  Cur2F :: () -- (Typeable x) 
    => x f a b -> Alt2F' 'Behind xs f a b -> Alt2F' 'Ahead (x ': xs) f a b
  ANil2 :: Alt2F' 'Behind '[] f a b 

type Alt2F p f a b = Alt2F' 'Ahead p f a b


instance (Show (a f b c), Show (Alt2F' 'Behind rest f b c), Show (Alt2F' 'Ahead rest f b c)) 
  => Show (Alt2F' 'Ahead (a ': rest) f b c) where
  show (Cur2F x rest) = show x <> " " <> show rest
  show (Blank2F _ rest) = "* " <> show rest

instance Show (Alt2F' 'Behind rest f b c) => Show (Alt2F' 'Behind (a ': rest) f b c) where
  show (Blank2F p rest) = "* " <> show rest

instance Show (Alt2F' k '[] f b c) where
  show _ = ""


-- | blank tail/head for handling Alt
class BlankTail2F p where
  blankTail2f :: Alt2F' 'Behind p f a b

instance BlankTail2F '[] where
  blankTail2f = ANil2

instance (BlankTail2F rest) => BlankTail2F (a ': rest) where
  blankTail2f = Blank2F (Proxy :: Proxy a) $ blankTail2f


-- class BlankHead2 p where
--   blankHead2 :: Proxy p -> Alt2F' 'Ahead q -> Alt2F' 'Ahead (p :++ q)

-- instance BlankHead2 '[] where
--   blankHead _ y = unsafeCoerce y

type family (:~~) (a :: (* -> * -> *) -> * -> * -> *) b where
  (:~~) a a = 'True
  (:~~) a b = 'False

class Sum2F' (flag :: Bool) (c :: (* -> * -> *) -> * -> * -> *) s where
  peek2f' :: Proxy flag -> c f a b -> Maybe (s f a b)
  lft2f'  :: Proxy flag -> s f a b -> c f a b

instance 
  (   (c :~~ d) ~ flag
    , Sum2F' flag (Alt2F' 'Ahead (d ': rest)) c
  ) => Sum2F (Alt2F' 'Ahead (d ': rest)) c where
    peek2f = peek2f' (Proxy :: Proxy flag)
    lft2f = lft2f' (Proxy :: Proxy flag)    

instance 
  (   BlankTail2F rest
    -- , Typeable a
  ) 
  => Sum2F' 'True (Alt2F' 'Ahead (a ': rest)) a where
    peek2f' _ (Cur2F x _)      = Just x
    peek2f' _ (Blank2F _ rest) = Nothing
    lft2f' _ x                 = Cur2F x blankTail2f 

instance 
  (   Sum2F (Alt2F' 'Ahead rest) b
    -- , Typeable a
  )
  => Sum2F' 'False (Alt2F' 'Ahead (a ': rest)) b where
    peek2f' _ (Cur2F x _) = Nothing
    peek2f' _ (Blank2F _ rest) = peek2f rest
    lft2f' _ x = Blank2F (Proxy :: Proxy a) $ lft2f x


-- class Project2 (p :: [* -> * -> *]) q m where
--   project2 :: m p a b -> Maybe (m q a b)

-- class Project2' (flag :: Bool) (p :: [* -> * -> *]) q m where
--   project2' :: Proxy flag -> m p a b -> Maybe (m q a b)

-- type family ElemIn2 (a :: * -> * -> *) q where
--   ElemIn2 _ '[] = 'False
--   ElemIn2 a (a ': rest) = 'True
--   ElemIn2 b (a ': rest) = ElemIn2 b rest


-- instance (ElemIn2 a q ~ flag, Project2' flag (a ': rest) q m) => Project2 (a ': rest) q m where
--   project2 = project2' (Proxy :: Proxy flag)

-- instance Project2 '[] q m where
--   project2 _ = Nothing

-- instance (Project2 rest q (Alt2F' 'Ahead)) => Project2' 'False (a ': rest) q (Alt2F' 'Ahead) where
--   project2' _ (Cur2F x _) = Nothing
--   project2' _ (Blank2F _ rest) = project2 rest

-- instance (Sum2F (Alt2F' 'Ahead q) a, Project2 rest q (Alt2F' 'Ahead)) 
--   => Project2' 'True (a ': rest) q (Alt2F' 'Ahead) where
--   project2' _ (Cur2F x _) = Just $ lft2f x
--   project2' _ (Blank2F _ rest) = project2 rest


-- class Replace2 p f q m where
--   replace2 :: (f a b -> m q a b) -> m p a b -> m (q :++ (p :- (f ': q))) a b

-- instance 
--   (   Project2 q (q :++ (p :- (f ': q))) (Alt2F' 'Ahead)
--     , Project2 p (q :++ (p :- (f ': q))) (Alt2F' 'Ahead)
--     , Sum2F (Alt2F' Ahead p) f
--   ) => Replace2 p f q (Alt2F' 'Ahead) where
--   replace2 fxn x
--     | Just y <- peek2 x,
--       Just z <- project2 (fxn y) = z
--     | Just z <- project2 x       = z