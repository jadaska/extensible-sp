{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module Data.Extensible.Sum where

import Data.Typeable
import Control.Lens(prism', Prism')
import Control.Monad
import Data.Monoid((<>))
import Data.Promotion.Prelude.List
import Data.Promotion.TH
import Data.Typeable
import GHC.Exts(Constraint)
import Unsafe.Coerce(unsafeCoerce)


class SumClass c s where
  peek :: c -> Maybe s
  lft  :: s -> c

-- | SumClass constraint syntactic sugar
type (w :>|: a)  = (SumClass w a)

type NoConstr = (() :: Constraint)

type family (>|) x p where
  (>|) x '[] = NoConstr
  (>|) x (a ': rest) = (x :>|: a, x >| rest)


sumPrism :: (w :>|: a) => Prism' w a 
sumPrism = prism' lft peek

-- | Standard SumClass definitions
instance SumClass a a where
  peek = Just
  lft  = id

instance SumClass (Maybe a) a where
   peek = id
   lft  = Just

instance SumClass (Either a b) b where
   peek (Right x) = Just x
   peek _ = Nothing
   lft  = Right


-- | Alt p  generic sum type parameterized by a type list p

data AltLoc = Behind | Ahead

data Alt' k p where
  Blank  :: Typeable x => Proxy x -> Alt' k xs -> Alt' k (x ': xs)
  Cur    :: Typeable x => x -> Alt' 'Behind xs -> Alt' 'Ahead (x ': xs)
  ANil   :: Alt' 'Behind '[]

type Alt p = Alt' 'Ahead p


instance (Show a, Show (Alt' 'Behind rest), Show (Alt' 'Ahead rest)) => Show (Alt' 'Ahead (a ': rest)) where
  show (Cur x rest) = show x <> " " <> show rest
  show (Blank _ rest) = "* " <> show rest

instance Show (Alt' 'Behind rest) => Show (Alt' 'Behind (a ': rest)) where
  show (Blank p rest) = "* " <> show rest

instance Show (Alt' k '[]) where
  show _ = ""


-- | blank tail/head for handling Alt
class BlankTail p where
  blankTail :: Alt' 'Behind p

instance BlankTail '[] where
  blankTail = ANil

instance (Typeable a, BlankTail rest) => BlankTail (a ': rest) where
  blankTail = Blank (Proxy :: Proxy a) $ blankTail


class BlankHead p where
  blankHead :: Proxy p -> Alt' 'Ahead q -> Alt' 'Ahead (p :++ q)

instance BlankHead '[] where
  blankHead _ y = unsafeCoerce y

instance (Typeable a,  BlankHead rest) => BlankHead (a ': rest) where
  blankHead _ y = unsafeCoerce $ Blank (Proxy :: Proxy a) $ blankHead (Proxy :: Proxy rest) y



-- | Sum class
instance {-# INCOHERENT #-} (Typeable c, BlankTail rest) => SumClass (Alt' 'Ahead (c ': rest)) c where
  peek (Cur x _)   = Just x
  peek (Blank _ _) = Nothing
  lft x = Cur x blankTail 

-- instance Typeable c => SumClass (Alt' 'Ahead '[c]) c where
--    peek (Cur x _)  = Just x
--    lft x = Cur x ANil -- HLast x 

instance {-# INCOHERENT #-} (Typeable a, Typeable c, SumClass (Alt' 'Ahead p) c) => SumClass (Alt' 'Ahead (a ': p)) c where
  peek (Blank _ xs) = peek xs
  lft x = Blank (Proxy :: Proxy a) $ lft x








-- Legacy alternative syntax
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



instance {-# INCOHERENT #-} SumClass (a :|: b) b where
  peek (DataR x) = Just x
  peek _ = Nothing
  lft = DataR

instance {-# INCOHERENT #-} (SumClass c a) => SumClass (c :|: b) a where
  peek (DataL x) = peek x
  peek _ = Nothing
  lft = DataL . lft




