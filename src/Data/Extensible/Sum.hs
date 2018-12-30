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
import GHC.Exts(Constraint)
import Data.Extensible.Type
import Unsafe.Coerce(unsafeCoerce)


class SumClass c s where
  peek :: c -> Maybe s
  lft  :: s -> c

-- | SumClass constraint syntactic sugar
type (w :>|: a)  = (SumClass w a)


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

data Alt' (constr :: * -> Constraint) k p where
  Blank  :: (constr x, Typeable x) => Proxy x -> Alt' constr k xs -> Alt' constr k (x ': xs)
  Cur    :: (constr x, Typeable x) => x -> Alt' constr 'Behind xs -> Alt' constr 'Ahead (x ': xs)
  ANil   :: Alt' constr 'Behind '[]


class Empty x
instance Empty x


type Alt p = Alt' Empty 'Ahead p


instance (Show a, Show (Alt' constr 'Behind rest), Show (Alt' constr 'Ahead rest))
  => Show (Alt' constr 'Ahead (a ': rest)) where
  show (Cur x rest) = show x <> " " <> show rest
  show (Blank _ rest) = "* " <> show rest

instance Show (Alt' constr 'Behind rest) => Show (Alt' constr 'Behind (a ': rest)) where
  show (Blank _ rest) = "* " <> show rest

instance Show (Alt' constr k '[]) where
  show _ = ""


-- | blank tail/head for handling Alt
class BlankTail constr p where
  blankTail :: Alt' constr 'Behind p

instance BlankTail constr '[] where
  blankTail = ANil

instance (constr a, Typeable a, BlankTail constr rest) => BlankTail constr (a ': rest) where
  blankTail = Blank (Proxy :: Proxy a) $ blankTail


class BlankHead constr p where
  blankHead :: Proxy p -> Alt' constr 'Ahead q -> Alt' constr 'Ahead (p :++ q)

instance BlankHead constr '[] where
  blankHead _ y = unsafeCoerce y

instance (constr a, Typeable a,  BlankHead constr rest) => BlankHead constr (a ': rest) where
  blankHead _ y = unsafeCoerce $ Blank (Proxy :: Proxy a) $ blankHead (Proxy :: Proxy rest) y


-- | Sum class
instance {-# INCOHERENT #-} (constr c, Typeable c, BlankTail constr rest)
  => SumClass (Alt' constr 'Ahead (c ': rest)) c where
  peek (Cur x _)   = Just x
  peek (Blank _ _) = Nothing
  lft x = Cur x blankTail

-- instance Typeable c => SumClass (Alt' 'Ahead '[c]) c where
--    peek (Cur x _)  = Just x
--    lft x = Cur x ANil -- HLast x

instance {-# INCOHERENT #-}
  ( constr a
  , Typeable a
  , constr c
  , Typeable c
  , SumClass (Alt' constr 'Ahead p) c
  ) => SumClass (Alt' constr 'Ahead (a ': p)) c where
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
