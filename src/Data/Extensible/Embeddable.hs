{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Extensible.Embeddable where

import           Control.Monad (mplus)
import           Data.Extensible.Sum
import           Data.Extensible.Sum1
import qualified Data.Extensible.Sum2 as S
import           Data.Extensible.Sum2 ((:>+:), (:+:))

class Embeddable a b where
  embed :: a -> b


instance {-# INCOHERENT #-} (b :>|: a) => Embeddable a b where
  embed = lft

instance {-# INCOHERENT #-} (Embeddable a c, Embeddable b c) => Embeddable (a :|: b) c where
  embed (DataL x) = embed x
  embed (DataR x) = embed x

instance {-# INCOHERENT #-} (b :>||: a) => Embeddable (a x) (b x) where
  embed = lft1

instance {-# INCOHERENT #-} (Embeddable (a x) (c x), Embeddable (b x) (c x)) => Embeddable ((a :||: b) x) (c x) where
  embed (InL x) = embed x
  embed (InR x) = embed x


instance {-# INCOHERENT #-} (b :>+: a) => Embeddable (a x y) (b x y) where
  embed = S.lft2

instance {-# INCOHERENT #-} (Embeddable (a x y) (c x y), Embeddable (b x y) (c x y)) => Embeddable ((a :+: b) x y) (c x y) where
  embed (S.InL x) = embed x
  embed (S.InR x) = embed x



class Projectable a b where
  project :: a -> Maybe b

instance {-# INCOHERENT #-} (b :>|: a) => Projectable b a where
  project = peek

instance {-# INCOHERENT #-} (Projectable c a, Projectable c b) => Projectable c (a :|: b) where
  project y = (DataL <$> project y) `mplus` (DataR <$> project y)




instance {-# INCOHERENT #-} (b :>||: a) => Projectable (b x) (a x) where
  project = peek1

instance {-# INCOHERENT #-} (Projectable (c x) (a x), Projectable (c x) (b x))
  => Projectable (c x) ((a :||: b) x) where
  project y = (InL <$> project y) `mplus` (InR <$> project y)


-- higher kinded projectable class
class Projectable1 a b where
  project1 :: a x -> Maybe (b x)


instance {-# INCOHERENT #-} (b :>||: a) => Projectable1 b a where
  project1 = peek1


instance {-# INCOHERENT #-} (Projectable1 c a, Projectable1 c b)
  => Projectable1 c  (a :||: b) where
  project1 y = (InL <$> project1 y) `mplus` (InR <$> project1 y)
