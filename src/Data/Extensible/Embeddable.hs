{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Extensible.Embeddable where

import Data.Extensible.Sum
import Data.Extensible.Sum1
import Data.Extensible.Sum2((:>+:), (:+:))
import qualified Data.Extensible.Sum2 as S

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

