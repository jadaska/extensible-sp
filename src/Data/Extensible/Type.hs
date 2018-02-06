{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Extensible.Type where

import GHC.Exts(Constraint)

class TypeCastK (k :: [*] -> *) a b where
	typeCastK :: k a -> k b 

instance TypeCastK k a a where
	typeCastK = id


class TypeCast a b where
	typeCast :: a -> b

instance TypeCast a a where
	typeCast = id


type NoConstr = (() :: Constraint)