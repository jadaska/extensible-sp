{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}



-- | Provides a simple example of how the extensible-sp is
-- | employed.  Two functions foo and bar both define an
-- | independent sum constraint.  foobar aggregates them.
module Data.Extensible.Example where


import Data.Extensible.Sum
import Data.Maybe

-- | reverse a string (if it is present)
foo :: (a :>|: String) => a -> a
foo x = fromMaybe x $ do 
  (s :: String) <- peek x
  return $ lft $ reverse s

-- | add one to the integer
bar :: (a :>|: Int) => a -> a
bar x = fromMaybe x $ do 
  (i :: Int) <- peek x
  return $ lft $ i + 1


-- | Fix a concrete type for the extensible sum 
-- | and utilize the two polymorphic functions 
-- | foo and bar.
foobar :: (String :|: Int) -> (String :|: Int)
foobar = foo . bar

-- | The concrete type need not be the minimal sum
foobar' :: (Char :|: String :|: Int :|: ()) 
        -> (Char :|: String :|: Int :|: ())
foobar' = foo . bar

-- | run the example
runExample :: IO ()
runExample = do 
  let s = "hello" :: String
  let i = 1 :: Int

  print $ foobar (lft s)
  print $ foobar (lft i)