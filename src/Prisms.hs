{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds  #-}

module Prisms ((?), (##), (##?), (#?), (#!), (#.), (#^.), (^#~)) where

import GHC.Exts (TYPE)
import Data.Maybe   ()
import Data.Monoid  ( First )
import Control.Monad.Representable.Reader
import Control.Lens ( (^?), (^.), re, view, from, lens, withLens, Getting, Getter, AReview, AnIso, Iso, Prism, Lens, ALens) 

data Ternary a = a :? a

infixl 0 ?
infixl 1 :?

(?) :: Bool -> Ternary a -> a
True  ? (x :? _) = x
False ? (_ :? y) = y

(##)  :: AReview t s -> s -> t
x ## y = y ^. re x

(#?)  :: Getting (First a) s a -> s -> Maybe a 
x #? y = y ^? x 

(#!) :: Getter s a -> s -> a
x #! y = view x y

(#.) :: AnIso s t a b -> Iso b a t s 
x #. y = from x y 

(##?) :: Getting (First a) s a -> s -> Bool
x ##? y = case (y ^? x) of
    Nothing        -> False
    Just _         -> True
    
(#^.) :: s -> Getting a s a -> a 
x #^. y = x ^. y 

(^#~) :: (s -> a) -> (s -> b -> t) -> Lens s t a b
g ^#~ s = lens g s

(.^#) :: forall s t a b rep (r :: TYPE rep) . ALens s t a b -> ((s -> a) -> (s -> b -> t) -> r) -> r 
x .^# o = withLens x o 