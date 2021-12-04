{-# LANGUAGE RankNTypes
           , BlockArguments
           , GADTs
           , MultiParamTypeClasses
           , LiberalTypeSynonyms
           #-}

module Lens
  ( Profunctor(..)
  , Contravariant(..)
  , Iso
  , AnIso
  , Lens
  , Mono
  , iso
  , from
  , lens
  , view
  , (%)
  , (&)
  , over
  , set
  , use
  , ($=)
  , (#=)
  , (+=)
  , (-=)
  , (*=)
  , (^=)
  , (|=)
  , (&=)
  , ($~)
  , (#~)
  , (+~)
  , (-~)
  , (*~)
  , (^~)
  , (|~)
  , (&~)
  , Indexable(..)
  , at
  , bitAt
  , bits 
  , (<#=)
  , (<+=)
  , (<-=)
  , (<^=)
  , (<|=)
  , (<&=)
  , _head
  , _tail
  , _fst
  , _snd
  ) where

import Data.Kind
import Data.Bits
import Control.Monad.State
import Data.Functor.Const
import Data.Functor.Identity
import qualified Data.Sequence as Seq
import Data.Function

class Profunctor p where
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d

  lmap :: (a -> b) -> p b c -> p a c
  lmap f = dimap f id
  
  rmap :: (b -> c) -> p a b -> p a c
  rmap f = dimap id f

instance Profunctor (->) where
  dimap f g p = g . p . f

class Contravariant f where
  contramap :: f a -> f b

data Exchange a b s t
  = Exchange { unwrap :: s -> a
             , wrap :: b -> t
             }

instance Profunctor (Exchange a b) where
  dimap f g (Exchange unwrap wrap) =
    Exchange { unwrap = unwrap . f
             , wrap = g . wrap
             }

instance Functor (Exchange a b s) where
  fmap f (Exchange unwrap wrap) =
    Exchange { unwrap = unwrap
             , wrap = f . wrap
             }

type Mono o s a = o s s a a

type Iso s t a b = forall p f. (Profunctor p, Functor f) => p a (f b) -> p s (f t)

type AnIso s t a b = Exchange a b a (Identity b) -> Exchange a b s (Identity t)

type Lens s t a b = forall f. Functor f => (a -> f b) -> (s -> f t)

type Setter s t a b = (a -> Identity b) -> (s -> Identity t)

type Getter r s a = (a -> Const r a) -> (s -> Const r s)

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso sa bt = dimap sa (fmap bt)

{-# INLINE iso #-}

from :: AnIso s t a b -> Iso b a t s
from l | Exchange sa bt <- l (Exchange id Identity) = dimap (runIdentity . bt) (fmap sa)

{-# INLINE from #-}

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens sa sbt afb s = sbt s <$> afb (sa s)

{-# INLINE lens #-}

view :: Getter a s a -> s -> a
view l = getConst . l Const

{-# INLINE view #-}

infixl 8 %

(%) :: s -> Getter a s a -> a
(%) v l = view l v

{-# INLINE (%) #-}

over :: Setter s t a b -> (a -> b) -> s -> t
over l f = runIdentity . l (Identity . f)

{-# INLINE over #-}

set :: Setter s t a b -> b -> s -> t
set l a = runIdentity . l (Identity . const a)

{-# INLINE set #-}

use :: MonadState s m => Getter a s a -> m a
use l = gets $ getConst . l Const

{-# INLINE use #-}

infix 4 $=, #=, +=, -=, *=, ^=, |=, &=
infix 4 $~, #~, +~, -~, *~, ^~, |~, &~
infixr 2 <#=, <+=, <-=, <^=, <|=, <&=

($=) :: MonadState s m => Setter s s a b -> (a -> b) -> m ()
($=) l f = modify $ over l f

{-# INLINE ($=) #-}

(#=) :: MonadState s m => Setter s s a b -> b -> m ()
(#=) l v = modify $ over l $ const v

{-# INLINE (#=) #-}

(+=) :: (Num a, MonadState s m) => Setter s s a a -> a -> m ()
(+=) l v = modify $ over l \x -> x + v

{-# INLINE (+=) #-}

(-=) :: (Num a, MonadState s m) => Setter s s a a -> a -> m ()
(-=) l v = modify $ over l \x -> x - v

{-# INLINE (-=) #-}

(*=) :: (Num a, MonadState s m) => Setter s s a a -> a -> m ()
(*=) l v = modify $ over l \x -> x * v

{-# INLINE (*=) #-}

(^=) :: (Bits a, MonadState s m) => Setter s s a a -> a -> m ()
(^=) l v = modify $ over l (xor v)

{-# INLINE (^=) #-}

(|=) :: (Bits a, MonadState s m) => Setter s s a a -> a -> m ()
(|=) l v = modify $ over l (v .|.)

{-# INLINE (|=) #-}

(&=) :: (Bits a, MonadState s m) => Setter s s a a -> a -> m ()
(&=) l v = modify $ over l (v .&.)

{-# INLINE (&=) #-}

($~) :: Setter s t a b -> (a -> b) -> s -> t
($~) l f = over l f

{-# INLINE ($~) #-}

(#~) :: Setter s t a b -> b -> s -> t
(#~) l v = over l $ const v

{-# INLINE (#~) #-}

(+~) :: Num a => Setter s t a a -> a -> s -> t
(+~) l v = over l \x -> x + v

{-# INLINE (+~) #-}

(-~) :: Num a => Setter s t a a -> a -> s -> t
(-~) l v = over l \x -> x - v

{-# INLINE (-~) #-}

(*~) :: Num a => Setter s t a a -> a -> s -> t
(*~) l v = over l \x -> x * v

{-# INLINE (*~) #-}

(^~) :: Bits a => Setter s t a a -> a -> s -> t
(^~) l v = over l (xor v)

{-# INLINE (^~) #-}

(|~) :: Bits a => Setter s t a a -> a -> s -> t
(|~) l v = over l (v .|.)

{-# INLINE (|~) #-}

(&~) :: Bits a => Setter s t a a -> a -> s -> t
(&~) l v = over l (v .&.)

{-# INLINE (&~) #-}

(<#=) :: MonadState s m => Setter s s a b -> m b -> m ()
(<#=) l v = v >>= (l #=)

{-# INLINE (<#=) #-}

(<+=) :: (Num a, MonadState s m) => Setter s s a a -> m a -> m ()
(<+=) l v = modify =<< over l <$> fmap (+) v

{-# INLINE (<+=) #-}

(<-=) :: (Num a, MonadState s m) => Setter s s a a -> m a -> m ()
(<-=) l v = modify =<< over l <$> fmap (-) v

{-# INLINE (<-=) #-}

(<^=) :: (Bits a, MonadState s m) => Setter s s a a -> m a -> m ()
(<^=) l v = modify =<< over l <$> fmap xor v

{-# INLINE (<^=) #-}

(<|=) :: (Bits a, MonadState s m) => Setter s s a a -> m a -> m ()
(<|=) l v = modify =<< over l <$> fmap (.|.) v

{-# INLINE (<|=) #-}

(<&=) :: (Bits a, MonadState s m) => Setter s s a a -> m a -> m ()
(<&=) l v = modify =<< over l <$> fmap (.&.) v

{-# INLINE (<&=) #-}

class Indexable c i where
  getAt :: i -> c a -> a
  putAt :: i -> c a -> a -> c a

instance Indexable Seq.Seq Int where
  getAt idx val = Seq.index val idx
  {-# INLINE getAt #-}
  putAt idx old new = Seq.update idx new old
  {-# INLINE putAt #-}

at :: Indexable c i => i -> Mono Lens (c a) a
at idx = lens (getAt idx) (putAt idx)

{-# INLINE at #-}

bitAt :: Bits a => Int -> Mono Lens a Bool
bitAt idx = lens getter setter
  where getter bits = testBit bits idx
        setter bits bit = if bit then setBit bits idx else clearBit bits idx

{-# INLINE bitAt #-}

bits :: Mono Iso Int [Bool]
bits = iso (reverse . from) (to . reverse)
  where from 0 = []
        from n = not (mod n 2 == 0) : from (div n 2)
        to = foldl (\a (i, v) -> a & bitAt i #~ v) 0 . zip [0..]

{-# INLINE bits #-}

_head :: Mono Lens [a] a
_head = lens getter setter
  where getter (x : xs) = x
        setter (x : xs) y = y : xs

{-# INLINE _head #-}

_tail :: Mono Lens [a] [a]
_tail = lens getter setter
  where getter (x : xs) = xs
        setter (x : xs) ys = x : ys

{-# INLINE _tail #-}

_fst :: Lens (a, c) (b, c) a b
_fst = lens fst \(a, c) b -> (b, c)

{-# INLINE _fst #-}

_snd :: Lens (c, a) (c, b) a b
_snd = lens snd \(c, a) b -> (c, b)

{-# INLINE _snd #-}
