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
  , lens
  , view
  , (%)
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
  , (<#=)
  , (<+=)
  , (<-=)
  , (<^=)
  , (<|=)
  , (<&=)
  , _head
  , _tail
  ) where

import Data.Kind
import Data.Bits
import Control.Monad.State
import Data.Functor.Const
import Data.Functor.Identity
import qualified Data.Sequence as Seq

class Profunctor p where
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d

  lmap :: (a -> b) -> p b c -> p a c
  lmap f = dimap f id
  
  rmap :: (b -> c) -> p a b -> p a c
  rmap f = dimap id f
  
class Contravariant f where
  contramap :: f a -> f b

data Exchange a b s t where
  Exchange :: (s -> a) -> (b -> t) -> Exchange a b s t

instance Profunctor (Exchange a b) where
  dimap f g (Exchange sa bt) = Exchange (sa . f) (g . bt)

instance Functor (Exchange a b s) where
  fmap f (Exchange sa bt) = Exchange sa (f . bt)

type Mono o s a = o s s a a

type Iso s t a b = forall p f. (Profunctor p, Functor f) => p a (f b) -> p s (f t)

type AnIso s t a b = Exchange a b a (Identity b) -> Exchange a b s (Identity s)

type Lens s t a b = forall f. Functor f => (a -> f b) -> (s -> f t)

type Setter s t a b = (a -> Identity b) -> (s -> Identity t)

type Getter r s a = (a -> Const r a) -> (s -> Const r s)

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso sa bt = dimap sa (fmap bt)

{-# INLINE iso #-}

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
