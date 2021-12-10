{-# LANGUAGE RankNTypes
           , BlockArguments
           , GADTs
           , MultiParamTypeClasses
           , OverloadedLabels
           , DataKinds
           , PolyKinds
           , FunctionalDependencies
           , AllowAmbiguousTypes
           , TypeApplications
           , ScopedTypeVariables
           , FlexibleInstances
           , TypeOperators
           , LiberalTypeSynonyms
           , ConstraintKinds
           , TupleSections
           , DefaultSignatures
           , DeriveFunctor
           , TypeFamilies
           #-}

module Optics
  ( Distributive(..)
  , Profunctor(..)
  , Strong(..)
  , Choice(..)
  , Closed(..)
  , Semigroupal(..)
  , Monoidal(..)
  , Bifunctor(..)
  , Contravariant(..)
  , Bicontravariant(..)
  , Traversing(..)
  , Mapping(..)
  , Iso
  , Lens
  , Simple
  , AffineTraversal
  , Prism
  , iso
  , lens
  , prism
  , affineTraversal

  , refract
  , re

  , view

  , (%)
  , (&)

  , over
  , set
  , use
  , traverseOf
  , each
  
  , (%=)
  , (.=)

  , (+=)
  , (-=)
  , (*=)

  , (%~)
  , (.~)

  , (+~)
  , (-~)
  , (*~)

  , Indexable(..)
  , at
  --  , bitAt
  -- , bits
  , _Just
  , _Left
  , _Right
  , _head
  , _tail
  , _fst
  , _snd
  , swapped
  , flipped
  , reversed
  , identity
  , asSeq
  , asText
  , charAt
  ) where

import Data.Bifunctor
import Data.Text as Text
import Data.List as List
import Data.Tuple as Tuple
import Data.Sequence as Seq
import Data.Map as Map
import Control.Monad.State as State
import Data.Functor.Identity
import Data.Functor.Const
import Data.Function
import Data.Coerce

-- Interfaces

class Functor g => Distributive g where
  distribute :: Functor f => f (g a) -> g (f a)

  {-# INLINE collect #-}
  
  collect :: Functor f => (a -> g b) -> f a -> g (f b)
  collect f = distribute . fmap f
  
class Profunctor p where
  promap :: (a -> b) -> (c -> d) -> p b c -> p a d
 
  {-# INLINE lmap #-}

  lmap :: (a -> b) -> p b c -> p a c
  lmap f = promap f id
  
  {-# INLINE rmap #-}

  rmap :: (b -> c) -> p a b -> p a c
  rmap f = promap id f

class Profunctor p => Strong p where
  profirst :: p a b -> p (a, c) (b, c)

  prosecond :: p a b -> p (c, a) (c, b)

class Profunctor p => Choice p where
  proleft :: p a b -> p (Either a c) (Either b c)

  proright :: p a b -> p (Either c a) (Either c b)

class Profunctor p => Closed p where
  closed :: p a b -> p (c -> a) (c -> b)

class Profunctor p => Semigroupal p where
  mult :: p a c -> p b d -> p (a, b) (c, d)
  
class Semigroupal p => Monoidal p where
  unit :: p a a

class Contravariant f where
  contramap :: (a -> b) -> f b -> f a

class Bicontravariant f where
  bicontramap :: (a -> b) -> (c -> d) -> f b d -> f a c

class (Choice p, Monoidal p) => Traversing p where
  wander :: (forall f. Applicative f => (a -> f b) -> s -> f t) -> p a b -> p s t

class (Traversing p, Closed p) => Mapping p where
  mapping :: Functor f => p a b -> p (f a) (f b)

  roam :: (forall f. (Applicative f, Distributive f) => (a -> f b) -> (s -> f t)) -> p a b -> p s t

instance Distributive Identity where
  {-# INLINE distribute #-}

  distribute = Identity . fmap runIdentity

-- Wrapper Types

type Id a = a

newtype Star f a b
  = Star { runStar :: a -> f b
         }

newtype Tagged a b
  = Tagged { runTagged :: b
           }

newtype Forget r a b
  = Forget { runForget :: a -> r
           }

newtype Recycle r a b
  = Recycle { runRecycle :: a -> Either b r
            }

data Exchange a b s t
  = Exchange { unwrap :: s -> a
             , wrap :: b -> t
             }

-- (->) Implementations

instance Profunctor (->) where
  {-# INLINE promap #-}

  promap f g p = g . p . f

instance Strong (->) where
  {-# INLINE profirst #-}

  profirst f (a, c) = (f a, c)

  {-# INLINE prosecond #-}

  prosecond f (a, c) = (a, f c)

instance Choice (->) where
  {-# INLINE proright #-}

  proright f = either Left (Right . f)

  {-# INLINE proleft #-}

  proleft f = either (Left . f) Right

instance Semigroupal (->) where
  {-# INLINE mult #-}

  mult ab cd (a, c) = (ab a, cd c)

instance Monoidal (->) where
  {-# INLINE unit #-}

  unit = id

instance Traversing (->) where
  {-# INLINE wander #-}

  wander f ab = runIdentity . f (Identity . ab)

instance Distributive ((->) a) where
  {-# INLINE distribute #-}

  distribute fga = \f -> fmap ($ f) fga

instance Closed (->) where
  {-# INLINE closed #-}

  closed f = \ca c -> f (ca c)

instance Mapping (->) where
  {-# INLINE roam #-}

  roam f g s = runIdentity (f (Identity . g) s)

  {-# INLINE mapping #-}

  mapping = fmap

-- Star Implementations

instance Functor f => Profunctor (Star f) where
  {-# INLINE promap #-}

  promap f g (Star h) = Star (fmap g . h . f)

instance Functor f => Strong (Star f) where
  {-# INLINE profirst #-}

  profirst (Star f) = Star \(a, c) -> fmap (, c) (f a)

  {-# INLINE prosecond #-}

  prosecond (Star f) = Star \(c, b) -> fmap (c ,) (f b)

instance Applicative f => Choice (Star f) where
  {-# INLINE proleft #-}

  proleft (Star f) = Star (either (fmap Left . f) (fmap Right . pure))

  {-# INLINE proright #-}

  proright (Star f) = Star (either (fmap Left . pure) (fmap Right . f))

instance Applicative f => Semigroupal (Star f) where
  {-# INLINE mult #-}

  mult (Star f) (Star g) = Star \(a, c) -> (,) <$> f a <*> g c

instance Applicative f => Monoidal (Star f) where
  {-# INLINE unit #-}

  unit = Star (pure . id)

instance Applicative f => Traversing (Star f) where
  {-# INLINE wander #-}

  wander f (Star g) = Star (f g)

-- Tagged Implementations

instance Profunctor Tagged where
  {-# INLINE promap #-}

  promap _ g (Tagged a) = Tagged (g a)

instance Choice Tagged where
  {-# INLINE proleft #-}

  proleft (Tagged a) = Tagged (Left a)

  {-# INLINE proright #-}

  proright (Tagged a) = Tagged (Right a)

-- Forget Implementations

instance Profunctor (Forget r) where
  {-# INLINE promap #-}

  promap f _ (Forget p) = Forget (p . f)

instance Strong (Forget r) where
  {-# INLINE profirst #-}

  profirst (Forget p) = Forget \(a, _) -> p a

  {-# INLINE prosecond #-}

  prosecond (Forget p) = Forget \(_, b) -> p b

instance Monoid r => Choice (Forget r) where
  {-# INLINE proleft #-}

  proleft (Forget p) = Forget (either p (const mempty))

  {-# INLINE proright #-}

  proright (Forget p) = Forget (either (const mempty) p)

instance Semigroup r => Semigroupal (Forget r) where
  {-# INLINE mult #-}

  mult (Forget p) (Forget q) = Forget \(a, b) -> p a <> q b

instance Monoid r => Monoidal (Forget r) where
  {-# INLINE unit #-}

  unit = Forget (const mempty)

instance Bicontravariant (Forget r) where
  {-# INLINE bicontramap #-}

  bicontramap f _ (Forget p) = Forget (p . f)

instance Monoid r => Traversing (Forget r) where
  {-# INLINE wander #-}

  wander f (Forget g) = Forget (getConst . f (Const . g))

-- Recycle Implementations

instance Profunctor (Recycle r) where
  {-# INLINE promap #-}

  promap f g (Recycle p) = Recycle (first g . p . f)

instance Strong (Recycle r) where
  {-# INLINE profirst #-}

  profirst (Recycle p) = Recycle \(a, c) -> first (, c) (p a)

  {-# INLINE prosecond #-}

  prosecond (Recycle p) = Recycle \(b, d) -> first (b, ) (p d) -- (, b) (p d)
  
instance Choice (Recycle r) where
  {-# INLINE proleft #-}

  proleft (Recycle p) = Recycle (shift . first p)
    where shift (Right c) = Left (Right c)
          shift (Left (Right r)) = Right r
          shift (Left (Left b)) = Left (Left b)
          {-# INLINE shift #-}

  {-# INLINE proright #-}

  proright (Recycle p) = Recycle (shift . second p)
    where shift (Left c) = Left (Left c)
          shift (Right (Left b)) = Left (Right b)
          shift (Right (Right r)) = Right r
          {-# INLINE shift #-}

-- Exchange Implementations

instance Profunctor (Exchange a b) where
  {-# INLINE promap #-}

  promap f g (Exchange unwrap wrap) =
    Exchange { unwrap = unwrap . f
             , wrap = g . wrap
             }

instance Functor (Exchange a b s) where
  {-# INLINE fmap #-}

  fmap f (Exchange unwrap wrap) =
    Exchange { unwrap = unwrap
             , wrap = f . wrap
             }

-- Helper Types

type Simple f s a = f s s a a

-- Optic

type Optic p s t a b = p a b -> p s t

-- Iso, and Lens

type Iso s t a b = forall p. Profunctor p => Optic p s t a b
type Lens s t a b = forall p. Strong p => Optic p s t a b

-- Prism, AffineTraversal and Traversal

type Prism s t a b = forall p. Choice p => Optic p s t a b
type AffineTraversal s t a b = forall p. (Choice p, Strong p) => Optic p s t a b
type Traversal s t a b = forall p. Traversing p => Optic p s t a b

-- Getter and Setter

type Getter s a = forall p. (Bicontravariant p, Strong p) => Optic p s s a a
type Setter s t a b = forall p. (Strong p, Mapping p) => Optic p s t a b

-- Constructors
{-# INLINE iso #-}

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso unwrap wrap = promap unwrap wrap

{-# INLINE lens #-}

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens getter setter = promap get set . profirst
  where get s = (getter s, s)
        {-# INLINE get #-}
        set (b, s) = setter s b
        {-# INLINE set #-}

{-# INLINE prism #-}

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism unwrap wrap = promap wrap (either id unwrap) . proright

{-# INLINE affineTraversal #-}

affineTraversal :: (s -> Either t a) -> (s -> b -> t) -> AffineTraversal s t a b
affineTraversal getter setter = promap get set . profirst . proright
  where get s = (getter s, s)
        {-# INLINE get #-}
        set (bt, s) = either id (setter s) bt
        {-# INLINE set #-}

-- Combinators

{-# INLINE re #-}

re :: Iso s t a b -> Iso b a t s
re o | Exchange sa bt <- o (Exchange id id) = promap bt sa

{-# INLINE refract #-}

refract :: Optic (Recycle a) s t a b -> s -> Either t a
refract o = runRecycle (o (Recycle Right))

{-# INLINE _Just #-}

_Just :: Prism (Maybe a) (Maybe b) a b
_Just = prism Just (maybe (Left Nothing) Right)

{-# INLINE _Left #-}

_Left :: Prism (Either a r) (Either b r) a b
_Left = prism Left (either Right (Left . Right))

{-# INLINE _Right #-}

_Right :: Prism (Either l a) (Either l b) a b
_Right = prism Right (either (Left . Left) Right)

{-# INLINE view #-}

view :: Getter s a -> s -> a
view o s = runIdentity (runForget (coerce (o (Forget Identity))) s)

{-# INLINE over #-}

over :: Setter s t a b -> (a -> b) -> s -> t
over o = o

{-# INLINE set #-}

set :: Setter s t a b -> s -> b -> t
set o s b = over o (const b) s

use :: MonadState s m => Getter s a -> m a
use o = gets (view o)

{-# INLINE traverseOf #-}

traverseOf :: Applicative f => Traversal s t a b -> (a -> f b) -> s -> f t
traverseOf o afb = runStar (o (Star afb))

{-# INLINE each #-}

each :: Traversable f => Traversal (f a) (f b) a b
each = wander traverse

infixl 8 %, ?

{-# INLINE (%) #-}

(%) :: s -> Getter s a -> a
(%) v o = view o v

{-# INLINE (?) #-}

(?) :: s -> Prism s t a b -> Either t a
(?) v o = refract o v

infix 4 %~, .~, +~, -~, *~

{-# INLINE (%~) #-}

(%~) :: Setter s t a b -> (a -> b) -> s -> t
(%~) o = over o

{-# INLINE (.~) #-}

(.~) :: Setter s t a b -> b -> s -> t
(.~) o = over o . const

{-# INLINE (+~) #-}
{-# INLINE (-~) #-}
{-# INLINE (*~) #-}

(+~), (-~), (*~) :: Num a => Setter s t a a -> a -> s -> t
(+~) o n = over o \a -> a + n
(-~) o n = over o \a -> a - n
(*~) o n = over o \a -> a * n

infix 4 %=, .=, +=, -=, *=

{-# INLINE (%=) #-}

(%=) :: MonadState s m => Setter s s a b -> (a -> b) -> m ()
(%=) o = modify . (o %~)

{-# INLINE (.=) #-}

(.=) :: MonadState s m => Setter s s a b -> b -> m ()
(.=) o = modify . (o .~)

{-# INLINE (+=) #-}
{-# INLINE (-=) #-}
{-# INLINE (*=) #-}

(+=), (-=), (*=) :: (MonadState s m, Num a) => Simple Setter s a -> a -> m ()
(+=) o = modify . (o +~)
(-=) o = modify . (o -~)
(*=) o = modify . (o *~)

-- Optics

{-# INLINE _fst #-}

_fst :: Lens (a, b) (a', b) a a'
_fst = lens Tuple.fst \(_, b) a -> (a, b)

{-# INLINE _snd #-}

_snd :: Lens (a, b) (a, b') b b'
_snd = lens Tuple.snd \(a, _) b -> (a, b)

{-# INLINE _head #-}

_head :: Simple Lens [a] a
_head = lens List.head \(_ : xs) x -> x : xs

{-# INLINE _tail #-}

_tail :: Simple Lens [a] [a]
_tail = lens List.tail \(x : _) xs -> x : xs

{-# INLINE swapped #-}

swapped :: Simple Iso (a, b) (b, a)
swapped = iso swap swap

{-# INLINE flipped #-}

flipped :: Simple Iso (a -> b -> c) (b -> a -> c)
flipped = iso flip flip

{-# INLINE reversed #-}

reversed :: Simple Iso [a] [a]
reversed = iso List.reverse List.reverse

{-# INLINE identity #-}

identity :: Simple Iso a a
identity = id

class Indexable c i where
  getAt :: i -> c a -> a
  putAt :: i -> c a -> a -> c a

instance Indexable Seq Int where
  {-# INLINE getAt #-}

  getAt idx val = Seq.index val idx
  
  {-# INLINE putAt #-}

  putAt idx val new = Seq.update idx new val

instance Indexable [] Int where
  {-# INLINE getAt #-}

  getAt idx val = val % asSeq.at idx

  {-# INLINE putAt #-}

  putAt idx val new = val & asSeq.at idx .~ new

instance Ord k => Indexable (Map k) k where
  {-# INLINE getAt #-}

  getAt idx val = val Map.! idx

  {-# INLINE putAt #-}

  putAt idx val new = Map.insert idx new val

{-# INLINE at #-}

at :: Indexable c i => i -> Simple Lens (c a) a
at idx = lens (getAt idx) (putAt idx)

{-# INLINE asSeq #-}

asSeq :: Simple Iso [a] (Seq a)
asSeq = iso Seq.fromList (List.foldr (:) [])

{-# INLINE asText #-}

asText :: Simple Iso String Text
asText = iso Text.pack Text.unpack

{-# INLINE charAt #-}

charAt :: Int -> Simple Lens Text Char
charAt idx = lens indexText updateText
  where indexText str = Text.index str idx
        updateText str new = do
          let (l, r) = Text.splitAt idx str
          let updated = r & Text.cons new . Text.tail
          l <> updated
