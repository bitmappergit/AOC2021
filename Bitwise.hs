{-# LANGUAGE LiberalTypeSynonyms #-}

module Bitwise
  ( bit0
  , bit1
  , castBit
  , Bit(..)
  , IsBit(..)
  , asBit
  , Bitwise(..)
  , bitAt
  ) where

import Optics
import Data.Bits as Bits
import Data.Text as Text
import Data.Sequence as Seq

bit0 :: IsBit a => a
bit0 = fromBit O

bit1 :: IsBit a => a
bit1 = fromBit I

data Bit = O | I
  deriving (Show, Eq, Enum) 

class IsBit a where
  toBit :: a -> Bit
  fromBit :: Bit -> a

castBit :: (IsBit a, IsBit b) => a -> b
castBit = fromBit . toBit

instance IsBit Bit where
  toBit = id
  fromBit = id

instance IsBit Bool where
  toBit True = I
  toBit False = O
  
  {-# INLINE toBit #-}

  fromBit I = True
  fromBit O = False

  {-# INLINE fromBit #-}

instance IsBit Int where
  toBit 1 = I
  toBit 0 = O
  toBit n = error ("toBit: The number " <> show n <> " is not 1 or 0!")

  {-# INLINE toBit #-}

  fromBit I = 1
  fromBit O = 0

  {-# INLINE fromBit #-}

instance IsBit Char where
  toBit '1' = I
  toBit '0' = O
  toBit c = error ("toBit: the char " <> show c <> " is not '1' or '0'!")

  {-# INLINE toBit #-}

  fromBit I = '1'
  fromBit O = '0'

  {-# INLINE fromBit #-}

asBit :: IsBit a => Simple Iso a Bit
asBit = iso toBit fromBit

class IsBit b => Bitwise a b where
  putBit :: IsBit b => Int -> a -> b -> a
  getBit :: IsBit b => Int -> a -> b 

instance {-# OVERLAPPABLE #-} (IsBit b, Bits a) => Bitwise a b where
  putBit idx bits bit =
    case toBit bit of
      I -> setBit bits idx
      O -> clearBit bits idx

  getBit idx bits =
    castBit (testBit bits idx)

instance (IsBit b, Indexable a Int) => Bitwise (a b) b where
  putBit idx bits bit =
    bits & at idx .~ bit
  
  getBit idx bits =
    bits % at idx

bitAt :: Bitwise a b => Int -> Simple Lens a b 
bitAt idx = lens (getBit idx) (putBit idx)
