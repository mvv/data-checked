-- | Type-indexed runtime-checked properties.
module Data.Checked
  ( Checked
  , trustThat
  , preserving
  , checked
  , Property
  , property
  , maybeHolds
  , check
  , I
  , U
  , pand
  , (~&&)
  , por
  , (~||)
  ) where

import Control.DeepSeq (NFData(..))

-- | Wrapper-evidence for property /p/.
newtype Checked p v = Checked v

instance NFData v => NFData (Checked p v) where
  rnf (Checked v) = rnf v

-- | Use when the property can be deduced without a runtime check.
trustThat :: p -> v -> Checked p v
trustThat _ = Checked
{-# INLINE trustThat #-}

-- | Apply a fuction that preserves the property to the checked value.
preserving :: p -> (v -> v) -> Checked p v -> Checked p v
preserving _ f (Checked v) = Checked (f v)
{-# INLINE preserving #-}

-- | Unwrap the checked value.
checked :: Checked p v -> v
checked (Checked v) = v
{-# INLINE checked #-}

newtype Property p v = Property {
  -- | Test if the property holds for the given value.
  --   The first argument is supposed to be ignored.
    holds :: v -> Bool }

property :: (v -> Bool) -> Property p v
property = Property

-- | Return 'Just' /v/ if /p/ holds and 'Nothing' overwise.
maybeHolds :: Property p v -> v -> Maybe v
maybeHolds p v | holds p v = Just v
               | otherwise = Nothing
{-# INLINABLE maybeHolds #-}

-- | Wrap the value if the property holds.
check :: Property p v -> v -> Maybe (Checked p v)
check p v | holds p v = Just (Checked v)
          | otherwise = Nothing
{-# INLINABLE check #-}

-- | Property intersection
newtype I p1 p2 = I (p1, p2)

-- | Property union
newtype U p1 p2 = U (p1, p2)

-- | Property intersect (p1 AND p2). p1 is checked first
pand :: Property p1 v -> Property p2 v -> Property (I p1 p2) v
pand p1 p2 = Property (\v -> holds p1 v && holds p2 v)
{-# INLINABLE pand #-}

-- | Synonym for pand
(~&&) :: Property p1 v -> Property p2 v -> Property (I p1 p2) v
(~&&) = pand
{-# INLINABLE (~&&) #-}

-- | Property union (c1 OR c2)
por :: Property p1 v -> Property p2 v -> Property (U p1 p2) v
por p1 p2 = Property (\v -> holds p1 v || holds p2 v)
{-# INLINABLE por #-}

-- | Synonym for por
(~||) :: Property p1 v -> Property p2 v -> Property (U p1 p2) v
(~||) = por
{-# INLINABLE (~||) #-}

