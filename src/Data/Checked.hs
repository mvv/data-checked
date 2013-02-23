{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Type-indexed runtime-checked properties.
module Data.Checked
  ( Checked
  , trustMe
  , checked
  , Property(..)
  , maybeHolds
  , check
  ) where

-- | Wrapper-evidence for property /p/.
newtype Checked p v = Checked v

-- | Use when the property can be deduced without a runtime check.
trustMe ∷ v → Checked p v
trustMe = Checked
{-# INLINE trustMe #-}

-- | Unwrap the checked value.
checked ∷ Checked p v → v
checked (Checked v) = v
{-# INLINE checked #-}

class Property p v where
  -- | Test if the property holds for the given value.
  --   The first argument is supposed to be ignored.
  holds ∷ p → v → Bool

-- | Return 'Just' /v/ if /p/ holds and 'Nothing' overwise.
maybeHolds ∷ Property p v ⇒ p → v → Maybe v
maybeHolds p v | holds p v = Just v
               | otherwise = Nothing
{-# INLINABLE maybeHolds #-}

-- | Wrap the value if the property holds.
check ∷ ∀ p v . Property p v ⇒ v → Maybe (Checked p v)
check v | holds (undefined ∷ p) v = Just (Checked v)
        | otherwise               = Nothing
{-# INLINABLE check #-}

