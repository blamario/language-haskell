{-# Language TypeData #-}

module Data.Kind where

-- | The kind of types with lifted values. For example @Int :: Type@.
type data Type

-- | The kind of lifted constraints
type data Constraint


{-|The builtin function type, written in infix form as @a % m -> b@.
   Values of this type are functions taking inputs of type @a@ and
   producing outputs of type @b@. The multiplicity of the input is
   @m@.

   Note that @'FUN' m a b@ permits representation polymorphism in both
   @a@ and @b@, so that types like @'Int#' -> 'Int#'@ can still be
   well-kinded.
  -}
data FUN m a b
