{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Vinyl.Sugar
  (
  -- * Record-construction sugar
    rec_
  -- * Extending the syntax sugar
  , NamedField(..)
  , RecSugarTy(..)
  -- * Auxiliary types
  , FieldLabel(..)
  ) where

import Data.Kind
import Foreign.Storable (Storable)
import GHC.OverloadedLabels
import GHC.TypeLits

import qualified Data.Vinyl.ARec as V
import qualified Data.Vinyl.Core as V
import qualified Data.Vinyl.Functor as V
import qualified Data.Vinyl.SRec as V
import qualified Data.Vinyl.TypeLevel as V

-- | A simple proxy for type-level strings.
data FieldLabel (s :: Symbol) = FieldLabel
  deriving (Eq, Ord, Enum)

instance s ~ s' => IsLabel s (FieldLabel s') where
  fromLabel = FieldLabel

{- |
Specifies how to convert a labeled value into an element of a
vinyl record. The only default instance is for @'Vinyl.ElField'@,
but you can define additional instances for your own interpretation
functors.
-}
class NamedField (s :: Symbol) (r :: k) (f :: k -> Type) (a :: Type)
  | r -> s
  , r f -> a
  where
  {- |
  @
  toNamedField :: KnownSymbol s => a -> ElField '(s, a)
  @
  -}
  toNamedField :: a -> f r

instance KnownSymbol s => NamedField s '(s, a) V.ElField a where
  toNamedField = V.Field

{- |
The workhorse class of this package. Implements a polyvariadic
function that builds up a @'V.Rec'@ and converts it into
the desired output type.

You can enable @rec_@ syntax for your own types by writing additional
@RecSugarTy@ instances, adding a base case to @'rec''@. Example:

@
data MyFunctor r -- elided

newtype MyRec rs = MyRec { unMyRec :: 'V.Rec' MyFunctor rs }

instance 'RecSugarTy' ('V.Rec' MyFunctor rs) (MyRec rs) where
  rec' = MyRec
-}
class RecSugarTy e o | o -> e where
  rec' :: e -> o

instance RecSugarTy (V.Rec f rs) (V.Rec f rs) where
  rec' = id

instance (NamedField s r f a, RecSugarTy (V.Rec f (r:rs)) o, lbl ~ FieldLabel s) => RecSugarTy (V.Rec f rs) (lbl -> a -> o) where
  rec' rs _lbl a = rec' (toNamedField a V.:& rs)

instance V.NatToInt (V.RLength rs) => RecSugarTy (V.Rec f rs) (V.ARec f rs) where
  rec' = V.toARec

instance Storable (V.Rec f rs) => RecSugarTy (V.Rec f rs) (V.SRec f rs) where
  rec' = V.toSRec

{-|
Provides "syntax sugar" for constructing @vinyl@ records with
named fields, using GHC's OverloadedLabels extension.

john :: 'V.Rec' 'V.ElField' '[("name", String), ("age", Int)]
john = rec_
  #name "John Doe"
  #age 30

-}
rec_ :: RecSugarTy (V.Rec f '[]) o => o
rec_ = rec' V.RNil