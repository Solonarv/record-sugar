{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
module Example where

import Data.Vinyl.Core
import Data.Vinyl.Functor
import Data.Vinyl.Sugar

john :: Rec ElField ['("name", String), '("age", Int)]
john = rec_
  #age 30
  #name "John Doe"

john' :: Rec ElField ['("bornAt", String), '("name", String), '("age", Int)]
john' = extend_ john
  #bornAt "Zurich, Switzerland"