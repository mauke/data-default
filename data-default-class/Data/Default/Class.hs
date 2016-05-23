{-

Copyright (c) 2013 Lukas Mai

All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.
* Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.
* Neither the name of the author nor the names of his contributors
  may be used to endorse or promote products derived from this software
  without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY LUKAS MAI AND CONTRIBUTORS "AS IS" AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-}

{-# LANGUAGE CPP #-}

#define HAVE_GHC_GENERICS (__GLASGOW_HASKELL__ >= 704)

#if HAVE_GHC_GENERICS
{-# LANGUAGE DefaultSignatures, TypeOperators, FlexibleContexts #-}
#endif

module Data.Default.Class (
-- | This module defines a class for types with a default value.
    Default(..)
) where

#if HAVE_GHC_GENERICS
import GHC.Generics

class GDefault f where
    gdef :: f a

instance GDefault U1 where
    gdef = U1

instance (Default a) => GDefault (K1 i a) where
    gdef = K1 def

instance (GDefault a, GDefault b) => GDefault (a :*: b) where
    gdef = gdef :*: gdef

instance (GDefault a) => GDefault (M1 i c a) where
    gdef = M1 gdef
#endif

-- | A class for types with a default value.
class Default a where
    -- | The default value for this type.
    def :: a

#if HAVE_GHC_GENERICS
    default def :: (Generic a, GDefault (Rep a)) => a
    def = to gdef
#endif
