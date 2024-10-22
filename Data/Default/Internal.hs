{-

Copyright (c) 2013, Lukas Mai

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Lukas Mai nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures, TypeOperators, FlexibleContexts #-}

{-|
Module      : Data.Default.Internal
Description : Implementation details.

Nothing to see here.
-}
module Data.Default.Internal (Default(..), GDefault(..)) where

import Data.Int
import Data.Word
import Data.Monoid
import Data.Ratio
import Data.Complex
import Data.Fixed
import Foreign.C.Types
#if MIN_VERSION_base(4, 18, 0)
import Foreign.C.ConstPtr
#endif
import Foreign.Ptr
import Data.Proxy
import Data.Functor.Identity
import Control.Applicative (Const(..))
#if MIN_VERSION_base(4, 16, 0)
import Data.Tuple
#endif
import qualified Data.Set as S
import qualified Data.Map as M
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Sequence (Seq)
import Data.Tree (Tree(..))

import GHC.Generics

-- | Defaults, generically.
class GDefault f where
    gdef :: f a

-- | A nullary constructor is its own default.
instance GDefault U1 where
    gdef = U1

-- | A unary constructor wraps the default value of the argument type.
instance (Default a) => GDefault (K1 i a) where
    gdef = K1 def

-- | Default of products = product of defaults.
instance (GDefault a, GDefault b) => GDefault (a :*: b) where
    gdef = gdef :*: gdef

-- | For sums, we arbitrarily choose the left side.
instance (GDefault a) => GDefault (a :+: b) where
    gdef = L1 gdef

-- | Default of wrapper = wrapper of default.
instance (GDefault a) => GDefault (M1 i c a) where
    gdef = M1 gdef

-- | A class for types with a default value.
class Default a where
    -- | The default value for this type.
    def :: a

    default def :: (Generic a, GDefault (Rep a)) => a
    def = to gdef

instance Default Bool    where def = False
instance Default Int     where def = 0
instance Default Int8    where def = 0
instance Default Int16   where def = 0
instance Default Int32   where def = 0
instance Default Int64   where def = 0
instance Default Word    where def = 0
instance Default Word8   where def = 0
instance Default Word16  where def = 0
instance Default Word32  where def = 0
instance Default Word64  where def = 0
instance Default Integer where def = 0
instance Default Float   where def = 0
instance Default Double  where def = 0
instance (Integral a) => Default (Ratio a)               where def = 0
instance (Default a, RealFloat a) => Default (Complex a) where def = def :+ def
instance (HasResolution a) => Default (Fixed a)          where def = 0

#if MIN_VERSION_base(4, 10, 0)
instance Default CBool      where def = 0
#endif
instance Default CShort     where def = 0
instance Default CUShort    where def = 0
instance Default CInt       where def = 0
instance Default CUInt      where def = 0
instance Default CLong      where def = 0
instance Default CULong     where def = 0
instance Default CLLong     where def = 0
instance Default CULLong    where def = 0
instance Default CPtrdiff   where def = 0
instance Default CSize      where def = 0
instance Default CSigAtomic where def = 0
instance Default CIntPtr    where def = 0
instance Default CUIntPtr   where def = 0
instance Default CIntMax    where def = 0
instance Default CUIntMax   where def = 0
instance Default CClock     where def = 0
instance Default CTime      where def = 0
instance Default CUSeconds  where def = 0
instance Default CSUSeconds where def = 0
instance Default CFloat     where def = 0
instance Default CDouble    where def = 0

instance Default (Ptr a)      where def = nullPtr
instance Default (FunPtr a)   where def = nullFunPtr
instance Default IntPtr       where def = ptrToIntPtr def
instance Default WordPtr      where def = ptrToWordPtr def
#if MIN_VERSION_base(4, 18, 0)
instance Default (ConstPtr a) where def = ConstPtr def
#endif

instance Default (Maybe a) where def = Nothing
instance (Default a) => Default (Identity a) where def = Identity def
instance (Default a) => Default (Const a b)  where def = Const def

instance Default ()                     where def = mempty
instance Default [a]                    where def = mempty
instance Default Ordering               where def = mempty
instance Default Any                    where def = mempty
instance Default All                    where def = mempty
instance Default (Last a)               where def = mempty
instance Default (First a)              where def = mempty
instance (Num a) => Default (Sum a)     where def = mempty
instance (Num a) => Default (Product a) where def = mempty
instance Default (Endo a)               where def = mempty
instance Default (Proxy a)              where def = mempty
#if MIN_VERSION_base(4, 16, 0)
instance (Default a) => Default (Solo a) where def = pure def
#endif
instance (Default a) => Default (Dual a) where def = Dual def

instance Default (S.Set v)               where def = S.empty
instance Default (M.Map k v)             where def = M.empty
instance Default (IntMap v)              where def = mempty
instance Default IntSet                  where def = mempty
instance Default (Seq a)                 where def = mempty
instance (Default a) => Default (Tree a) where def = Node def []

instance (Default a1, Default a2) => Default (a1, a2) where
    def = (def, def)

instance (Default a1, Default a2, Default a3) => Default (a1, a2, a3) where
    def = (def, def, def)

instance (Default a1, Default a2, Default a3, Default a4) => Default (a1, a2, a3, a4) where
    def = (def, def, def, def)

instance (Default a1, Default a2, Default a3, Default a4, Default a5) => Default (a1, a2, a3, a4, a5) where
    def = (def, def, def, def, def)

instance (Default a1, Default a2, Default a3, Default a4, Default a5, Default a6) => Default (a1, a2, a3, a4, a5, a6) where
    def = (def, def, def, def, def, def)

instance (Default a1, Default a2, Default a3, Default a4, Default a5, Default a6, Default a7) => Default (a1, a2, a3, a4, a5, a6, a7) where
    def = (def, def, def, def, def, def, def)

instance (Default a1, Default a2, Default a3, Default a4, Default a5, Default a6, Default a7, Default a8) => Default (a1, a2, a3, a4, a5, a6, a7, a8) where
    def = (def, def, def, def, def, def, def, def)

instance (Default a1, Default a2, Default a3, Default a4, Default a5, Default a6, Default a7, Default a8, Default a9) => Default (a1, a2, a3, a4, a5, a6, a7, a8, a9) where
    def = (def, def, def, def, def, def, def, def, def)

instance (Default a1, Default a2, Default a3, Default a4, Default a5, Default a6, Default a7, Default a8, Default a9, Default a10) => Default (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) where
    def = (def, def, def, def, def, def, def, def, def, def)

instance (Default a1, Default a2, Default a3, Default a4, Default a5, Default a6, Default a7, Default a8, Default a9, Default a10, Default a11) => Default (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) where
    def = (def, def, def, def, def, def, def, def, def, def, def)

instance (Default a1, Default a2, Default a3, Default a4, Default a5, Default a6, Default a7, Default a8, Default a9, Default a10, Default a11, Default a12) => Default (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) where
    def = (def, def, def, def, def, def, def, def, def, def, def, def)

instance (Default a1, Default a2, Default a3, Default a4, Default a5, Default a6, Default a7, Default a8, Default a9, Default a10, Default a11, Default a12, Default a13) => Default (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) where
    def = (def, def, def, def, def, def, def, def, def, def, def, def, def)

instance (Default a1, Default a2, Default a3, Default a4, Default a5, Default a6, Default a7, Default a8, Default a9, Default a10, Default a11, Default a12, Default a13, Default a14) => Default (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) where
    def = (def, def, def, def, def, def, def, def, def, def, def, def, def, def)

instance (Default a1, Default a2, Default a3, Default a4, Default a5, Default a6, Default a7, Default a8, Default a9, Default a10, Default a11, Default a12, Default a13, Default a14, Default a15) => Default (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) where
    def = (def, def, def, def, def, def, def, def, def, def, def, def, def, def, def)

instance (Default a1, Default a2, Default a3, Default a4, Default a5, Default a6, Default a7, Default a8, Default a9, Default a10, Default a11, Default a12, Default a13, Default a14, Default a15, Default a16) => Default (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16) where
    def = (def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def)

instance (Default a1, Default a2, Default a3, Default a4, Default a5, Default a6, Default a7, Default a8, Default a9, Default a10, Default a11, Default a12, Default a13, Default a14, Default a15, Default a16, Default a17) => Default (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17) where
    def = (def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def)

instance (Default a1, Default a2, Default a3, Default a4, Default a5, Default a6, Default a7, Default a8, Default a9, Default a10, Default a11, Default a12, Default a13, Default a14, Default a15, Default a16, Default a17, Default a18) => Default (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18) where
    def = (def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def)

instance (Default a1, Default a2, Default a3, Default a4, Default a5, Default a6, Default a7, Default a8, Default a9, Default a10, Default a11, Default a12, Default a13, Default a14, Default a15, Default a16, Default a17, Default a18, Default a19) => Default (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19) where
    def = (def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def)

instance (Default a1, Default a2, Default a3, Default a4, Default a5, Default a6, Default a7, Default a8, Default a9, Default a10, Default a11, Default a12, Default a13, Default a14, Default a15, Default a16, Default a17, Default a18, Default a19, Default a20) => Default (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20) where
    def = (def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def)

instance (Default a1, Default a2, Default a3, Default a4, Default a5, Default a6, Default a7, Default a8, Default a9, Default a10, Default a11, Default a12, Default a13, Default a14, Default a15, Default a16, Default a17, Default a18, Default a19, Default a20, Default a21) => Default (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21) where
    def = (def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def)

instance (Default a1, Default a2, Default a3, Default a4, Default a5, Default a6, Default a7, Default a8, Default a9, Default a10, Default a11, Default a12, Default a13, Default a14, Default a15, Default a16, Default a17, Default a18, Default a19, Default a20, Default a21, Default a22) => Default (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22) where
    def = (def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def)

instance (Default a1, Default a2, Default a3, Default a4, Default a5, Default a6, Default a7, Default a8, Default a9, Default a10, Default a11, Default a12, Default a13, Default a14, Default a15, Default a16, Default a17, Default a18, Default a19, Default a20, Default a21, Default a22, Default a23) => Default (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23) where
    def = (def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def)

instance (Default a1, Default a2, Default a3, Default a4, Default a5, Default a6, Default a7, Default a8, Default a9, Default a10, Default a11, Default a12, Default a13, Default a14, Default a15, Default a16, Default a17, Default a18, Default a19, Default a20, Default a21, Default a22, Default a23, Default a24) => Default (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24) where
    def = (def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def)

instance (Default a1, Default a2, Default a3, Default a4, Default a5, Default a6, Default a7, Default a8, Default a9, Default a10, Default a11, Default a12, Default a13, Default a14, Default a15, Default a16, Default a17, Default a18, Default a19, Default a20, Default a21, Default a22, Default a23, Default a24, Default a25) => Default (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25) where
    def = (def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def)

instance (Default a1, Default a2, Default a3, Default a4, Default a5, Default a6, Default a7, Default a8, Default a9, Default a10, Default a11, Default a12, Default a13, Default a14, Default a15, Default a16, Default a17, Default a18, Default a19, Default a20, Default a21, Default a22, Default a23, Default a24, Default a25, Default a26) => Default (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26) where
    def = (def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def)

instance (Default a1, Default a2, Default a3, Default a4, Default a5, Default a6, Default a7, Default a8, Default a9, Default a10, Default a11, Default a12, Default a13, Default a14, Default a15, Default a16, Default a17, Default a18, Default a19, Default a20, Default a21, Default a22, Default a23, Default a24, Default a25, Default a26, Default a27) => Default (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27) where
    def = (def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def)

instance (Default a1, Default a2, Default a3, Default a4, Default a5, Default a6, Default a7, Default a8, Default a9, Default a10, Default a11, Default a12, Default a13, Default a14, Default a15, Default a16, Default a17, Default a18, Default a19, Default a20, Default a21, Default a22, Default a23, Default a24, Default a25, Default a26, Default a27, Default a28) => Default (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28) where
    def = (def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def)

instance (Default a1, Default a2, Default a3, Default a4, Default a5, Default a6, Default a7, Default a8, Default a9, Default a10, Default a11, Default a12, Default a13, Default a14, Default a15, Default a16, Default a17, Default a18, Default a19, Default a20, Default a21, Default a22, Default a23, Default a24, Default a25, Default a26, Default a27, Default a28, Default a29) => Default (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29) where
    def = (def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def)

instance (Default a1, Default a2, Default a3, Default a4, Default a5, Default a6, Default a7, Default a8, Default a9, Default a10, Default a11, Default a12, Default a13, Default a14, Default a15, Default a16, Default a17, Default a18, Default a19, Default a20, Default a21, Default a22, Default a23, Default a24, Default a25, Default a26, Default a27, Default a28, Default a29, Default a30) => Default (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30) where
    def = (def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def)

instance (Default a1, Default a2, Default a3, Default a4, Default a5, Default a6, Default a7, Default a8, Default a9, Default a10, Default a11, Default a12, Default a13, Default a14, Default a15, Default a16, Default a17, Default a18, Default a19, Default a20, Default a21, Default a22, Default a23, Default a24, Default a25, Default a26, Default a27, Default a28, Default a29, Default a30, Default a31) => Default (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31) where
    def = (def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def, def)

