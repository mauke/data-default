module Data.Default (
-- | This module defines a class for types with a default value. Instances are
-- provided for '()', 'S.Set', 'M.Map', 'Int', 'Integer', 'Float', 'Double',
-- and many others (see below).
    Default(..)
) where

import Data.Monoid
import Data.Ratio
import qualified Data.Set as S
import qualified Data.Map as M
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Sequence (Seq)
import Data.Tree (Tree(..))
import Data.DList (DList)
import System.Locale

-- | A class for types with a default value.
class Default a where
    -- | The default value for this type.
    def :: a

instance Default (S.Set v) where def = S.empty
instance Default (M.Map k v) where def = M.empty
instance Default (IntMap v) where def = mempty
instance Default IntSet where def = mempty
instance Default (Seq a) where def = mempty
instance (Default a) => Default (Tree a) where def = Node def def

instance Default Int where def = 0
instance Default Integer where def = 0
instance Default Float where def = 0
instance Default Double where def = 0
instance (Integral a) => Default (Ratio a) where def = 0

instance (Default r) => Default (e -> r) where def = const def
instance (Default a) => Default (IO a) where def = return def

instance Default (Maybe a) where def = Nothing

instance Default () where def = mempty
instance Default [a] where def = mempty
instance Default Ordering where def = mempty
instance Default Any where def = mempty
instance Default All where def = mempty
instance Default (Last a) where def = mempty
instance Default (First a) where def = mempty
instance (Num a) => Default (Sum a) where def = mempty
instance (Num a) => Default (Product a) where def = mempty
instance Default (Endo a) where def = mempty

instance Default (DList a) where def = mempty

instance (Default a) => Default (Dual a) where def = Dual def
instance (Default a, Default b) => Default (a, b) where def = (def, def)
instance (Default a, Default b, Default c) => Default (a, b, c) where def = (def, def, def)
instance (Default a, Default b, Default c, Default d) => Default (a, b, c, d) where def = (def, def, def, def)
instance (Default a, Default b, Default c, Default d, Default e) => Default (a, b, c, d, e) where def = (def, def, def, def, def)

instance Default TimeLocale where def = defaultTimeLocale
