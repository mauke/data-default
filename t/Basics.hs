{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}

module Basics (tests) where

import qualified Distribution.TestSuite as C
import Control.Monad.State.Strict
import Data.Default
import Data.Int
import Data.Word
import Data.Monoid
import Data.Complex
import qualified Data.Sequence as Seq
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.Tree (Tree(..))

data TestInstance = TestInstance
    { result :: Maybe String
    , name :: String
    }

newtype Test a = MkTest (State (Int, [TestInstance]) a)
    deriving (Functor, Applicative, Monad, MonadState (Int, [TestInstance]))

ok :: Bool -> String -> Test ()
ok r n = do
    (c, ts) <- get
    let !c' = c + 1
        t = TestInstance
            { result = if r then Nothing else Just n
            , name = shows c' $ " # " ++ n
            }
    put (c', t : ts)

is :: (Show a, Eq a) => a -> a -> Test ()
is g e = ok (g == e) (show g ++ " == " ++ show e)

execTest :: Test a -> [C.TestInstance]
execTest (MkTest t) = map wrap . reverse . snd $ execState t (0, [])
    where
    wrap TestInstance{ result = r, name = n } = C.TestInstance
        { C.run       = pure $ C.Finished (maybe C.Pass C.Fail r)
        , C.name      = n
        , C.tags      = def
        , C.options   = def
        , C.setOption = \k _ -> Left ("bad option: " ++ show k)
        }

tests :: IO [C.Test]
tests = pure . map C.Test . execTest $ do
    is def ()
    is def (Nothing :: Maybe (Int, Ordering, [Float]))
    is def ""
    is def (S.empty :: S.Set ())
    is def (M.empty :: M.Map () ())
    is def IS.empty
    is def (IM.empty :: IM.IntMap ())
    is def (Seq.empty :: Seq.Seq ())
    is def (Node (0 :: Complex Float) [])
    is def EQ
    is def (Any False)
    is def (All True)
    is def (Last Nothing :: Last ())
    is def (First Nothing :: First ())
    is def (Sum (0 :: Integer))
    is def (Product (1 :: Rational))
    is def (0 :: Int)
    is def (0 :: Integer)
    is def (0 :: Float)
    is def (0 :: Double)
    is def (0 :: Rational)
    is def (0 :: Complex Double)
    is def (0 :: Int8)
    is def (0 :: Int16)
    is def (0 :: Int32)
    is def (0 :: Int64)
    is def (0 :: Word)
    is def (0 :: Word8)
    is def (0 :: Word16)
    is def (0 :: Word32)
    is def (0 :: Word64)
    is def ((def, def) :: ((), Maybe ((), ())))
    is def ((def, def, def) :: ((), Maybe ((), ()), [Ordering]))
    is def ((def, def, def, def) :: ((), Maybe ((), ()), [Ordering], Float))
    is def ((def, def, def, def, def, def, def) :: ((), (), (), (), (), (), ()))
