{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

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
import Data.Functor.Identity
import Control.Applicative
import Data.Proxy
#if MIN_VERSION_base(4, 16, 0)
import Data.Tuple
#endif
import GHC.Generics
import Foreign.C.Types
import Foreign.Ptr
#if MIN_VERSION_base(4, 18, 0)
import Foreign.C.ConstPtr
#endif

import Control.Monad (when, join)
import Control.Monad.Reader
import Data.IORef
import System.Exit (exitFailure)
import System.IO

data TestState = TestState
    { testState_count :: !(IORef Int)
    , testState_ok    :: !(IORef Bool)
    }

newtype Test a = Test{ unTest :: ReaderT TestState IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader TestState)

runTest :: (MonadIO m) => Test a -> m a
runTest t = liftIO $ do
    hSetBuffering stdout LineBuffering
    rc <- newIORef 0
    rk <- newIORef True
    x <- runReaderT (unTest t) TestState{ testState_count = rc, testState_ok = rk }
    c <- readIORef rc
    putStrLn $ "1.." ++ show c
    k <- readIORef rk
    when (not k) exitFailure
    pure x


instance (Default a) => Default (Test a) where
    def = return def

withRef :: (IORef Int -> IO () -> IO a) -> Test a
withRef f = do
    TestState rc rk <- ask
    liftIO (f rc (atomicWriteIORef rk False))

ok :: Bool -> String -> Test ()
ok b s = withRef $ \ref lose -> do
    c <- atomicModifyIORef' ref (join (,) . succ)
    putStrLn $ (if b then "" else "not ") ++ "ok " ++ show c ++ " - " ++ s
    when (not b) lose

is {-, isNot-} :: (Show a, Eq a) => a -> a -> Test ()
is    x y = ok (x == y) (show x ++ " == " ++ show y)
-- isNot x y = ok (x /= y) (show x ++ " /= " ++ show y)

-- diag :: String -> Test ()
-- diag s = liftIO $ do
--     putStrLn $ "# " ++ s

data T0 a b
    = C0 a a
    | C1
    | C2 b
    deriving (Eq, Show, Generic)

instance (Default a) => Default (T0 a b)

main :: IO ()
main = runTest $ do
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
    is def (Identity ())
    is def (Const 0 :: Const Int Char)
    is def (Proxy :: Proxy Char)
#if MIN_VERSION_base(4, 16, 0)
    is def (pure () :: Solo ())
#endif
    is def False
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
    is def (0 :: CShort)
    is def (0 :: CUShort)
    is def (0 :: CInt)
    is def (0 :: CUInt)
    is def (0 :: CLong)
    is def (0 :: CULong)
    is def (0 :: CLLong)
    is def (0 :: CULLong)
    is def (0 :: CPtrdiff)
    is def (0 :: CSize)
    is def (0 :: CSigAtomic)
    is def (0 :: CIntPtr)
    is def (0 :: CUIntPtr)
    is def (0 :: CIntMax)
    is def (0 :: CUIntMax)
    is def (0 :: CClock)
    is def (0 :: CTime)
    is def (0 :: CUSeconds)
    is def (0 :: CSUSeconds)
    is def (0 :: CFloat)
    is def (0 :: CDouble)
    is def (0 :: IntPtr)
    is def (0 :: WordPtr)
#if MIN_VERSION_base(4, 10, 0)
    is def (0 :: CBool)
#endif
    is def nullPtr
    is def nullFunPtr
#if MIN_VERSION_base(4, 18, 0)
    is def (ConstPtr nullPtr)
#endif
    is def ((def, def) :: ((), Maybe ((), ())))
    is def ((def, def, def) :: ((), Maybe ((), ()), [Ordering]))
    is def ((def, def, def, def) :: ((), Maybe ((), ()), [Ordering], Float))
    is def ((def, def, def, def, def, def, def) :: ((), (), (), (), (), (), ()))
    is def (C0 0 0 :: T0 Int Char)
