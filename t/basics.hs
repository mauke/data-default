{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Default
import Data.Int
import Data.Word
import Data.Monoid
import Data.Complex
import System.Locale
import qualified Data.Sequence as Seq
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.Tree (Tree(..))

import Control.Monad.Reader
import Data.IORef
import System.IO
import Text.Printf.Mauke

newtype Test a = Test{ unTest :: ReaderT (IORef Int) IO a }
    deriving (Functor, Monad, MonadIO, MonadReader (IORef Int))

runTest :: (MonadIO m) => Test a -> m a
runTest t = liftIO $ do
    hSetBuffering stdout LineBuffering
    r <- newIORef 1
    runReaderT (unTest t) r

instance (Default a) => Default (Test a) where
    def = return def

withRef :: (IORef Int -> IO a) -> Test a
withRef f = do
    r <- ask
    liftIO (f r)

planTests :: Int -> Test ()
planTests n = liftIO $ do
    printf "1..%d\n" n

ok :: Bool -> String -> Test ()
ok b s = withRef $ \r -> do
    c <- atomicModifyIORef r ((,) =<< succ)
    printf "%sok %d - %s\n" (if b then "" else "not ") c s

is {-, isNot-} :: (Show a, Eq a) => a -> a -> Test ()
is    x y = ok (x == y) (printf "%s == %s" (show x) (show y))
-- isNot x y = ok (x /= y) (printf "%s /= %s" (show x) (show y))

-- diag :: String -> Test ()
-- diag s = liftIO $ do
--     printf "# %s\n" s

main :: IO ()
main = runTest $ do
    planTests 36
    sequence_ [def, liftIO def, return ()]
    is (def length) (0 :: Int)
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
    is def defaultTimeLocale
