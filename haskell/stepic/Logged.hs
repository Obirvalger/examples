import Control.Monad (liftM, ap)
import Data.Functor.Identity
data Logged a = Logged String a deriving (Eq,Show)

newtype LoggT m a = LoggT { runLoggT :: m (Logged a) }

instance Monad m => Functor (LoggT m) where
  fmap = liftM

instance Monad m => Applicative (LoggT m) where
  pure = return
  f <*> x = ap f x

instance Monad m => Monad (LoggT m) where
  return x = LoggT $ return $ Logged "" x
  m >>= k  = LoggT $ do
    (Logged s1 x) <- runLoggT m
    (Logged s2 y) <- runLoggT (k x)
    return $ Logged (s1 ++ s2) y
  fail = LoggT . fail

logTst :: LoggT Identity Integer
logTst = do 
  x <- LoggT $ Identity $ Logged "AAA" 30
  let y = 10
  z <- LoggT $ Identity $ Logged "BBB" 2
  return $ x + y + z
  
failTst :: [Integer] -> LoggT [] Integer
failTst xs = do
  5 <- LoggT $ fmap (Logged "") xs
  LoggT [Logged "A" ()]
  return 42
