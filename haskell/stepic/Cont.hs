{-# LANGUAGE RankNTypes#-}
import Control.Monad (liftM, MonadPlus(mzero, mplus), guard, msum, ap, when)
import Data.Char (intToDigit, digitToInt)
import Except

newtype Cont r a = Cont {runCont :: (a -> r) -> r}

evalCont :: Cont r r -> r
evalCont m = runCont m id

instance Functor (Cont e) where
  fmap = liftM

instance Applicative (Cont e) where
  pure = return
  (<*>) = ap

instance Monad (Cont r) where
  return a = Cont $ \c -> c a
  Cont v >>= k = Cont $ \c -> v (\a -> runCont (k a) c)

sumIt :: Cont [r] Integer
sumIt = do
  a <- Cont $ \c -> c 3
  b <- Cont $ \c -> c 4 ++ c 5
  return $ a + b

myf :: (Ord r, Num r) => Int -> Cont r Int
myf x = do
  Cont $ \c -> min (c 1 + 5) 100
  Cont $ \c -> c 1 * 2
  Cont $ \c -> c 1 ^ 3
  Cont $ \c -> c x - 1
  {-return x-}

type Checkpointed a = forall m . Monad m => (a -> m a) -> m a
{-type Checkpointed a = forall r . (a -> Cont r a) -> Cont r a-}
{-type Checkpointed a = (a -> Cont a a) -> Cont a a-}

checkpointed :: (a -> Bool) -> Checkpointed a -> Cont a a
checkpointed p a = a $ \b ->
    Cont $ \c -> let x = c b in
        if (p (c b)) then x else b

{-checkpointed :: (a -> Bool) -> Checkpointed a -> Cont a a-}
{-checkpointed p a = a $ \b -> do-}
    {-Cont $ \c -> if not (p (c b)) then b else c b-}

{-evalCont (checkpointed  (< 10) $ addTens 1)-}

addTens :: Int -> Checkpointed Int
addTens x1  = \checkpoint -> do
  checkpoint x1
  let x2 = x1 + 10
  checkpoint x2     {- x2 = x1 + 10 -}
  let x3 = x2 + 10
  checkpoint x3     {- x3 = x1 + 20 -}
  let x4 = x3 + 10
  return x4         {- x4 = x1 + 30 -}

addTens' x1 pr = callCC $ \exit ->
  if pr x1 
  then exit x1
  else let x2 = x1 + 10 in
           if pr x2
           then exit x2
           else return (x2 + 10)

addTens'' :: Int -> (Int -> Bool) -> Cont r Int
addTens'' x1 pr = callCC $ \exit -> do
  let p = not . pr
  when (p x1) $ exit x1
  let x2 = x1 + 10
  when (p x2) $ exit x2
  let x3 = x2 + 10
  when (p x3) $ exit x3
  let x4 = x3 + 10
  return x4

callCC :: ((a -> Cont r a1) -> Cont r a) -> Cont r a
callCC f = Cont $ \h -> runCont (f (\a -> Cont $ \_ -> h a)) h

foo :: Int -> Cont r String
foo x = callCC $ \k -> do
    let y = x ^ 2 + 3
    when (y > 20) $ k "over twenty"
    return (show $ y - 4)

bar :: Char -> String -> Cont r Int
bar c s = do
    msg <- callCC $ \k -> do
        let s0 = c : s
        when (s0 == "hello") $ k "They say hello."
        let s1 = show s0
        return ("They appear to be saying " ++ s1)
    return (length msg)

fun :: Int -> String
fun n = (`runCont` id) $ do
    str <- callCC $ \exit1 -> do                            -- define "exit1"
        when (n < 10) (exit1 (show n))
        let ns = map digitToInt (show (n `div` 2))
        n' <- callCC $ \exit2 -> do                         -- define "exit2"
            when (length ns < 3) (exit2 (length ns))
            when (length ns < 5) (exit2 n)
            when (length ns < 7) $ do
                let ns' = map intToDigit (reverse ns)
                exit1 (dropWhile (=='0') ns')               --escape 2 levels
            return $ sum ns
        return $ "(ns = " ++ show ns ++ ") " ++ show n'
    return $ "Answer: " ++ str

newtype FailCont r e a = FailCont {runFailCont :: (a -> r) -> (a -> e) -> r}
{-newtype Cont r a = Cont {runCont :: (a -> r) -> r}-}

instance Functor (FailCont r e) where
  fmap = liftM

instance Applicative (FailCont r e) where
  pure = return
  (<*>) = ap

{-instance Monad (Cont r) where-}
  {-return a = Cont $ \c -> c a-}
  {-Cont v >>= k = Cont $ \c -> v (\a -> runCont (k a) c)-}

instance Monad (FailCont r e) where
  return x = FailCont $ \ok err -> ok x
  FailCont f >>= k = FailCont $ \ok err -> f (\a -> runFailCont (k a) ok err) (\a -> err a)
  {-FailCont f >>= k = FailCont $ \ok err -> runFailCont (k (f z w)) ok err-}

add x y = FailCont $ \ok _ -> ok $ x + y
{-add' x y = \ok _ -> ok $ x + y-}

{-toFailCont :: Except ReadError r -> FailCont r ReadError a-}
{-toFailCont e = case runExcept e of-}
  {-Left l  -> FailCont $ \_ err -> err l-}
  {-Right r -> FailCont $ \ok _ -> ok r-}

{-evalFailCont :: FailCont (Either e a) e a -> Either e a-}
{-evalFailCont (FailCont f) = -}

{-addInts :: Read r => String -> String -> FailCont r ReadError Int-}
{-addInts s1 s2 = do-}
  {-i1 <- toFailCont $ tryRead s1-}
  {-i2 <- toFailCont $ tryRead s2-}
  {-return $ i1 + i2-}
