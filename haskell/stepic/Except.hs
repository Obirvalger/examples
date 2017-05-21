module Except where
import Control.Monad (liftM, MonadPlus(mzero, mplus), guard, msum, ap, foldM)
import Control.Applicative (Alternative(empty, (<|>)))

newtype Except e a = Except {runExcept :: Either e a} deriving Show

except :: Either e a -> Except e a
except = Except

instance Functor (Except e) where
  fmap = liftM

instance Applicative (Except e) where
  pure = return
  (<*>) = ap

instance Monoid e => Alternative (Except e) where
  empty = mzero
  (<|>) = mplus

instance Monad (Except e) where
  return a = Except (Right a)
  m >>= k  = 
    case runExcept m of
      Left e  -> Except (Left e)
      Right x -> k x

instance Monoid e => MonadPlus (Except e) where
  mzero = Except (Left mempty)
  Except x `mplus` Except y = Except $
    case x of
      Left e -> either (Left . mappend e) Right y
      r      -> r

throwE :: e -> Except e a
throwE = except . Left

catchE :: Except e a -> (e -> Except e' a) -> Except e' a
m `catchE` h = 
  case runExcept m of
    Left e  -> h e
    Right r -> except (Right r) 

withExcept :: (e -> e') -> Except e a -> Except e' a
withExcept f e = case runExcept e of
                 (Left l)  -> except $ Left (f l)
                 (Right r) -> except $ Right r

data ListIndexError = ErrIndexTooLarge Int | ErrNegativeIndex 
  deriving (Eq, Show)

infixl 9 !!!
(!!!) :: [a] -> Int -> Except ListIndexError a
(!!!) xs n | n < 0     = except (Left ErrNegativeIndex)
           | otherwise = get xs n n where
             get [] m n     = except (Left (ErrIndexTooLarge n))
             get (y:_) 0 n  = except (Right y)
             get (_:ys) m n = get ys (m-1) n

data ReadError = EmptyInput | NoParse String deriving Show

tryRead :: Read a => String -> Except ReadError a
tryRead "" = except (Left EmptyInput)
tryRead s  = case reads s of
               ((x,""):ys) -> except (Right x)
               _           -> except (Left (NoParse s))

data SumError = SumError Int ReadError
  deriving Show

trySum :: (Read a, Num a) => [String] -> Except SumError a
trySum = fmap sum . traverse (\(n,s) -> withExcept (SumError n) (tryRead s)) . zip [1..]
{-trySum = foldM fun 0 . zip [1..] . map tryRead  where-}
  {-fun y (n, e)  = case runExcept e of-}
                    {-Right r -> except (Right (r+y))-}
                    {-Left  l -> throwE (SumError n l)-}


newtype SimpleError = Simple {getSimple :: String} deriving (Eq, Show)

instance Monoid SimpleError where
  mempty = Simple ""
  mappend (Simple s1) (Simple s2) = Simple (s1 ++ s2)

lie2se :: ListIndexError -> SimpleError
lie2se ErrNegativeIndex     = Simple "[negative index]"
lie2se (ErrIndexTooLarge i) = Simple $ "[index (" ++ show i ++ ") is too large]"


newtype Validate e a = Validate { getValidate :: Either [e] a }

instance Functor (Validate e) where
  fmap = liftM

instance Applicative (Validate e) where
  pure = return
  (Validate (Left fl)) <*> (Validate (Left xl)) = Validate (Left (fl ++ xl))
  f <*> x = ap f x

instance Monad (Validate e) where
  return a = Validate (Right a)
  m >>= k  = 
    case getValidate m of
      Left v  -> Validate (Left v)
      Right x -> k x

collectE :: Except e a -> Validate e a
collectE = Validate . either (Left . (:[])) Right . runExcept
{-collectE (Except (Left l))  = Validate (Left [l]) -}
{-collectE (Except (Right r)) = Validate (Right r)-}

validateSum :: [String] -> Validate SumError Integer
{-validateSum = undefined-}
validateSum = fmap sum . traverse (\(n,s) -> collectE $ withExcept (SumError n) (tryRead s)) . zip [1..]
