import Control.Monad
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Data.Char (toUpper)
import Data.List (partition)

type MyRWT m a = ReaderT [String]
                 (WriterT String m)
                 a

runMyRWT :: MyRWT m a -> [String] -> m (a, String)
runMyRWT r e = runWriterT (runReaderT r e)

myAsks :: Monad m => ([String] -> a) -> MyRWT m a
myAsks = asks

myTell :: Monad m => String -> MyRWT m ()
myTell = lift . tell

myLift :: Monad m => m a -> MyRWT m a
myLift = lift . lift

logFirstAndRetSecond :: MyRWT IO String
logFirstAndRetSecond = do
  el1 <- myAsks head
  myLift $ putStrLn $ "First is " ++ show el1
  el2 <- myAsks (map toUpper . head . tail)
  myLift $ putStrLn $ "Second is " ++ show el2
  myTell el1
  return el2


veryComplexComputation :: MyRWT Maybe (String, String)
veryComplexComputation = do
  xs <- myAsks (fun ([], []))
  case xs of
    (e2:e1:o2:o1:_) -> myTell (e1 ++ ',':o1) >> return (map toUpper e2, map toUpper o2)
    []              -> myLift Nothing

fun :: ([String], [String]) -> [String] -> [String]
fun (es,os) ss | length es == 2 && length os == 2 = es ++ os
               | null ss = []
               | otherwise = let (s,ts) = (head ss, tail ss) in
                                 if even $ length s
                                 then if length es < 2
                                      then fun (s:es, os) ts 
                                      else fun (es, os) ts 
                                 else if length os < 2
                                      then fun (es, s:os) ts 
                                      else fun (es, os) ts 


tickCollatz :: State Integer Integer
tickCollatz = do
  n <- get
  let res = if odd n then 3 * n + 1 else n `div` 2
  put res
  return n

type EsSi = ExceptT String (State Integer)

runEsSi :: EsSi a -> Integer -> (Either String a, Integer)
runEsSi e = runState (runExceptT e)

go :: Integer -> Integer -> State Integer Integer -> EsSi ()
go l u s = do
  m <- lift get
  let n = execState s m
  lift $ put n
  when (n > u) $ throwE "Upper bound"
  when (n < l) $ throwE "Lower bound"
  return ()
  {-if (n > u)-}
  {-then throwE $ "Upper" ++ show n-}
  {-else if n < l-}
       {-then throwE  $ "Lower"-}
       {-else return ()-}
  
