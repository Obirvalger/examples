import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont

main = flip runContT return $ do
  lift $ putStrLn "alpha"
  (k, num) <- callCC $ \k -> let f x = k (f, x)
                             in return (f, 0)
  lift $ putStrLn "beta"          -- k
  lift $ putStrLn "gamma"         -- j
  if num < 5
  then k (num + 1) >> return ()
  else lift $ print num       -- l
