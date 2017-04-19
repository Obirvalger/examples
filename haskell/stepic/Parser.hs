import Control.Applicative
import Data.Maybe
import Data.Char

newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }

anyChr :: Prs Char
anyChr = Prs f where
  f ""     = Nothing
  f (c:cs) = Just (c, cs)

satisfy :: (Char -> Bool) -> Prs Char
satisfy pr = Prs fun where
  fun ""     = Nothing
  fun (c:cs) | pr c      = Just (c, cs)
             | otherwise = Nothing

char :: Char -> Prs Char
char c = satisfy (== c)

instance Functor Prs where
  fmap f p = Prs fun where
    fun s = case runPrs p s of
      Nothing        -> Nothing
      (Just (a, s')) -> Just (f a, s')

digit :: Prs Int
digit = digitToInt <$> satisfy isDigit

instance Applicative Prs where
  pure a = Prs (\s -> Just (a, s))
  pf <*> pv = Prs fun where
    fun s = case runPrs pf s of
      Nothing        -> Nothing
      (Just (f, s')) -> case runPrs pv s' of
        Nothing         -> Nothing
        (Just (a, s'')) -> Just (f a, s'')

mult :: Prs Int
mult = (*) <$> nat <* char '*' <*> nat

instance Alternative Prs where
  empty = Prs (const Nothing)
  p <|> q = Prs f where
    f s = let ps = runPrs p s
      in if isNothing ps
      {-in if (ps == Nothing)-}
         then runPrs q s
         else ps 

many1 :: Prs a -> Prs [a]
many1 p = (:) <$> p <*> many1 p <|> (: []) <$> p 

nat :: Prs Int
nat = toNat <$> many1 digit where
  toNat = sum . zipWith (*) (map (10^) [0..]) . reverse


newtype PrsE a = PrsE { runPrsE :: String -> Either String (a, String) }

instance Functor PrsE where
  fmap f p = PrsE fun where
    fun s = case runPrsE p s of
      (Left l)        -> Left l
      (Right (a, s')) -> Right (f a, s')

instance Applicative PrsE where
  pure a = PrsE (\s -> Right (a, s))
  pf <*> pv = PrsE fun where
    fun s = case runPrsE pf s of
      (Left l1)       -> Left l1
      (Right (f, s')) -> case runPrsE pv s' of
        (Left l2)        -> Left l2
        (Right (a, s'')) -> Right (f a, s'')

