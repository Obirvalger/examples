import Control.Applicative
import Data.Maybe
import Data.Char
import Data.Either

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


newtype PrsEP a = PrsEP { runPrsEP :: Int -> String -> (Int, Either String (a, String)) }

parseEP :: PrsEP a -> String -> Either String (a, String)
parseEP p  = snd . runPrsEP p 0

instance Functor PrsEP where
  fmap f p = PrsEP fun where
    fun i s = case runPrsEP p i s of
      (n, Left l)        -> (n, Left l)
      (n, Right (a, s')) -> (n, Right (f a, s'))

instance Applicative PrsEP where
  pure a = PrsEP (\i s -> (i, Right (a, s)))
  pf <*> pv = PrsEP fun where
    fun i s = case runPrsEP pf i s of
      (n1, Left l1)       -> (n1, Left l1)
      (n1, Right (f, s')) -> case runPrsEP pv n1 s' of
        (n2, Left l2)        -> (n2, Left l2)
        (n2, Right (a, s'')) -> (n2, Right (f a, s''))

instance Alternative PrsEP where
  empty = PrsEP (\_ _ ->  (0, Left "pos 0: empty alternative"))
  p <|> q = PrsEP f where
    f i s = let ps = runPrsEP p i s
                qs = runPrsEP q i s
            in case ps of
               (n1, Left l1)  -> case qs of
                                 (n2, Right r2) -> qs
                                 (n2, Left l2)  -> if n1 >= n2 then ps else qs
               (n1, Right r1) -> ps
                          
            {-in if fst ps >= fst qs -}
               {-then if not (isLeft (snd ps) && isRight (snd qs))-}
                    {-then ps-}
                    {-else qs -}
               {-else if not (isLeft (snd qs) && isRight (snd ps))-}
                    {-then qs-}
                    {-else ps-}

satisfyEP :: (Char -> Bool) -> PrsEP Char
satisfyEP pr = PrsEP fun where
  fun i ""     = (succ i, Left $ "pos " ++ show (succ i) ++ ": unexpected end of input")
  fun i (c:cs) | pr c      = (i + 1, Right (c, cs))
               | otherwise = (succ i, Left $ "pos " ++ show (succ i) ++ ": unexpected " ++ [c])

charEP c = satisfyEP (== c)
charEP' = fmap (: []) . charEP

tripleP [a,b,c] = (\x y z -> [x,y,z]) <$> charEP a <*> charEP b <*>  charEP c

