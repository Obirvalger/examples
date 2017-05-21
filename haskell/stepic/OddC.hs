data OddC a = Un a | Bi a a (OddC a) deriving (Eq,Show)

cnt1 = Un 42
cnt3 = Bi 1 2 cnt1
cnt5 = Bi 3 4 cnt3

instance Functor OddC where
  fmap f (Un a)      = Un (f a)
  fmap f (Bi x y xo) = Bi (f x) (f y) (fmap f xo)

instance Applicative OddC where
  pure = Un

  (Un f) <*> x      = f <$> x
  (Bi f g fs) <*> x = concat3OC (f <$> x) (g <$> x) (fs <*> x)

instance Monad OddC where
  (>>=) m f = concatOC (fmap f m)

instance Foldable OddC where
  foldr f ini (Un a)      = f a ini
  foldr f ini (Bi x y xo) = f x (f y (foldr f ini xo))

instance Traversable OddC where
  sequenceA (Un x)      = Un <$> x
  sequenceA (Bi x y xo) = Bi <$> x <*> y <*> sequenceA xo

concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC (Un x) (Un y) zs        = Bi x y zs
concat3OC (Bi x1 x2 xs) ys zs     = Bi x1 x2 (concat3OC xs ys zs)
concat3OC (Un x) (Bi y1 y2 ys) zs = Bi x y1 (concat3OC (Un y2) ys zs)

concatOC :: OddC (OddC a) -> OddC a
concatOC (Un x) = x
concatOC (Bi x y zs) = concat3OC x y (concatOC zs) 
