data OddC a = Un a | Bi a a (OddC a) deriving (Eq,Show)

cnt1 = Un 42
cnt3 = Bi 1 2 cnt1
cnt5 = Bi 3 4 cnt3

instance Functor OddC where
  fmap f (Un a)      = Un (f a)
  fmap f (Bi x y xo) = Bi (f x) (f y) (fmap f xo)

instance Foldable OddC where
  foldr f ini (Un a)      = f a ini
  foldr f ini (Bi x y xo) = f x (f y (foldr f ini xo))

instance Traversable OddC where
  sequenceA (Un x)      = Un <$> x
  sequenceA (Bi x y xo) = Bi <$> x <*> y <*> sequenceA xo
