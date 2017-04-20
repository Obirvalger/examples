data Triple a = Tr a a a deriving (Eq,Show)

instance Functor Triple where
  fmap g (Tr x y z) = Tr (g x) (g y) (g z) 

instance Applicative Triple where
  pure x = Tr x x x
  (Tr f g h) <*> (Tr x y z) = Tr (f x) (g y) (h z)

instance Foldable Triple where
  foldr g ini (Tr x y z) = foldr g ini [x,y,z]

instance Traversable Triple where
  traverse g (Tr x y z) = Tr <$> (g x) <*> (g y) <*> (g z)
