{-# LANGUAGE TypeOperators#-}

infixr 9 |.|
newtype (|.|) f g a = Cmps { getCmps :: f (g a) }  deriving (Eq,Show) 

instance (Functor f, Functor g) => Functor ((|.|) f g) where
  fmap j (Cmps x) = Cmps $ fmap (fmap j) x

instance (Foldable f, Foldable g) => Foldable ((|.|) f g) where
  foldMap f (Cmps x) = foldMap (foldMap f) x

instance (Traversable f, Traversable g) => Traversable ((|.|) f g) where
  traverse j (Cmps x) = Cmps <$> traverse (traverse j) x
