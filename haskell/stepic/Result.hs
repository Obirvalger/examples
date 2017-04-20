data Result a = Ok a | Error String deriving (Eq,Show)

instance Functor Result where
  fmap g (Error s) = Error s
  fmap g (Ok a)    = Ok (g a)

instance Applicative Result where
  pure a = Ok a

  (Ok g) <*> r    = fmap g r
  (Error s) <*> _ = Error s

instance Foldable Result where
  foldr _ ini (Error s) = ini
  foldr g ini (Ok a)    = g a ini

instance Traversable Result where
  traverse _ (Error s) = pure (Error s)
  traverse g (Ok a)    = Ok <$> g a
