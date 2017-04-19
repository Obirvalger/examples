{-# LANGUAGE TypeOperators#-}

infixr 9 |.|
newtype (|.|) f g a = Cmps { getCmps :: f (g a) }  deriving (Eq,Show) 

instance (Foldable c1, Foldable c2) => Foldable ((|.|) c1 c2) where
    -- foldr :: (a -> b -> b) -> b -> t a -> b-}
    -- foldr f ini (Cmps x) = foldr f ini x
    -- foldMap (a -> m) t a -> m
    foldMap f (Cmps x) = foldMap (foldMap f) x
