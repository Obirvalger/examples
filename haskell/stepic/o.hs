instance Applicative ZipList where
    pure x = ZipList (repeat x)
    ZipList fs <*> ZipList xs = ZipList (zipWith id fs xs)

instance Applicative (Either e) where
    pure          = Right
    Left  e <*> _ = Left e
    Right f <*> r = fmap f r


instance Applicative Maybe where
    pure = Just
    Just f  <*> m       = fmap f m
    Nothing <*> _m      = Nothing

instance Applicative [] where
    pure x    = [x]
    fs <*> xs = [f x | f <- fs, x <- xs]

instance Applicative ((->) a) where
    pure = const
    (<*>) f g x = f x (g x)

liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = fmap f a <*> b

(<**>) :: Applicative f => f a -> f (a -> b) -> f b
(<**>) = liftA2 (flip ($))

(<*?>) :: Applicative f => f a -> f (a -> b) -> f b
(<*?>) = flip (<*>)
