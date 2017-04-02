newtype Arr2 e1 e2 a = Arr2 { getArr2 :: e1 -> e2 -> a }

newtype Arr3 e1 e2 e3 a = Arr3 { getArr3 :: e1 -> e2 -> e3 -> a }

instance Functor (Arr2 e1 e2) where
  fmap g a = Arr2 (\x y -> g (getArr2 a x y))

instance Functor (Arr3 e1 e2 e3) where
  fmap g a = Arr3 (\x y z -> g (getArr3 a x y z))

instance Applicative (Arr2 e1 e2) where
  pure x = Arr2 $ \e1 e2 -> x
  (<*>) g h = Arr2 $ \e1 e2 -> getArr2 g e1 e2 (getArr2 h e1 e2)

instance Applicative (Arr3 e1 e2 e3) where
  pure x = Arr3 $ \e1 e2 e3-> x
  (<*>) g h = Arr3 $ \e1 e2 e3-> getArr3 g e1 e2 e3 (getArr3 h e1 e2 e3)
