{-# LANGUAGE TypeOperators #-}

infixr 9 |.|
newtype (|.|) f g a = Cmps {getCmps :: f (g a)} deriving Show

type A   = ((,) Integer |.| (,) Char) Bool
type B t = ((,,) Bool (t -> t) |.| Either String) Int
type C   = (|.|) ((->) Bool) ((->) Integer) Integer

a :: A
a = Cmps (3, ('c',True))

b :: B t
b = Cmps (False, id ,Left "Error")

c :: C
c = Cmps (\x y -> 1)
