decode c = c 0
as c x y = c
a = ()
number = ()

one x c = c $ 1 + x
two x c = c $ 2 + x
three x c = c $ 3 + x
seventeen x c = c $ 17 + x
twenty x c = c $ 20 + x
hundred x c = c $ 100 * x
thousand x c = c $ 1000 * x
