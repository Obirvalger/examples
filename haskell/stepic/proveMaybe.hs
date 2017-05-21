-- Докажем, что (u <|> v) <*> w == u <*> w <|> v <*> w
(u <|> v) <*> w
(Nothing <|> v) <*> w -- 1 <|>
== v <*> w

(Just f <|> v) <*> w -- 2 <|>
== Just f <*> w


u <*> w <|> v <*> w
Nohting <*> w <|> v <*> w -- 2 <*>
== Nothing <|> v <*> w -- 1 <|>
== v <*> w

Just f <*> Nohting <|> v <*> Nohting -- 1 <*>, 1 fmap
== Nothing <|> v <*> Nohting -- 2 <*> || (1 <*>, 1 fmap)
== Nothing <|> Nohting -- 1 <|>
== Nohting
== v <*> w

Just f <*> Just a <|> v <*> Just a -- 1 <*>, 1 fmap
== Just (f a) <|> v <*> Just a -- 2 <|>
== Just (f a)
== v <*> w


-- Докажем, что (u `mplus` v) >>= k /= (u >>= k) `mplus` (v >>= k)
-- Приведем контрпример:
u = Just 2
v = Just 3
k x = if odd x then Just x else Nothing

(u `mplus` v) >>= k == Nohting
(u >>= k) `mplus` (v >>= k) == Just 3

