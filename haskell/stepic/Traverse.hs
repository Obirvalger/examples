{-traverse1_ f = foldr ((*>) . f) (pure ())-}

traverse1 f = foldr (\x y -> pure (:) <*> (f x) <*> y) (pure [])
