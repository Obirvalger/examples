import Data.Foldable

data Tree a = Nil | Branch (Tree a) a (Tree a)   deriving (Eq, Show)

tree  = Branch (Branch Nil 1 (Branch Nil 2 Nil)) 3 (Branch Nil 4 Nil)
--   3
--  / \
-- 1   4
--  \
--   2

tree1 = Branch (Branch (Branch Nil 1 Nil) 3 (Branch Nil 2 Nil)) 4 (Branch Nil 5 (Branch Nil 6 Nil))
--     4
--    / \
--   3   5
--  / \   \
-- 1   2   6

newtype Preorder a   = PreO   (Tree a)    deriving (Eq, Show)
newtype Postorder a  = PostO  (Tree a)    deriving (Eq, Show)
newtype Levelorder a = LevelO (Tree a)    deriving (Eq, Show)

instance Foldable Preorder where
  foldr f ini (PreO Nil)            = ini
  foldr f ini (PreO (Branch l x r)) = f x (foldr f (foldr f ini (PreO r)) (PreO l))

instance Foldable Postorder where
  foldr f ini (PostO Nil)            = ini
  foldr f ini (PostO (Branch l x r)) = foldr f (foldr f (f x ini) (PostO r)) (PostO l)

instance Foldable Levelorder where
  foldr f ini (LevelO tree) = foldr f ini (traverseBF tree)

instance Foldable Tree where
  foldr f ini Nil            = ini
  foldr f ini (Branch l x r) = foldr f (f x (foldr f ini r)) l

flat Nil acc = acc
flat (Branch l x r) acc = flat l (x: acc) ++  flat r (x:acc)

tbf [] = []
tbf xs = map nodeValue xs ++ tbf (concatMap leftAndRightBranchs xs)

nodeValue (Branch _ a _) = a


leftAndRightBranchs (Branch Nil _ Nil) = []
leftAndRightBranchs (Branch Nil _ b)   = [b]
leftAndRightBranchs (Branch a _ Nil)   = [a]
leftAndRightBranchs (Branch a _ b)     = [a,b]

traverseBF :: Tree a -> [a]
traverseBF Nil  = []
traverseBF tree = tbf [tree]
