import Control.Applicative (ZipList(ZipList), getZipList)

infixl 4 >$<
infixl 4 >*<

(>$<) f xs = getZipList $ f <$> ZipList xs
(>*<) fs xs = getZipList $ ZipList fs <*> ZipList xs
