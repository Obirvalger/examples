import Text.Parsec
import Control.Applicative ((*>), (<*))

ignoreBraces :: Parsec String u a -> Parsec String u b -> Parsec String u c -> Parsec String u c
ignoreBraces o c t = o *> t <* c

getList :: Parsec String u [String]
getList = (:) <$> many1 digit <*> many (char ';' *> many1 digit)
