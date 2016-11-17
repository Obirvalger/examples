import Data.Either
import Data.Maybe
import Data.Char

data Error = ParsingError | IncompleteDataError | IncorrectDataError String deriving Show

data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving Show

fromTuple (f,l,a) = Person f l a

l = [("firstName","John"), ("lastName","Doe"), ("age", "32")]

getParam :: String -> Either Error (String, String)
getParam cs = case span (/='=') cs of
                ([],_)      -> Left ParsingError
                (_,[])      -> Left ParsingError
                (p,(e:s:v)) -> Right (init p,v)

parsePerson :: String -> Either Error Person
parsePerson cs = case partitionEithers $ map getParam $ lines cs of
                   ((e:es),_)    -> Left e
                   (_,ps@(r:rs)) -> fromMaybe (Left IncompleteDataError) $ maybePerson ps
                   (_,_)         -> Left IncompleteDataError


maybePerson ps = lookup "firstName" ps >>= (\firstName ->
                 lookup "lastName" ps  >>= (\lastName  ->
                 lookup "age" ps       >>= (\age       ->
                   if all isDigit age 
                   then Just $ Right $ fromTuple (firstName, lastName, (read age :: Int))
                   else Just $ Left $ IncorrectDataError age)))
