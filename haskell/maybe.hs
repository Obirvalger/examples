--import Prelude(($))
import Data.Maybe
import Data.Foldable (foldlM)
--import Data.Map.Strict

{-empDep          = fromList [("Mike", "It"), ("Jan", "Sales")]
depCountry      = fromList [("It", "Japan"), ("Sales", "USA")]
countryCurrency = fromList [("Japan", "JPY"), ("USA", "USD")]
rate            = fromList [("JPY", 112), ("USD", 1)]-}
empDep          = [("Mike", "It"), ("Jan", "Sales")]
depCountry      = [("It", "Japan"), ("Sales", "USA")]
countryCurrency = [("Japan", "JPY"), ("USA", "USD")]
currencyRate    = [("JPY", 112), ("USD", 1)]

f emp = case lookup emp empDep of
          Nothing   -> Nothing
          Just dep  -> case lookup dep depCountry of
                         Nothing      -> Nothing
                         Just country -> case lookup country countryCurrency of
                                           Nothing       -> Nothing
                                           Just currency -> lookup currency currencyRate

thenDo Nothing _  = Nothing
thenDo (Just x) g = g x

l  = [empDep, depCountry, countryCurrency]
