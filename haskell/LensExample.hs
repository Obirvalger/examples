module Main where

data Athlete = Athlete {name :: String}

getName :: Athlete -> String
getName (Athlete name) = name

setName :: String -> Athlete -> Athlete
setName name (Athlete _) = Athlete name

main :: IO ()
main = let athleteWithoutName = Athlete ""
           realAthlete        = athleteWithoutName {name = "Taylor Fausak"}
           nameOfRealAthlete  = name realAthlete
       in putStrLn nameOfRealAthlete

