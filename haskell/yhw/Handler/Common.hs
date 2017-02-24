-- | Common handler functions.
module Handler.Common where

import Data.FileEmbed (embedFile)
import Import

-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.

getFaviconR :: Handler TypedContent
getFaviconR = do cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
                 return $ TypedContent "image/x-icon"
                        $ toContent $(embedFile "config/favicon.ico")

getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")
lol :: Text
lol = "LOL"

isRed :: Bool
isRed = False

numbers :: [Int]
numbers = [1..5]

getFibR :: Handler Html
getFibR = defaultLayout $(widgetFile "fibs") --[whamlet|Hello World!|]

getFibIntR :: Int -> Handler Html
getFibIntR n = defaultLayout [whamlet|Hello #{n} World!|]

getFibTextR :: Text -> Handler Html
getFibTextR _ = redirect $ FibIntR 3
