module BookBrainz.Types.Country
  ( Country(..)
  ) where

import Data.Text (Text)

data Country = Country
              { countryName :: Text
              , countryIsoCode :: String
              }
            deriving Show

