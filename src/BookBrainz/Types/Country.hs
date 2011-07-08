module BookBrainz.Types.Country
  ( Country(..)
  ) where

import BookBrainz.Types.Newtypes (CountryIsoCode)
import Data.Text (Text)

data Country = Country
              { countryName :: Text
              , countryIsoCode :: CountryIsoCode
              }
            | CountryReference
              { countryIsoCode :: CountryIsoCode
              }
            deriving Show

