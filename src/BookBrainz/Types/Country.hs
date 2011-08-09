-- | Country identification.
module BookBrainz.Types.Country
  ( Country(..)
  ) where

import Data.Text (Text)

--------------------------------------------------------------------------------
-- | A country, as defined by ISO 3166-1.
data Country = Country
    { -- | The human readable name of the country.
      countryName    :: Text
      -- | The ISO 3166-1-alpha-2 ISO code of the country.
    , countryIsoCode :: String
    } deriving Show
