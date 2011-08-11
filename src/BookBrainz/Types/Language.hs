-- | Language identification.
module BookBrainz.Types.Language
  ( Language(..)
  ) where

import Data.Text (Text)

--------------------------------------------------------------------------------
-- | A language, as defined by ISO-639-3.
data Language = Language
    { -- | The human-readable name of the language.
      languageName :: Text
      -- | The ISO-639-3 code for the language.
    , languageIsoCode :: String
    } deriving Show

