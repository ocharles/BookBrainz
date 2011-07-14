module BookBrainz.Types.Language
  ( Language(..)
  ) where

import Data.Text (Text)

data Language = Language
              { languageName :: Text
              , languageIsoCode :: String
              }
            deriving Show

