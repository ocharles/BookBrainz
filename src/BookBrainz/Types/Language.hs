module BookBrainz.Types.Language
  ( Language(..)
  ) where

import BookBrainz.Types.Newtypes (LanguageIsoCode)
import Data.Text (Text)

data Language = Language
              { languageName :: Text
              , languageIsoCode :: LanguageIsoCode
              }
            | LanguageReference
              { languageIsoCode :: LanguageIsoCode
              }
            deriving Show

