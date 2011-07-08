{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module BookBrainz.Types.Newtypes
  ( AuthorCreditId(..)
  , BookId(..)
  , CountryIsoCode(..)
  , EditionId(..)
  , EditionFormatId(..)
  , LanguageIsoCode(..)
  , PersonId(..)
  , PublisherId(..)
  ) where

import Database.PostgreSQL.Simple.Result (Result)
import Database.PostgreSQL.Simple.Param (Param)

newtype AuthorCreditId = AuthorCreditId Integer
               deriving (Integral,Real,Num,Ord,Eq,Enum,Result,Param)

instance Show AuthorCreditId where
  show (AuthorCreditId id) = show id


newtype BookId = BookId Integer
               deriving (Integral,Real,Num,Ord,Eq,Enum,Result,Param)

instance Show BookId where
  show (BookId id) = show id


newtype CountryIsoCode = CountryIsoCode String
                       deriving (Ord,Eq,Result,Param)

instance Show CountryIsoCode where
  show (CountryIsoCode iso_code) = show iso_code


newtype EditionId = EditionId Integer
                  deriving (Integral,Real,Num,Ord,Eq,Enum,Result,Param)

instance Show EditionId where
  show (EditionId id) = show id


newtype EditionFormatId = EditionFormatId Integer
                        deriving (Integral,Real,Num,Ord,Eq,Enum,Result,Param)

instance Show EditionFormatId where
  show (EditionFormatId id) = show id


newtype LanguageIsoCode = LanguageIsoCode String
                        deriving (Ord,Eq,Result,Param)

instance Show LanguageIsoCode where
  show (LanguageIsoCode iso_code) = show iso_code


newtype PersonId = PersonId Integer
                 deriving (Integral,Real,Num,Ord,Eq,Enum,Result,Param)

instance Show PersonId where
  show (PersonId pid) = show pid


newtype PublisherId = PublisherId Integer
                    deriving (Integral,Real,Num,Ord,Eq,Enum,Result,Param)

instance Show PublisherId where
  show (PublisherId id) = show id


