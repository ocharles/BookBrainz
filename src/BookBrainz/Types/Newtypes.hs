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

newtype AuthorCreditId = AuthorCreditId Integer
               deriving (Integral,Real,Num,Ord,Eq,Enum)

instance Show AuthorCreditId where
  show (AuthorCreditId acid) = show acid


newtype BookId = BookId Integer
               deriving (Integral,Real,Num,Ord,Eq,Enum)

instance Show BookId where
  show (BookId bid) = show bid


newtype CountryIsoCode = CountryIsoCode String
                       deriving (Ord,Eq)

instance Show CountryIsoCode where
  show (CountryIsoCode iso_code) = show iso_code


newtype EditionId = EditionId Integer
                  deriving (Integral,Real,Num,Ord,Eq,Enum)

instance Show EditionId where
  show (EditionId eid) = show eid


newtype EditionFormatId = EditionFormatId Integer
                        deriving (Integral,Real,Num,Ord,Eq,Enum)

instance Show EditionFormatId where
  show (EditionFormatId efid) = show efid


newtype LanguageIsoCode = LanguageIsoCode String
                        deriving (Ord,Eq)

instance Show LanguageIsoCode where
  show (LanguageIsoCode iso_code) = show iso_code


newtype PersonId = PersonId Integer
                 deriving (Integral,Real,Num,Ord,Eq,Enum)

instance Show PersonId where
  show (PersonId pid) = show pid


newtype PublisherId = PublisherId Integer
                    deriving (Integral,Real,Num,Ord,Eq,Enum)

instance Show PublisherId where
  show (PublisherId pid) = show pid


