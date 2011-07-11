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

import Data.Convertible (Convertible(..), convError)
import Data.UUID (UUID, fromString)
import Database.HDBC (fromSql, toSql, SqlValue)

newtype AuthorCreditId = AuthorCreditId Integer
               deriving (Integral,Real,Num,Ord,Eq,Enum)

instance Show AuthorCreditId where
  show (AuthorCreditId acid) = show acid

instance Convertible SqlValue AuthorCreditId where
  safeConvert = Right . AuthorCreditId . fromSql


newtype BookId = BookId Integer
               deriving (Integral,Real,Num,Ord,Eq,Enum)

instance Show BookId where
  show (BookId bid) = show bid

instance Convertible BookId SqlValue where
  safeConvert (BookId bid) = Right $ toSql bid

newtype CountryIsoCode = CountryIsoCode String
                       deriving (Ord,Eq)

instance Show CountryIsoCode where
  show (CountryIsoCode iso_code) = show iso_code

instance Convertible SqlValue CountryIsoCode where
  safeConvert = Right . CountryIsoCode . fromSql


newtype EditionId = EditionId Integer
                  deriving (Integral,Real,Num,Ord,Eq,Enum)

instance Show EditionId where
  show (EditionId eid) = show eid

instance Convertible SqlValue EditionId where
  safeConvert = Right . EditionId . fromSql


newtype EditionFormatId = EditionFormatId Integer
                        deriving (Integral,Real,Num,Ord,Eq,Enum)

instance Show EditionFormatId where
  show (EditionFormatId efid) = show efid

instance Convertible SqlValue EditionFormatId where
  safeConvert = Right . EditionFormatId . fromSql

newtype LanguageIsoCode = LanguageIsoCode String
                        deriving (Ord,Eq)

instance Show LanguageIsoCode where
  show (LanguageIsoCode iso_code) = show iso_code

instance Convertible SqlValue LanguageIsoCode where
  safeConvert = Right . LanguageIsoCode . fromSql

newtype PersonId = PersonId Integer
                 deriving (Integral,Real,Num,Ord,Eq,Enum)

instance Show PersonId where
  show (PersonId pid) = show pid

instance Convertible SqlValue PersonId where
  safeConvert = Right . PersonId . fromSql

newtype PublisherId = PublisherId Integer
                    deriving (Integral,Real,Num,Ord,Eq,Enum)

instance Show PublisherId where
  show (PublisherId pid) = show pid

instance Convertible SqlValue PublisherId where
  safeConvert = Right . PublisherId . fromSql


instance Convertible SqlValue UUID where
  safeConvert gid = case (fromString $ fromSql gid) of
              Just uuid -> return uuid
              Nothing -> convError "Not a valid UUID" gid
