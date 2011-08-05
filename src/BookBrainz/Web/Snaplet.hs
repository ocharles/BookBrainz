{-# LANGUAGE TemplateHaskell #-}

module BookBrainz.Web.Snaplet
       ( BookBrainz(..)
       , BookBrainzHandler
       , database
       ) where

import Data.Record.Label
import Snap.Snaplet

import BookBrainz.Database (HasDatabase(..))
import BookBrainz.Web.Snaplet.Database

data BookBrainz = BookBrainz
    { _database :: Snaplet Database
    }

type BookBrainzHandler = Handler BookBrainz BookBrainz

mkLabels [''BookBrainz]

instance HasDatabase BookBrainzHandler where
  askConnection = with database askConnection
