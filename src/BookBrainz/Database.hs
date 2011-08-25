{-# LANGUAGE FlexibleContexts #-}

{-| Connect to a BookBrainz PostgreSQL database and interact with it.

This module is very low level, and deals with executing arbitrary SQL. You are
likely more interested in the various 'BookBrainz.Model' modules. -}
module BookBrainz.Database
       (
         Row
       , (!)
       , prefixedRow

         -- * Database Operations
       , query
       , queryOne

         -- * Connection Handling
       , HasDatabase(..)
       , Database
       , connectionHandle
       , openConnection
       ) where

import Control.Applicative      ((<$>))

import Control.Monad.IO.Class   (MonadIO, liftIO)
import Database.HDBC.PostgreSQL (connectPostgreSQL)

import BrainzStem.Database

--------------------------------------------------------------------------------
-- | Open a connection to the BookBrainz PostgreSQL database.
openConnection :: MonadIO m => m Database  -- ^ A connected 'Database'.
openConnection =
  liftIO $ Database <$> connectPostgreSQL "dbname=bookbrainz user=bookbrainz"
