{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module BookBrainz.Model.Editor
       ( getEditorByName
       ) where

import Data.Maybe (listToMaybe)

import Data.Text (Text)
import Database.PostgreSQL.Simple (Only(..))
import Snap.Snaplet.PostgresqlSimple (HasPostgres, query)

import BookBrainz.Schema ()
import BookBrainz.Types

getEditorByName :: (Functor m, HasPostgres m) => Text -> m (Maybe (LoadedEntity Editor))
getEditorByName name =
  listToMaybe `fmap` query selectSql (Only name)
  where selectSql = "SELECT editor_id, name FROM editor WHERE name = ?"
