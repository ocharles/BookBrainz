module BookBrainz.Model.Editor
       ( getEditorByName
       ) where

import Data.Maybe (listToMaybe)

import Database.HDBC (toSql)
import Data.Text (Text)

import BrainzStem.Database (query, HasDatabase, Row, (!))
import BookBrainz.Types

getEditorByName :: HasDatabase m => Text -> m (Maybe (LoadedEntity Editor))
getEditorByName name =
  (fmap fromRow . listToMaybe) `fmap` query selectSql [ toSql name ]
  where selectSql = "SELECT editor_id, name FROM editor WHERE name = ?"

fromRow :: Row -> LoadedEntity Editor
fromRow r = Entity { entityInfo = Editor { editorName = r ! "name"
                                         , editorRef = r ! "editor_id" } }
