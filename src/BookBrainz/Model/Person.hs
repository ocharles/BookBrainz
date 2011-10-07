-- | Functions for working with 'BookBrainz.Types.Person.Person' entities.
module BookBrainz.Model.Person () where

import BrainzStem.Model.GenericVersioning (GenericallyVersioned (..)
                                          ,VersionConfig (..))

import Database.HDBC                      (toSql, fromSql)

import BookBrainz.Types                   (Person (..))
import BrainzStem.Database                (queryOne, safeQueryOne, (!))
import BrainzStem.Types                   (LoadedCoreEntity (..))

instance GenericallyVersioned Person where
  versioningConfig = VersionConfig { cfgView = "person"
                                   , cfgIdCol = "person_id"
                                   , cfgConcept = "person"
                                   , cfgTree = "person_tree"
                                   , cfgBbid = "person_bbid"
                                   , cfgRevision = "person_revision"
                                   , cfgBranch = "person_branch"
                                   }

  fromViewRow row =
    CoreEntity { bbid = row ! "bbid"
               , coreEntityRevision = row ! "rev_id"
               , coreEntityTree = row ! "person_tree_id"
               , coreEntityConcept = row ! "person_id"
               , coreEntityInfo = Person { personName = row ! "name" }
               }

  newTree _ pubData = do
    versionId <- findOrInsertVersion
    fromSql `fmap` queryOne insertTreeSql [ versionId ]
    where
      findOrInsertVersion = do
        foundId <- findVersion
        case foundId of
          Just id' -> return id'
          Nothing -> newVersion
      insertTreeSql = unlines [ "INSERT INTO bookbrainz_v.person_tree"
                              , "(version) VALUES (?)"
                              , "RETURNING person_tree_id"
                              ]
      findVersion =
        let findSql = unlines [ "SELECT version"
                              , "FROM bookbrainz_v.person_v"
                              , "WHERE name = ?"
                              ]
        in safeQueryOne findSql [ toSql $ personName pubData ]
      newVersion =
        let insertSql = unlines [ "INSERT INTO bookbrainz_v.person_v"
                                , "(name) VALUES (?)"
                                , "RETURNING version"
                                ]
        in queryOne insertSql [ toSql $ personName pubData ]
