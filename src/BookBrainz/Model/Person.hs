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
                                   }

  fromViewRow row =
    CoreEntity { gid = row ! "gid"
               , coreEntityRevision = row ! "revision"
               , coreEntityTree = row ! "person_tree_id"
               , coreEntityConcept = row ! "person_id"
               , coreEntityInfo = Person { personName = row ! "name" }
               }

  findVersion personData = fmap fromSql `fmap`
                          safeQueryOne findSql [ toSql $ personName personData ]
    where findSql = unlines [ "SELECT version"
                            , "FROM bookbrainz_v.person_v"
                            , "WHERE name = ?"
                            ]

  newVersion personData = fromSql `fmap`
                         queryOne insertSql [ toSql $ personName personData ]
    where insertSql = unlines [ "INSERT INTO bookbrainz_v.person_v"
                              , "(name) VALUES (?)"
                              , "RETURNING version"
                              ]
