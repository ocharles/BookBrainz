-- | Functions for working with 'BookBrainz.Types.Publisher.Publisher' entities.
module BookBrainz.Model.Publisher () where

import BrainzStem.Model.GenericVersioning (GenericallyVersioned (..)
                                          ,VersionConfig (..))

import Database.HDBC                      (toSql, fromSql)

import BookBrainz.Types                   (Publisher (..))
import BrainzStem.Database                (queryOne, safeQueryOne, (!))
import BrainzStem.Types                   (LoadedCoreEntity (..))

instance GenericallyVersioned Publisher where
  versioningConfig = VersionConfig { cfgView = "publisher"
                                   , cfgIdCol = "publisher_id"
                                   }

  fromViewRow row =
    CoreEntity { gid = row ! "gid"
               , coreEntityRevision = row ! "revision"
               , coreEntityTree = row ! "publisher_tree_id"
               , coreEntityConcept = row ! "publisher_id"
               , coreEntityInfo = Publisher { publisherName = row ! "name" }
               }

  findVersion pubData = fmap fromSql `fmap`
                          safeQueryOne findSql [ toSql $ publisherName pubData ]
    where findSql = unlines [ "SELECT version"
                            , "FROM bookbrainz_v.publisher_v"
                            , "WHERE name = ?"
                            ]

  newVersion pubData = fromSql `fmap`
                         queryOne insertSql [ toSql $ publisherName pubData ]
    where insertSql = unlines [ "INSERT INTO bookbrainz_v.publisher_v"
                              , "(name) VALUES (?)"
                              , "RETURNING version"
                              ]

