{-# LANGUAGE OverloadedStrings #-}
module Test.BookBrainz.Role (tests) where

import Test.BrainzStem

import Database.HDBC (toSql)
import Data.Maybe (fromJust)

import BookBrainz.Model.Book
import BookBrainz.Model.Role
import BookBrainz.Types
import BrainzStem.Model

test_role_getByPk :: DatabaseTest
test_role_getByPk = do
  role <- getByPk $ Ref (toSql (1 :: Int))
  liftIO $ role @?= testRole

testRole = Entity { entityRef = Ref (toSql (1 :: Int))
                  , entityInfo = Role { roleName = "Author" }}

test_role_findRoles_book :: DatabaseTest
test_role_findRoles_book = do
  book <- fromJust `fmap`
    (getByBbid $
      (fromJust $ parseBbid "bcad4580-cfc7-47f6-a185-6fd1e0bb1079" :: BBID Book))
  roles <- findRoles $ coreEntityTree book
  liftIO $ do length roles @?= 1
              let (role, person) = roles !! 0
              role @?= testRole
              bbid person @?= (fromJust $ parseBbid "2be5075f-d115-42c1-bba5-00bed22f7095")

test_role_copyRoles_book :: DatabaseTest
test_role_copyRoles_book = do
  let from = (Ref (toSql (24 :: Int)) :: Ref (Tree Book))
  let to = (Ref (toSql (25 :: Int)) :: Ref (Tree Book))
  copyRoles from to
  oldRes <- findRoles from
  newRes <- findRoles to
  liftIO $ newRes @?= oldRes

test_role_findRoles_edition :: DatabaseTest
test_role_findRoles_edition = do
  edition <- fromJust `fmap`
    (getByBbid $
      (fromJust $ parseBbid "86bb2a9d-ac90-4c92-9dc1-2398b4283df4" :: BBID Edition))
  roles <- findRoles $ coreEntityTree edition
  liftIO $ do length roles @?= 1
              let (role, person) = roles !! 0
              role @?= testRole
              bbid person @?= (fromJust $ parseBbid "2be5075f-d115-42c1-bba5-00bed22f7095")

test_role_copyRoles_edition :: DatabaseTest
test_role_copyRoles_edition = do
  let from = (Ref (toSql (16 :: Int)) :: Ref (Tree Edition))
  let to = (Ref (toSql (12 :: Int)) :: Ref (Tree Edition))
  copyRoles from to
  oldRes <- findRoles from
  newRes <- findRoles to
  liftIO $ newRes @?= oldRes

tests :: [Test]
tests = [ testCase "getByPk" $ databaseTest test_role_getByPk
        , testGroup "HasRole Book"
             [ testCase "findRoles" $ databaseTest test_role_findRoles_book
             , testCase "copyRoles" $ databaseTest test_role_copyRoles_book
             ]
        , testGroup "HasRole Edition"
             [ testCase "findRoles" $ databaseTest test_role_findRoles_edition
             , testCase "copyRoles" $ databaseTest test_role_copyRoles_edition
             ]
        ]

