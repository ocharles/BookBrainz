{-# LANGUAGE OverloadedStrings #-}
module Test.BookBrainz.Role (tests) where

import Test.BrainzStem
import Test.BrainzStem.Gen
import Test.BrainzStem.Model
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (Gen, listOf1)

import Control.Applicative
import Database.HDBC (toSql)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Snap.Snaplet.Hdbc as HDBC

import BrainzStem.Database
import BookBrainz.Model.Book
import BookBrainz.Model.Editor
import BookBrainz.Model.Role
import BookBrainz.Types
import BrainzStem.Model

instance Arbitrary Role where
  arbitrary = Role <$> T.pack `fmap` name

instance Arbitrary (InDB Role LoadedEntity) where
  arbitrary = do
    r <- arbitrary
    return $ InDB r (insertRole r >>= toEnt r)
    where insertRole r =
            queryOne "INSERT INTO person_role (name) VALUES (?) RETURNING role_id"
                     [ toSql $ roleName r ]
          toEnt r id = return $ Entity { entityRef = Ref id
                                       , entityInfo = r }

testRole = Entity { entityRef = Ref (toSql (1 :: Int))
                  , entityInfo = Role { roleName = "Author" }}

test_role_allRoles :: DatabaseTest
test_role_allRoles = do
  allRoles <- allRoles
  liftIO $ do allRoles !! 0 @?= testRole
              length allRoles @?= 4

test_role_findRoles_book :: DatabaseTest
test_role_findRoles_book = do
  book <- fromJust `fmap` (getByBbid $ bookBbid)
  roles <- findRoles $ coreEntityTree book
  liftIO $ do length roles @?= 1
              let (role, person) = roles !! 0
              role @?= testRole
              bbid person @?= personBbid
  where bookBbid =
          fromJust $ parseBbid "bcad4580-cfc7-47f6-a185-6fd1e0bb1079" :: BBID Book
        personBbid = fromJust $ parseBbid "2be5075f-d115-42c1-bba5-00bed22f7095"

test_role_copyRoles_book :: DatabaseTest
test_role_copyRoles_book = do
  let from = (Ref (toSql (24 :: Int)) :: Ref (Tree Book))
  let to = (Ref (toSql (25 :: Int)) :: Ref (Tree Book))
  copyRoles from to
  oldRes <- findRoles from
  newRes <- findRoles to
  liftIO $ newRes @?= oldRes

test_role_addRole_book :: DatabaseTest
test_role_addRole_book = do
  book <- fetchBook
  currentRoles <- findRoles $ coreEntityTree book
  editor <- ((entityRef . fromJust) `fmap` getEditorByName "ocharles")
  master <- findMasterBranch $ coreEntityConcept book
  person <- fromJust `fmap` (getByBbid $ fromJust $ parseBbid "2be5075f-d115-42c1-bba5-00bed22f7095")
  role <- getByPk (Ref (toSql (4 :: Int)))
  changeBranch master editor $
    addRole (coreEntityConcept person, entityRef role)
  revisedBook <- fetchBook
  revisedRoles <- findRoles $ coreEntityTree revisedBook
  liftIO $ assertBool "New version has more roles"
                      (length revisedRoles > length currentRoles)
  where fetchBook = fromJust `fmap`
          (getByBbid (fromJust $ parseBbid "bcad4580-cfc7-47f6-a185-6fd1e0bb1079" :: BBID Book))

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

test_role_addRole_edition :: DatabaseTest
test_role_addRole_edition = do
  edition <- fetchEdition
  currentRoles <- findRoles $ coreEntityTree edition
  editor <- ((entityRef . fromJust) `fmap` getEditorByName "ocharles")
  master <- findMasterBranch $ coreEntityConcept edition
  person <- fromJust `fmap` (getByBbid $ fromJust $ parseBbid "2be5075f-d115-42c1-bba5-00bed22f7095")
  role <- getByPk (Ref (toSql (4 :: Int)))
  changeBranch master editor $
    addRole (coreEntityConcept person, entityRef role)
  revisedEdition <- fetchEdition
  revisedRoles <- findRoles $ coreEntityTree revisedEdition
  liftIO $ assertBool "New version has more roles"
                      (length revisedRoles > length currentRoles)
  where fetchEdition = fromJust `fmap`
          (getByBbid (fromJust $ parseBbid "86bb2a9d-ac90-4c92-9dc1-2398b4283df4" :: BBID Edition))

tests :: [Test]
tests = [ testProperty "getByPk" $ prop_getByPk
          (listOf1 arbitrary :: Gen [InDB Role LoadedEntity])
        , testCase "allRoles" $ databaseTest test_role_allRoles
        , testGroup "HasRole Book"
             [ testCase "findRoles" $ databaseTest test_role_findRoles_book
             , testCase "copyRoles" $ databaseTest test_role_copyRoles_book
             , testCase "addRole" $ databaseTest test_role_addRole_book
             ]
        , testGroup "HasRole Edition"
             [ testCase "findRoles" $ databaseTest test_role_findRoles_edition
             , testCase "copyRoles" $ databaseTest test_role_copyRoles_edition
             , testCase "addRole" $ databaseTest test_role_addRole_edition
             ]
        ]

