{-# LANGUAGE OverloadedStrings #-}
module Test.BookBrainz.Book (tests) where

import Test.BrainzStem
import Test.BrainzStem.Gen
import Test.BrainzStem.Model
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (Gen, listOf1)
import Test.QuickCheck.Monadic (pick, run, assert, monadicIO)
import Test.QuickCheck.Property (Property)

import Control.Applicative
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (fromJust)
import Database.HDBC (toSql)
import qualified Snap.Snaplet.Hdbc as HDBC
import System.Random (randomIO)

import BookBrainz.Model.Book (listAllBooks)
import BookBrainz.Types
import BrainzStem.Model (getByBbid)
import BookBrainz.Model.Role (findRoles)
import BrainzStem.Model (findMasterBranch, changeBranch, update)
import BrainzStem.Database

instance Arbitrary Book where
  arbitrary = Book <$> name

instance Arbitrary (InDB Book LoadedCoreEntity) where
  arbitrary = do
    book <- arbitrary :: Gen Book
    editor <- initDb `fmap` (arbitrary :: Gen (InDB Editor LoadedEntity))
    return $ InDB book (insertBook book editor)
    where insertBook book addEditor  = do
            eid <- entityRef `fmap` addEditor
            bid <- queryOne (unlines
              [ "INSERT INTO bookbrainz_v.book"
              , "DEFAULT VALUES RETURNING book_id" ]) []
            revision <- queryOne (unlines
              [ "INSERT INTO bookbrainz_v.revision"
              , "(editor) VALUES (?) RETURNING rev_id" ])
              [ toSql eid ]
            branch <- queryOne (unlines
              [ "INSERT INTO bookbrainz_v.branch"
              , "(rev_id, master) VALUES (?, TRUE) RETURNING branch_id" ])
              [ revision ]
            queryOne (unlines
              [ "INSERT INTO bookbrainz_v.book_branch"
              , "(book_id, branch_id) VALUES (?, ?)" ])
              [ bid, branch ]
            version <- queryOne (unlines
              [ "INSERT INTO bookbrainz_v.book_v"
              , "(name) VALUES (?) RETURNING version" ])
              [ toSql $ bookName book ]
            tree <- queryOne (unlines
              [ "INSERT INTO bookbrainz_v.book_tree"
              , "(version) VALUES (?) RETURNING book_tree_id" ])
              [ version ]
            queryOne (unlines
              [ "INSERT INTO bookbrainz_v.book_revision"
              , "(rev_id, book_tree_id) VALUES (?, ?)" ])
              [ revision, tree ]
            bbid' <- liftIO randomIO :: MonadIO m => m (BBID a)
            HDBC.run "INSERT INTO bookbrainz_v.bbid (bbid) VALUES (?)"
              [ toSql bbid' ]
            HDBC.run (unlines [ "INSERT INTO bookbrainz_v.book_bbid (book_id, bbid)"
                         , "VALUES (?, ?)" ])
              [ bid, toSql bbid' ]
            return $ CoreEntity { coreEntityRevision = Ref revision
                                , coreEntityTree = Ref tree
                                , coreEntityInfo = book
                                , coreEntityConcept = Ref bid
                                , bbid = bbid'
                                }

prop_listAllBooks :: Property
prop_listAllBooks = monadicIO $ do
  dbBooks <- pick (listOf1 arbitrary :: Gen [InDB Book LoadedCoreEntity])
  b <- run $ databaseTest $ do
    expect <- initDb `mapM` dbBooks
    actual <- listAllBooks
    return $ expect == actual
  assert b

prop_update :: Property
prop_update = monadicIO $ do
  dbBook <- pick (arbitrary :: Gen (InDB Book LoadedCoreEntity))
  dbEditor <- pick arbitrary
  newBook <- pick arbitrary
  b <- run $ databaseTest $ do
    editor <- initDb dbEditor
    original <- initDb dbBook
    master <- findMasterBranch $ coreEntityConcept original
    changeBranch master (entityRef editor) $ update newBook
    commitedBook <- fromJust <$> (getByBbid $ bbid original)
    tipRoles <- findRoles (coreEntityTree commitedBook)
    prevRoles <- findRoles (coreEntityTree original)
    return $ ((coreEntityInfo commitedBook) == newBook) &&
             (tipRoles == prevRoles)
  assert b

tests :: [Test]
tests = [ testProperty "listAllBooks" $ prop_listAllBooks
        , testProperty "getByBbid" $ prop_getByBbid
            (listOf1 arbitrary :: Gen [InDB Book LoadedCoreEntity])
        , testProperty "create" $ prop_create (arbitrary :: Gen Book)
        , testProperty "update" $ prop_update
        ]

