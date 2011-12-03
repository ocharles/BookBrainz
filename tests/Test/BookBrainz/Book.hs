{-# LANGUAGE OverloadedStrings #-}
module Test.BookBrainz.Book (tests) where

import Test.BrainzStem
import Test.BrainzStem.Gen
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (Gen, listOf1)
import Test.QuickCheck.Monadic (pick, run, assert, monadicIO)

import Control.Applicative
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Maybe (fromJust)
import Database.HDBC (toSql)
import qualified Snap.Snaplet.Hdbc as HDBC
import System.Random (randomIO)

import BookBrainz.Model.Book (listAllBooks, create)
import BookBrainz.Types
import BrainzStem.Model (getByBbid)
import BookBrainz.Model.Editor
import BookBrainz.Model.Role (findRoles)
import BrainzStem.Model (findMasterBranch, changeBranch, update)
import BrainzStem.Database

instance Arbitrary Editor where
  arbitrary = Editor <$> name

instance Arbitrary (InDB Editor LoadedEntity) where
  arbitrary = do
    editor <- arbitrary
    return $ InDB editor (insertEditor editor >>= return . toEnt editor)
    where insertEditor e = do
            queryOne (unlines [ "INSERT INTO editor (name, password) VALUES (?, 'a')"
                              , "RETURNING editor_id" ])
                     [ toSql $ editorName e]
          toEnt editor id = Entity { entityRef = Ref id
                                   , entityInfo = editor }

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
            bookBranch <- queryOne (unlines
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
            bookRevision <- queryOne (unlines
              [ "INSERT INTO bookbrainz_v.book_revision"
              , "(rev_id, book_tree_id) VALUES (?, ?)" ])
              [ revision, tree ]
            bbid <- liftIO randomIO :: MonadIO m => m (BBID a)
            HDBC.run "INSERT INTO bookbrainz_v.bbid (bbid) VALUES (?)"
              [ toSql bbid ]
            HDBC.run (unlines [ "INSERT INTO bookbrainz_v.book_bbid (book_id, bbid)"
                         , "VALUES (?, ?)" ])
              [ bid, toSql bbid ]
            return $ CoreEntity { coreEntityRevision = Ref revision
                                , coreEntityTree = Ref tree
                                , coreEntityInfo = book
                                , coreEntityConcept = Ref bid
                                , bbid = bbid
                                }

prop_listAllBooks = monadicIO $ do
  dbBooks <- pick (listOf1 arbitrary :: Gen [InDB Book LoadedCoreEntity])
  b <- run $ databaseTest $ do
    expect <- initDb `mapM` dbBooks
    actual <- listAllBooks
    return $ expect == actual
  assert b

testBook = CoreEntity { bbid = fromJust $ parseBbid "bcad4580-cfc7-47f6-a185-6fd1e0bb1079"
                      , coreEntityRevision = Ref (toSql (41::Int))
                      , coreEntityTree = Ref (toSql (24::Int))
                      , coreEntityConcept = Ref (toSql (17::Int))
                      , coreEntityInfo = Book { bookName = "A New Book" }
                      }

test_book_getByBbid :: DatabaseContext ()
test_book_getByBbid = do
  book <- getByBbid $ fromJust $ parseBbid "bcad4580-cfc7-47f6-a185-6fd1e0bb1079"
  liftIO $ case book of
    Just b -> b @?= testBook
    Nothing -> assertFailure "Expected to find a book"

test_book_create :: DatabaseContext ()
test_book_create = do
  createdBook <- ((entityRef . fromJust) `fmap` getEditorByName "ocharles") >>=
    create (Book { bookName = "My First Pony" })
  maybeFetchedBook <- getByBbid $ bbid createdBook
  liftIO $ case maybeFetchedBook of
    Just fetchedBook -> fetchedBook @?= createdBook
    Nothing -> assertFailure "Cannot retrieve created book"

test_book_update :: DatabaseTest
test_book_update = do
  original <- fetchBook
  editor <- ((entityRef . fromJust) `fmap` getEditorByName "ocharles")
  master <- findMasterBranch $ coreEntityConcept original
  let newBook = (coreEntityInfo original) { bookName = "Renamed book" }
  changeBranch master editor $ update newBook

  commitedBook <- fetchBook
  tipRoles <- findRoles (coreEntityTree commitedBook)
  prevRoles <- findRoles (coreEntityTree original)
  liftIO $ do (coreEntityInfo commitedBook) @?= newBook
              tipRoles @?= prevRoles
  where fetchBook = fromJust `fmap`
          (getByBbid $ fromJust $ parseBbid "bcad4580-cfc7-47f6-a185-6fd1e0bb1079")

tests :: [Test]
tests = [ testProperty "listAllBooks" $ prop_listAllBooks
        , testCase "getByBbid" $ databaseTest test_book_getByBbid
        , testCase "create" $ databaseTest test_book_create
        , testCase "update" $ databaseTest test_book_update
        ]

