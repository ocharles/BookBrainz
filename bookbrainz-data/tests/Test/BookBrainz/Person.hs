{-# LANGUAGE OverloadedStrings #-}
module Test.BookBrainz.Person (tests) where

import Test.BrainzStem

import Data.Maybe (fromJust)
import Database.HDBC (toSql)

import BookBrainz.Model.Person
import BookBrainz.Types
import BrainzStem.Model (getByBbid, create)

import BookBrainz.Model.Editor

test_person_allPersons :: DatabaseContext ()
test_person_allPersons = do
  persons <- allPersons
  liftIO $ do
    assertBool "Expected to find some people" (length persons == 1)
    (persons !! 0) @?= testPerson

testPerson = CoreEntity { bbid = fromJust $ parseBbid "2be5075f-d115-42c1-bba5-00bed22f7095"
                        , coreEntityRevision = Ref (toSql (36::Int))
                        , coreEntityTree = Ref (toSql (2::Int))
                        , coreEntityConcept = Ref (toSql (2::Int))
                        , coreEntityInfo = Person { personName = "Bob" }
                        }

test_person_getByBbid :: DatabaseContext ()
test_person_getByBbid = do
  person <- getByBbid $ fromJust $ parseBbid "2be5075f-d115-42c1-bba5-00bed22f7095"
  liftIO $ case person of
    Just b -> b @?= testPerson
    Nothing -> assertFailure "Expected to find a person"

test_person_create :: DatabaseContext ()
test_person_create = do
  createdPerson <- ((entityRef . fromJust) `fmap` getEditorByName "ocharles") >>=
    create (Person { personName = "My First Pony" })
  maybeFetchedPerson <- getByBbid $ bbid createdPerson
  liftIO $ case maybeFetchedPerson of
    Just fetchedPerson -> fetchedPerson @?= createdPerson
    Nothing -> assertFailure "Cannot retrieve created person"

tests :: [Test]
tests = [ testCase "allPersons" $ databaseTest test_person_allPersons
        , testCase "getByBbid" $ databaseTest test_person_getByBbid
        , testCase "create" $ databaseTest test_person_create
        ]

