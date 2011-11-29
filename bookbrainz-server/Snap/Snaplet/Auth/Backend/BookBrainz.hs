-- Presumably there will be a HDBC backend, but until then, we'll have to write
-- our own.

{-# LANGUAGE OverloadedStrings #-}
module Snap.Snaplet.Auth.Backend.BookBrainz
       ( initBookBrainzAuthManager
       , mkBookBrainzAuthMgr
       ) where

import           Data.List               (intercalate)
import           Data.Maybe              (listToMaybe)

import           Database.HDBC.PostgreSQL (Connection)
import           Control.Monad.IO.Class  (liftIO)
import           Data.Convertible        (Convertible(..))
import qualified Data.HashMap.Strict     as HM
import           Data.Lens.Lazy          (Lens)
import           Data.Text               (unpack)
import           Database.HDBC           (toSql, fromSql, SqlValue)
import           Snap.Snaplet            (Snaplet, makeSnaplet, SnapletInit)
import           Snap.Snaplet.Auth (IAuthBackend (..)
                                               , AuthManager (..))
import           Snap.Snaplet.Auth (UserId (..), AuthSettings (..)
                                         ,Password (..),AuthUser (..), unUid)
import           Snap.Snaplet.Session    (SessionManager)
import           Web.ClientSession       (getKey)

import           BrainzStem.Database     (Database(Database), query, (!)
                                         ,runDatabase, Row, queryOne)

data BookBrainzAuthManager b = BookBrainzAuthManager
    { dbConn :: Connection
    }

database = Database . dbConn 

------------------------------------------------------------------------------
-- | Initialize a BookBrainz auth backend 'AuthManager'.
initBookBrainzAuthManager
  :: AuthSettings
  -- ^ Authentication settings for your app.
  -> Lens b (Snaplet SessionManager)
  -- ^ Lens into a 'SessionManager' auth snaplet will use.
  -> Connection
  -- ^ A database connection to the BookBrainz schema.
  -> SnapletInit b (AuthManager b)
initBookBrainzAuthManager settings sessionL connL =
  makeSnaplet "BookBrainzAuthManager"
              "A snaplet providing user authentication for BookBrainz"
              Nothing $ liftIO $ do
    key <- getKey (asSiteKey settings)
    mgr <- mkBookBrainzAuthMgr connL
    return $ AuthManager { backend = mgr
                         , session = sessionL
                         , activeUser = Nothing
                         , minPasswdLen = asMinPasswdLen settings
                         , rememberCookieName = asRememberCookieName settings
                         , rememberPeriod = asRememberPeriod settings
                         , siteKey = key
                         , lockout = asLockout settings
                         }

mkBookBrainzAuthMgr :: Connection -> IO (BookBrainzAuthManager b)
mkBookBrainzAuthMgr = return . BookBrainzAuthManager

instance Convertible SqlValue UserId where
  safeConvert = return . UserId . fromSql

instance Convertible UserId SqlValue where
  safeConvert = return . toSql . (read . unpack . unUid :: UserId -> Int)

instance Convertible SqlValue Password where
  safeConvert = return . Encrypted . fromSql

instance Convertible Password SqlValue where
  safeConvert (Encrypted p) = return . toSql $ p

instance IAuthBackend (BookBrainzAuthManager b) where
  lookupByUserId r uid =
    runDatabase (database r) $
      (fmap buildUser . listToMaybe) `fmap` query lookupQuery [ toSql uid ]
    where lookupQuery = unlines [ "SELECT *"
                                , "FROM editor"
                                , "WHERE editor_id = ?"
                                ]

  lookupByLogin r login =
    runDatabase (database r) $
      (fmap buildUser . listToMaybe) `fmap` query lookupQuery [ toSql login ]
    where lookupQuery = unlines [ "SELECT *"
                                , "FROM editor"
                                , "WHERE name = ?"
                                ]

  lookupByRememberToken r token =
    runDatabase (database r) $
      (fmap buildUser . listToMaybe) `fmap` query lookupQuery [ toSql token ]
    where lookupQuery = "SELECT * FROM editor WHERE remember_token = ?"

  -- FIXME Needs to update or create users
  save r user = runDatabase (database r) $ do
    case userId user of
      Nothing  -> insertUser
      Just _ -> updateUser
    where
      insertUser =
        let sql = unlines ["INSERT INTO editor (name, password)"
                          ,"VALUES (?, ?)"
                          ,"RETURNING editor_id"
                          ]
        in do editorId <- queryOne sql [ toSql $ userLogin user
                                       , toSql $ userPassword user
                                       ]
              return $ user { userId = fromSql editorId }
      updateUser =
        let sql = unlines [ "UPDATE editor SET"
                          , intercalate ", " $ map (++ " = ?")
                              [ "name", "password", "remember_token" ]
                          , "WHERE editor_id = ?"
                          ]
        in query sql [ toSql $ userLogin user
                     , toSql $ userPassword user
                     , toSql $ userRememberToken user
                     , toSql $ userId user
                     ] >> return user


buildUser :: Row -> AuthUser
buildUser row = AuthUser { userId = row ! "editor_id"
                         , userLogin = row ! "name"
                         , userPassword = row ! "password"
                         , userActivatedAt = Nothing -- :: Maybe UTCTime
                         , userSuspendedAt = Nothing -- :: Maybe UTCTime
                         , userRememberToken = Nothing -- :: Maybe Text
                         , userLoginCount = 0 -- :: Int
                         , userFailedLoginCount = 0 -- :: Int
                         , userLockedOutUntil = Nothing -- :: Maybe UTCTime
                         , userCurrentLoginAt = Nothing -- :: Maybe UTCTime
                         , userLastLoginAt = Nothing -- :: Maybe UTCTime
                         , userCurrentLoginIp = Nothing -- :: Maybe ByteString
                         , userLastLoginIp = Nothing -- :: Maybe ByteString
                         , userCreatedAt = Nothing -- :: Maybe UTCTime
                         , userUpdatedAt = Nothing -- :: Maybe UTCTime
                         , userRoles = [] -- :: [Role]
                         , userMeta = HM.empty -- :: HashMap Text Value
                         }

--  lookupByRememberToken backend token = undefined
--  destroy backend user = undefined

