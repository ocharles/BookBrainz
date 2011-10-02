{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

-- | The BookBrainz snaplet.
module BookBrainz.Web.Snaplet
       ( BookBrainz
       , BookBrainzHandler

         -- * Snaplet lenses
       , database
       , session
       , auth

         -- * Internal
       , makeBbSnaplet
       ) where

import Control.Monad.State.Class            (gets)
import Database.HDBC.PostgreSQL             (Connection)
import Data.Lens.Template
import Snap.Snaplet
import Snap.Snaplet.Auth                    (AuthManager)
import Snap.Snaplet.Hdbc                    (HdbcSnaplet, hdbcConn)
import Snap.Snaplet.Session                 (SessionManager)

import BrainzStem.Database                  (HasDatabase(..))

{-| The BookBrainz snaplet. See lenses below in order to access child
snaplets. -}
data BookBrainz = BookBrainz
    { _database :: Snaplet (HdbcSnaplet Connection)
    , _session :: Snaplet SessionManager
    , _auth :: Snaplet (AuthManager BookBrainz)
    }

{-| The top-level BookBrainz handler type synonym. Pages that users access will
be executed in the context of this handler. -}
type BookBrainzHandler = Handler BookBrainz BookBrainz

$( makeLenses [''BookBrainz] )

instance HasDatabase (Handler b (HdbcSnaplet Connection)) where
  askConnection = gets hdbcConn

instance HasDatabase BookBrainzHandler where
  askConnection = with database askConnection

-- FIXME Should not be public!
--------------------------------------------------------------------------------
{-| Create a BookBrainz snaplet. Internal. See 'BookBrainz.Web.bookbrainz'
instead. -}
makeBbSnaplet :: Snaplet (HdbcSnaplet Connection)
              -> Snaplet SessionManager
              -> Snaplet (AuthManager BookBrainz)
              -> BookBrainz
makeBbSnaplet = BookBrainz
