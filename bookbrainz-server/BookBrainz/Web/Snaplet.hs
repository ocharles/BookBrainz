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
import Data.Pool (Pool)
import Snap.Snaplet
import Snap.Snaplet.Auth                    (AuthManager)
import Snap.Snaplet.Hdbc                    (HdbcSnaplet, HasHdbc(..), connSrc)
import Snap.Snaplet.Session                 (SessionManager)

{-| The BookBrainz snaplet. See lenses below in order to access child
snaplets. -}
data BookBrainz = BookBrainz
    { _database :: Snaplet (HdbcSnaplet Connection Pool)
    , _session :: Snaplet SessionManager
    , _auth :: Snaplet (AuthManager BookBrainz)
    }

{-| The top-level BookBrainz handler type synonym. Pages that users access will
be executed in the context of this handler. -}
type BookBrainzHandler = Handler BookBrainz BookBrainz

$( makeLenses [''BookBrainz] )

instance HasHdbc (Handler b BookBrainz) Connection Pool where
  getConnSrc = with database $ gets connSrc

-- FIXME Should not be public!
--------------------------------------------------------------------------------
{-| Create a BookBrainz snaplet. Internal. See 'BookBrainz.Web.bookbrainz'
instead. -}
makeBbSnaplet :: Snaplet (HdbcSnaplet Connection Pool)
              -> Snaplet SessionManager
              -> Snaplet (AuthManager BookBrainz)
              -> BookBrainz
makeBbSnaplet = BookBrainz
