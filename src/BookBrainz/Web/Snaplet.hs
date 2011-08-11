{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

-- | The BookBrainz snaplet.
module BookBrainz.Web.Snaplet
       ( BookBrainz
       , BookBrainzHandler
         -- * Snaplet lenses
       , database

         -- * Internal
       , makeBbSnaplet
       ) where

import Data.Record.Label
import Snap.Snaplet

import BookBrainz.Database (HasDatabase(..))
import BookBrainz.Web.Snaplet.Database

{-| The BookBrainz snaplet. See lenses below in order to access child
snaplets. -}
data BookBrainz = BookBrainz
    { _database :: Snaplet Database
    }

{-| The top-level BookBrainz handler type synonym. Pages that users access will
be executed in the context of this handler. -}
type BookBrainzHandler = Handler BookBrainz BookBrainz

mkLabels [''BookBrainz]

instance HasDatabase BookBrainzHandler where
  askConnection = with database askConnection

-- FIXME Should not be public!
--------------------------------------------------------------------------------
{-| Create a BookBrainz snaplet. Internal. See 'BookBrainz.Web.bookbrainz'
instead. -}
makeBbSnaplet :: Snaplet Database -> BookBrainz
makeBbSnaplet = BookBrainz
