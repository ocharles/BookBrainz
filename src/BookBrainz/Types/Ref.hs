module BookBrainz.Types.Ref
       ( Ref(..)
       ) where

-- |Represents a reference in a database. @entity@ is a phantom type which
-- tracks what type of entity this reference refers to.
data Ref entity = Ref { rid :: Int}
                deriving Show
