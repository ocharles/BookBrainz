import Snap.Http.Server.Config
import Snap.Snaplet (serveSnaplet)

import BookBrainz.Web (bookbrainz)

main :: IO ()
main = serveSnaplet emptyConfig bookbrainz
