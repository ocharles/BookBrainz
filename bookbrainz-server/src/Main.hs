{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Exception (SomeException, try)
import           Snap.Core
import           Snap.Http.Server
import           Snap.Snaplet
import           Snap.Snaplet.Config
import           System.IO
import Snap.Http.Server.Config
import System.Console.GetOpt
import Snap.Snaplet (serveSnaplet)
import qualified Data.Text as T

#ifdef DEVELOPMENT
import           Snap.Loader.Dynamic
#else
import           Snap.Loader.Static
#endif

import BookBrainz.Web (bookbrainz)

main :: IO ()
main = do
    --serveSnaplet emptyConfig bookbrainz
    (conf, site, cleanup) <- $(loadSnapTH [| getConf |] 'getActions [])
    _ <- try $ httpServe conf site :: IO (Either SomeException ())
    cleanup

getConf :: IO (Config Snap AppConfig)
getConf = commandLineConfig emptyConfig

getActions :: Config Snap AppConfig -> IO (Snap (), IO ())
getActions conf = do
    let env = appEnvironment =<< getOther conf
    (msgs, site, cleanup) <- runSnaplet env bookbrainz
    hPutStrLn stderr $ T.unpack msgs
    return (site, cleanup)
