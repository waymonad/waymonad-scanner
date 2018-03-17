{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Wayland.Scanner.WLS
    ( Scanner (..)
    , ScannerEnv (..)
    , getObjectConvert
    , scannerIO
    , runScanner
    )
where

import Control.Monad.Trans (MonadTrans(lift))
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Foreign.Ptr (Ptr)

import Graphics.Wayland.Scanner.Types

import qualified Data.Map as M
import qualified Language.Haskell.TH as TH

type ObjectMap = Map String (TH.Type, TH.Name, TH.Name)

data ScannerEnv = ScannerEnv
    { scannerObjectMap :: ObjectMap
    }

newtype Scanner m a = Scanner (ReaderT ScannerEnv m a)
    deriving (Functor, Applicative, Monad, MonadReader ScannerEnv)


getObjectMap :: Monad m => Scanner m ObjectMap
getObjectMap = scannerObjectMap <$> ask

getObjectConvert :: Monad m => String -> Scanner m (TH.Type, TH.Name, TH.Name)
getObjectConvert name = do
    oMap <- getObjectMap
    pure $ fromMaybe (TH.AppT (TH.ConT ''Ptr) (TH.ConT ''WlResource), 'pure, 'pure) $ M.lookup name oMap

scannerIO :: IO a -> Scanner TH.Q a
scannerIO = Scanner . lift . TH.runIO

runScanner :: Scanner m a -> ScannerEnv -> m a
runScanner (Scanner act) env = runReaderT act env
