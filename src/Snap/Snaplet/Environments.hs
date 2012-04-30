module Snap.Snaplet.Environments
    ( module Data.Configurator
    , lookupEnvDefault
    , module Snap.Snaplet.Environments.Instances ) 
    where

import           Control.Monad.Reader
import           Data.Configurator
import           Data.Configurator.Types
import qualified Data.HashMap.Lazy                   as HM
import           Data.List                           (filter, find)
import qualified Data.Text                           as T
import           Snap.Snaplet
import           Snap.Snaplet.Environments.Instances
import           System.Environment                  (getArgs)
import           Text.Regex.TDFA


-- | This function takes current env subconfig and at its base
--   looks up given name
lookupEnvDefault :: (Configured a, Monad (m b v), MonadSnaplet m, MonadIO (m b v)) => Name -> a -> m b v a
lookupEnvDefault name def = do
  mainConf <- getSnapletUserConfig
  subName <- getNameForCurEnv name mainConf
  mv <- liftIO $ Data.Configurator.lookup mainConf subName
  case mv of
    Nothing -> liftIO $ lookupDefault def mainConf name
    Just v  -> (liftIO $ putStrLn "Just") >> return v

getNameForCurEnv :: (Monad (m b v), MonadSnaplet m, MonadIO (m b v)) => Name -> Config -> m b v Name
getNameForCurEnv name cfg = do
  env <- getCurrentEnv cfg
  return $ T.pack $ "app.environments." ++ env ++ "." ++ (T.unpack name)

getCurrentEnv :: (Monad (m b v), MonadSnaplet m, MonadIO (m b v)) => Config -> m b v String
getCurrentEnv cfg = do
  mopt <- return . find (\a -> take 1 a == "@") =<< liftIO getArgs
  case mopt of
    Nothing  -> do
      hm <- liftIO $ getMap cfg
      case filter (\k -> (T.unpack k) =~ ("app.environments." :: String)) $ HM.keys hm of
        []     -> error "You have to put at least one env definition in your config file."
        (x:xs) -> return $ T.unpack $ (T.split (== '.') x) !! 2
    Just opt -> do
      hm <- liftIO $ getMap cfg
      case length (filter (\k -> (T.unpack k) =~ ("app.environments." ++ (tail opt))) $ HM.keys hm) > 0 of
        True  -> return $ tail opt
        False -> error $ "Given env name: " ++ opt ++ " wasn't found in your config file."