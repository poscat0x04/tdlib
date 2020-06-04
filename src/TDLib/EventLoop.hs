{-# LANGUAGE ExistentialQuantification #-}

-- | A heavyweight TDLib effect intepreter written using event loop
module TDLib.EventLoop
  ( -- * effect interpreter
    runTDLibEventLoop,

    -- * low level functions
    runCommand,
    loop,
  )
where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.Loops
import Data.Aeson
import Data.ByteString.Lazy (toStrict)
import qualified Data.HashMap.Strict as HM
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M
import Data.Maybe
import Polysemy
import TDLib.Effect
import TDLib.Errors
import TDLib.Generated.Types hiding (Error (..), error)
import TDLib.TDJson

type Ans = TVar (IntMap Value)

type Locks = TVar (IntMap ())

type Counter = TVar Int

newCounter :: IO Counter
newCounter = newTVarIO 0

countUp :: Counter -> IO Int
countUp counter = atomically $ do
  i <- readTVar counter
  let n = i + 1
  writeTVar counter n
  pure n

lookupExtra :: Value -> Maybe Int
lookupExtra v@(Object hm) =
  case HM.lookup "@extra" hm of
    Nothing -> Nothing
    Just v' -> case fromJSON v' of
      Error _ -> throw $ ExtraFieldNotInt v
      Success i -> Just i
lookupExtra _ = error "Not a object"

insertAns :: Int -> Locks -> Ans -> Value -> STM ()
insertAns index lck ans val = do
  m <- readTVar lck
  let r = M.lookup index m
  if isJust r
    then writeTVar lck (M.delete index m)
    else modifyTVar ans (M.insert index val)

waitRead :: Int -> Ans -> STM Value
waitRead index ans = do
  m <- readTVar ans
  let mr = M.lookup index m
  case mr of
    Nothing -> retry
    Just v -> do
      writeTVar ans (M.delete index m)
      pure v

lock :: Int -> Locks -> STM ()
lock index lck = modifyTVar lck (M.insert index ())

readAns :: Int -> Locks -> Ans -> IO Value
readAns index lck ans =
  readV `onException` cleanUp
  where
    readV = atomically $ do
      waitRead index ans
    cleanUp = atomically $ do
      m <- readTVar ans
      let ma = M.lookup index m
      case ma of
        Nothing -> lock index lck
        _ -> writeTVar ans (M.delete index m)

-- | runs the event loop that receives updates from the client and dispatches them
loop :: Client -> Double -> Locks -> Ans -> (Update -> IO ()) -> IO a
loop client timeout lck ans cont = forever $ do
  bs <- untilJust $ clientReceive client timeout
  let m = decodeStrict bs
  case m of
    Nothing -> throwIO $ UnableToParseJSON bs
    Just v -> do
      case lookupExtra v of
        Nothing -> do
          let r = fromJSON v
          case r of
            Error _ -> throwIO $ UnableToParseValue v
            Success u -> cont u
        Just i -> atomically $ insertAns i lck ans v

-- | runs a command and waits for its answer
runCommand :: (ToJSON cmd, FromJSON res) => Client -> Int -> Locks -> Ans -> cmd -> IO res
runCommand client i lck ans cmd =
  case toJSON cmd of
    Object hm -> do
      let o' = Object (hm <> HM.fromList [("@extra" .= i)])
      clientSend client (toStrict $ encode o')
      v <- readAns i lck ans
      let m = fromJSON v
      case m of
        Error _ -> throwIO $ UnableToParseValue v
        Success r -> pure r
    v -> throwIO $ UnableToParseValue v

-- | runs the TDLib effect
runTDLibEventLoop :: Members '[Embed IO] r => Double -> (Update -> IO ()) -> Sem (TDLib ': r) a -> Sem r a
runTDLibEventLoop timeout cont m = do
  lck <- embed $ newTVarIO mempty
  ans <- embed $ newTVarIO mempty
  c <- embed newClient
  tid <- embed $ forkIO $ loop c timeout lck ans cont
  counter <- embed newCounter
  let runTD = interpret $ \case
        RunCmd cmd -> do
          i <- embed $ countUp counter
          embed $ runCommand c i lck ans cmd
        SetVerbosity verbosity -> do
          embed $ setLogVerbosityLevel verbosity
        SetFatalErrorCallback callback -> do
          embed $ setLogFatalErrorCallback callback
        SetLogPath path -> do
          embed $ setLogFilePath path
        SetLogMaxSize size -> do
          embed $ setLogMaxFileSize size
  r <- runTD m
  embed $ do
    killThread tid
    destroyClient c
  pure r
