{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Bindings to TDLib Json interface
module TDLib.TDJson
  ( Verbosity (..),
    Client,

    -- * Creating, Destroying and Interacting with clients
    newClient,
    destroyClient,
    clientReceive,
    clientSend,
    clientExecute,

    -- * Managing the internal logging of TDLib
    setLogFilePath,
    setLogMaxFileSize,
    setLogVerbosityLevel,
    setLogFatalErrorCallback,
  )
where

import Data.ByteString
  ( ByteString,
    packCString,
    useAsCString,
  )
import Data.Int
import Foreign.C
import Foreign.ForeignPtr
import Foreign.Ptr

-- | TDLib client, will be automacially destroyed as soon as there are no references pointing to it (backed by 'ForeignPtr')
newtype Client = Client (ForeignPtr ())
  deriving newtype (Eq, Ord, Show)

type ClientPtr = Ptr ()

-- | Logging verbosity
data Verbosity
  = Fatal
  | Error
  | Warning
  | Info
  | Debug
  | Verbose
  deriving (Show, Eq, Enum)

foreign import ccall "td_json_client_create"
  tdJsonClientCreate :: IO ClientPtr

foreign import ccall "td_json_client_send"
  tdJsonClientSend :: ClientPtr -> CString -> IO ()

foreign import ccall "td_json_client_receive"
  tdJsonClientReceive :: ClientPtr -> CDouble -> IO CString

foreign import ccall "Td_json_client_execute"
  tdJsonClientExecute :: ClientPtr -> CString -> IO ()

foreign import ccall "td_json_client_destroy"
  tdJsonClientDestroy :: ClientPtr -> IO ()

foreign import ccall "&td_json_client_destroy"
  p_clientDestory :: FunPtr (ClientPtr -> IO ())

foreign import ccall "td_set_log_file_path"
  tdSetLogFilePath :: CString -> IO CInt

foreign import ccall "td_set_log_max_file_size"
  tdSetLogMaxFileSize :: CLLong -> IO ()

foreign import ccall "td_set_log_verbosity_level"
  tdSetLogVerbosityLevel :: CInt -> IO ()

type CallbackPtr = FunPtr (CString -> IO ())

foreign import ccall "td_set_log_fatal_error_callback"
  tdSetLogFatalErrorCallback :: CallbackPtr -> IO ()

foreign import ccall "wrapper"
  mkCallbackPtr_ :: (CString -> IO ()) -> IO CallbackPtr

mkCallbackPtr :: (ByteString -> IO ()) -> IO CallbackPtr
mkCallbackPtr cont =
  mkCallbackPtr_ cont'
  where
    cont' cs = do
      bs <- packCString cs
      cont bs

-- | Creates a new instance of TDLib.
newClient :: IO Client
newClient = do
  cptr <- tdJsonClientCreate
  fptr <- newForeignPtr p_clientDestory cptr
  pure $ Client fptr

-- | Sends request to the TDLib client. May be called from any thread.
clientSend ::
  -- | The client.
  Client ->
  -- | JSON-serialized null-terminated request to TDLib.
  ByteString ->
  IO ()
clientSend (Client fptr) msg =
  useAsCString msg $ \cstr ->
    withForeignPtr fptr $ \ptr -> do
      tdJsonClientSend ptr cstr

-- | Receives incoming updates and request responses from the TDLib client. May be called from any thread, but shouldn't be called simultaneously from two different threads. Returned pointer will be deallocated by TDLib during next call to 'clientReceive' or 'clientExecute' in the same thread, so it can't be used after that.
clientReceive ::
  -- | The client.
  Client ->
  -- | The maximum number of seconds allowed for this function to wait for new data.
  Double ->
  -- | JSON-serialized null-terminated incoming update or request response. May be NULL if the timeout expires.
  IO ByteString
clientReceive (Client fptr) t =
  withForeignPtr fptr $ \ptr -> do
    cs <- tdJsonClientReceive ptr (CDouble t)
    packCString cs

-- | Synchronously executes TDLib request. May be called from any thread. Only a few requests can be executed synchronously. Returned pointer will be deallocated by TDLib during next call to 'clientReceive' or 'clientExecute' in the same thread, so it can't be used after that.
clientExecute ::
  -- | The client. Currently ignored for all requests, so NULL can be passed.
  Client ->
  -- | JSON-serialized null-terminated request to TDLib.
  ByteString ->
  IO ()
clientExecute (Client fptr) cmd =
  useAsCString cmd $ \cstr ->
    withForeignPtr fptr $ \ptr -> do
      tdJsonClientExecute ptr cstr

-- | Destroys the TDLib client instance. After this is called the client instance shouldn't be used anymore.
destroyClient ::
  -- | The client.
  Client ->
  IO ()
destroyClient (Client fptr) = finalizeForeignPtr fptr

-- | Sets the path to the file where the internal TDLib log will be written. By default TDLib writes logs to stderr or an OS specific log. Use this method to write the log to a file instead.
setLogFilePath ::
  -- | Null-terminated path to a file where the internal TDLib log will be written. Use an empty path to switch back to the default logging behaviour.
  ByteString ->
  -- | True on success, False otherwise.
  IO Bool
setLogFilePath fp =
  useAsCString fp $ \cstr -> do
    i <- tdSetLogFilePath cstr
    if  | i == 1 -> pure True
        | i == 0 -> pure False
        | otherwise -> error $ "Unknown return code" <> show i

-- | Sets the maximum size of the file to where the internal TDLib log is written before the file will be auto-rotated. Unused if log is not written to a file. Defaults to 10 MB.
setLogMaxFileSize ::
  -- | The maximum size of the file to where the internal TDLib log is written before the file will be auto-rotated. Should be positive.
  Int64 ->
  IO ()
setLogMaxFileSize = tdSetLogMaxFileSize . CLLong

-- | Sets the verbosity level of the internal logging of TDLib. By default the TDLib uses a log verbosity level of 'Verbose'.
setLogVerbosityLevel :: Verbosity -> IO ()
setLogVerbosityLevel = tdSetLogVerbosityLevel . toEnum . fromEnum

-- | Sets the callback that will be called when a fatal error happens. None of the TDLib methods can be called from the callback. The TDLib will crash as soon as callback returns. By default the callback is not set.
setLogFatalErrorCallback :: (ByteString -> IO ()) -> IO ()
setLogFatalErrorCallback cont = do
  cbptr <- mkCallbackPtr cont
  tdSetLogFatalErrorCallback cbptr
