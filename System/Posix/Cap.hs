{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module System.Posix.Cap
  ( Cap, CapFlag, CapValue

  , newCap, dupCap

  , capGetProc, capSetProc, capGetPid

  , capToText, capFromText, capFromName, capToName

  ) where

import Control.Monad
import Foreign.C
import Foreign.Concurrent (newForeignPtr)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Marshal (alloca, withArrayLen)
import Foreign.Storable (Storable, peek)
import Foreign.Ptr
import System.Posix

data CapStruct
newtype Cap = Cap (ForeignPtr CapStruct)
type CapPtr = Ptr CapStruct
newtype CapFlag = CapFlag CInt
newtype CapValue = CapValue CInt

effective = CapFlag 0
permitted = CapFlag 1
inheritable = CapFlag 2

newCap :: IO Cap
newCap = wrapCapPtr =<< cap_init

dupCap :: Cap -> IO Cap
dupCap cap = withCapPtr cap (wrapCapPtr <=< cap_dup)

capGetProc :: IO Cap
capGetProc = wrapCapPtr =<< cap_get_proc

capSetProc :: Cap -> IO ()
capSetProc cap = do
  withCapPtr cap cap_set_proc
  return ()

capGetPid :: ProcessID -> IO Cap
capGetPid = wrapCapPtr <=< cap_get_pid

capToText :: Cap -> IO String
capToText cap = do
  textPtr <- withCapPtr cap $ \ ptr -> cap_to_text ptr nullPtr
  text <- peekCString textPtr
  cap_free_string textPtr
  return text

capFromText :: String -> IO Cap
capFromText text = wrapCapPtr =<< withCString text cap_from_text

capGetFlag :: Cap -> CapValue -> CapFlag -> IO Bool
capGetFlag cap val flag =
  alloca $ \ out -> do
  withCapPtr cap $ \ ptr -> cap_get_flag ptr val flag out
  peek out

capSetFlags :: Cap -> CapFlag -> [CapValue] -> Bool -> IO ()
capSetFlags cap flag vals flagval =
  withArrayLen [v | CapValue v <- vals] $ \ n caps ->
  withCapPtr cap $ \ ptr -> do
  cap_set_flag ptr flag (fromIntegral n) caps flagval
  return ()

capClear :: Cap -> IO ()
capClear cap = do
  withCapPtr cap cap_clear
  return ()

capClearFlag :: Cap -> CapFlag -> IO ()
capClearFlag cap flag = do
  withCapPtr cap $ \ ptr -> cap_clear_flag ptr flag
  return ()

capFromName :: String -> IO (Maybe CapValue)
capFromName name =
  alloca $ \ capPtr ->
  withCString name $ \ namePtr -> do
  res <- cap_from_name namePtr capPtr
  if res == 0
    then fmap (Just . CapValue) (peek capPtr)
    else return Nothing

capToName :: CapValue -> IO String
capToName val = do
  ptr <- cap_to_name val
  name <- peekCString ptr
  cap_free_string ptr
  return name
  
withCapPtr :: Cap -> (CapPtr -> IO a) -> IO a
withCapPtr (Cap fp) = withForeignPtr fp

wrapCapPtr :: CapPtr -> IO Cap
wrapCapPtr ptr = Cap `fmap` newForeignPtr ptr (cap_free_cap ptr >> return ())

foreign import ccall unsafe "sys/capabilities.h cap_init" cap_init
  :: IO CapPtr
foreign import ccall unsafe "sys/capabilities.h cap_dup" cap_dup
  :: CapPtr -> IO CapPtr
foreign import ccall unsafe "sys/capabilities.h cap_free" cap_free_cap
  :: CapPtr -> IO CInt
foreign import ccall unsafe "sys/capabilities.h cap_free" cap_free_string
  :: CString -> IO CInt

foreign import ccall unsafe "sys/capabilities.h cap_get_flag" cap_get_flag
  :: CapPtr -> CapValue -> CapFlag -> Ptr Bool -> IO CInt
foreign import ccall unsafe "sys/capabilities.h cap_set_flag" cap_set_flag
  :: CapPtr -> CapFlag -> CInt -> Ptr CInt -> Bool -> IO CInt
foreign import ccall unsafe "sys/capabilities.h cap_clear" cap_clear
  :: CapPtr -> IO CInt
foreign import ccall unsafe "sys/capabilities.h cap_clear_flag" cap_clear_flag
  :: CapPtr -> CapFlag -> IO CInt

foreign import ccall unsafe "sys/capabilities.h cap_set_proc" cap_set_proc
  :: CapPtr -> IO CInt
foreign import ccall unsafe "sys/capabilities.h cap_get_proc" cap_get_proc
  :: IO CapPtr
foreign import ccall unsafe "sys/capabilities.h cap_get_pid" cap_get_pid
  :: ProcessID -> IO CapPtr

foreign import ccall unsafe "sys/capabilities.h cap_to_text" cap_to_text
  :: CapPtr -> Ptr CInt -> IO CString
foreign import ccall unsafe "sys/capabilities.h cap_from_text" cap_from_text
  :: CString -> IO CapPtr
foreign import ccall unsafe "sys/capabilities.h cap_from_name" cap_from_name
  :: CString -> Ptr CInt -> IO CInt
foreign import ccall unsafe "sys/capabilities.h cap_to_name" cap_to_name
  :: CapValue -> IO CString
