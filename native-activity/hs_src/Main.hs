{-# LANGUAGE ForeignFunctionInterface #-}
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import AndroidNdk

-- Dummy
main :: IO ()
main = return ()

-- True main
foreign export ccall "androidMain" androidMain :: Ptr AndroidApp -> IO ()
androidMain :: Ptr AndroidApp -> IO ()
androidMain app = do
  eng <- malloc
  poke eng defaultAndroidEngine
  apphs <- peek app
  let apphs' = apphs { appUserData = eng, appOnAppCmd = p_engine_handle_cmd , appOnInputEvent = p_engine_handle_input }
  poke app apphs'
  enghs <- peek eng
  poke eng $ enghs { engApp = app }
  return ()

