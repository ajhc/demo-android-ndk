{-# LANGUAGE ForeignFunctionInterface #-}
import Control.Monad
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
  -- Prepare to monitor accelerometer
  sManage <- c_ASensorManager_getInstance
  accel <- c_ASensorManager_getDefaultSensor sManage c_ASENSOR_TYPE_ACCELEROMETER
  let looper = appLooper apphs'
  sEvent <- c_ASensorManager_createEventQueue sManage looper c_LOOPER_ID_USER nullPtr nullPtr
  poke eng $ enghs { engApp = app
                   , engSensorManager = sManage
                   , engAccelerometerSensor = accel
                   , engSensorEventQueue = sEvent }
  let ss_p = appSavedState apphs'
  when (ss_p /= nullPtr) $ do
    ss <- peek ss_p
    peek eng >>= (\e -> return $ e {engState = ss}) >>= poke eng
  return ()
