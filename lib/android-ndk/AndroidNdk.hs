{-# LANGUAGE ForeignFunctionInterface #-}
module AndroidNdk (module AndroidNdk, module AndroidNdk.Storable, module AndroidNdk.EGL, module AndroidNdk.OpenGLES) where
import Control.Monad
import Data.Maybe
import Foreign.Storable
import Foreign.C.Types
import Foreign.Ptr

import AndroidNdk.EGL
import AndroidNdk.OpenGLES
import AndroidNdk.Storable


type CSSize = Int
foreign import primitive "const.APP_CMD_SAVE_STATE" c_APP_CMD_SAVE_STATE :: Int
foreign import primitive "const.APP_CMD_INIT_WINDOW" c_APP_CMD_INIT_WINDOW :: Int
foreign import primitive "const.APP_CMD_TERM_WINDOW" c_APP_CMD_TERM_WINDOW :: Int
foreign import primitive "const.APP_CMD_GAINED_FOCUS" c_APP_CMD_GAINED_FOCUS :: Int
foreign import primitive "const.APP_CMD_LOST_FOCUS" c_APP_CMD_LOST_FOCUS :: Int

foreign import ccall "c_extern.h ASensorEventQueue_enableSensor" c_ASensorEventQueue_enableSensor :: Ptr ASensorEventQueue -> Ptr ASensor -> IO Int
foreign import ccall "c_extern.h ASensorEventQueue_setEventRate" c_ASensorEventQueue_setEventRate :: Ptr ASensorEventQueue -> Ptr ASensor -> Int -> IO Int
foreign import ccall "c_extern.h ANativeWindow_setBuffersGeometry" c_ANativeWindow_setBuffersGeometry :: Ptr ANativeWindow -> Int -> Int -> Int -> IO Int
foreign import ccall "c_extern.h ASensorEventQueue_disableSensor" c_ASensorEventQueue_disableSensor :: Ptr ASensorEventQueue -> Ptr ASensor -> IO Int
foreign import ccall "c_extern.h ASensorManager_getInstance" c_ASensorManager_getInstance :: IO (Ptr ASensorManager)
foreign import ccall "c_extern.h ASensorManager_getDefaultSensor" c_ASensorManager_getDefaultSensor :: Ptr ASensorManager -> Int -> IO (Ptr ASensor)
foreign import ccall "c_extern.h ASensorManager_createEventQueue" c_ASensorManager_createEventQueue :: Ptr ASensorManager -> Ptr ALooper -> Int -> ALooper_callbackFunc -> Ptr () -> IO (Ptr ASensorEventQueue)
foreign import ccall "c_extern.h ALooper_pollAll" c_ALooper_pollAll :: Int -> Ptr Int -> Ptr Int -> Ptr (Ptr ()) -> IO Int
foreign import ccall "c_extern.h ASensorEventQueue_getEvents" c_ASensorEventQueue_getEvents :: Ptr ASensorEventQueue -> Ptr ASensorEvent -> CSize -> IO CSSize
