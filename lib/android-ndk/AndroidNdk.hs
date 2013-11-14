{-# LANGUAGE ForeignFunctionInterface #-}
module AndroidNdk (module AndroidNdk, module AndroidNdk.Storable, module AndroidNdk.EGL, module AndroidNdk.OpenGLES) where
import Control.Monad
import Data.Maybe
import Foreign.Storable
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc

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

data AndroidNdkActs = AndroidNdkActs { drawFrame :: AndroidEngine -> IO () }

while :: IO Bool -> IO ()
while f = do r <- f
             when r $ while f

-- Read all pending events.
-- If not animating, we will block forever waiting for events.
-- If animating, we loop until all events are read, then continue
-- to draw the next frame of animation.
pollEvents :: AndroidNdkActs -> Ptr AndroidApp -> Ptr AndroidEngine -> Ptr Int -> Ptr (Ptr AndroidPollSource)  -> IO Bool
pollEvents acts app eng events source = do
  r <- pollEventWhile app eng events source
  if r == False then return False -- exit
    else do
      enghs' <- peek eng
      when (engAnimating enghs' /= 0) $ do
        -- Done with events; draw next animation frame.
        let stat      = engState $ enghs'
            newAngle  = sStateAngle stat + 0.01
            newAngle' = if newAngle > 1 then 0 else newAngle
            enghs''   = enghs' { engState = stat { sStateAngle = newAngle' } }
        poke eng enghs''
        -- Drawing is throttled to the screen update rate, so there is no need to do timing here.
        drawFrame acts $ enghs''
      return True

pollEventWhile :: Ptr AndroidApp -> Ptr AndroidEngine -> Ptr Int -> Ptr (Ptr AndroidPollSource)  -> IO Bool
pollEventWhile app eng events source = do
  enghs <- peek eng
  ident <- c_ALooper_pollAll (if engAnimating enghs /= 0 then 0 else (-1)) nullPtr events (castPtr source)
  if ident < 0 then return True
    else do
      source' <- peek source
      -- Process this event.
      when (source' /= nullPtr) $ do
        sourcehs <- peek source'
        (mkFun_AndroidPollSource_pollProcess . pollProcess $ sourcehs) app source'
      -- If a sensor has data, process it now.
      when (ident == c_LOOPER_ID_USER) $ do
        enghs' <- peek eng
        when (engAccelerometerSensor enghs' /= nullPtr) $ do
          allocaBytes sizeOf_ASensorEvent (\ase -> while $ getEventQueue eng ase)
      -- Check if we are exiting.
      apphs <- peek app
      if appDestroyRequested apphs /= 0 then engineTermDisplay eng >> return False -- exit
        else pollEventWhile app eng events source

getEventQueue :: Ptr AndroidEngine -> Ptr ASensorEvent -> IO Bool
getEventQueue eng asevent = do
  enghs <- peek eng
  r <- c_ASensorEventQueue_getEvents (engSensorEventQueue enghs) asevent 1
  if r > 0 then
    -- LOGI("accelerometer: x=%f y=%f z=%f",
    --      event.acceleration.x, event.acceleration.y,
    --      event.acceleration.z);
    return True
    else return False

-- Tear down the EGL context currently associated with the display.
engineTermDisplay :: Ptr AndroidEngine -> IO ()
engineTermDisplay eng = peek eng >>= go >>= poke eng
  where go :: AndroidEngine -> IO AndroidEngine
        go enghs = do
          let disp = engEglDisplay enghs
              cont = engEglContext enghs
              surf = engEglSurface enghs
          when (disp /= c_EGL_NO_DISPLAY) $ do
            c_eglMakeCurrent disp c_EGL_NO_SURFACE c_EGL_NO_SURFACE c_EGL_NO_CONTEXT
            when (cont /= c_EGL_NO_CONTEXT) (void $ c_eglDestroyContext disp cont)
            when (surf /= c_EGL_NO_SURFACE) (void $ c_eglDestroySurface disp surf)
            void $ c_eglTerminate disp
          return $ enghs { engAnimating  = 0
                         , engEglDisplay = c_EGL_NO_DISPLAY
                         , engEglSurface = c_EGL_NO_SURFACE
                         , engEglContext = c_EGL_NO_CONTEXT }
