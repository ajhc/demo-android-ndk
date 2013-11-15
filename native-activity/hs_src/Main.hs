{-# LANGUAGE ForeignFunctionInterface #-}
import Control.Monad
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array

import AndroidNdk

-- some magic
androidActs :: AndroidNdkActs
androidActs = AndroidNdkActs { drawFrame = engineDrawFrame
                             , initDisplay = engineInitDisplay
                             , handleInput = eHandleInput
                             , handleCmd = eHandleCmd }

foreign export ccall "engineHandleInput" engineHandleInput :: FuncHandleInput
foreign import ccall "&engineHandleInput" p_engineHandleInput :: FunPtr FuncHandleInput
engineHandleInput :: FuncHandleInput
engineHandleInput = handleInputHs androidActs

foreign export ccall "engineHandleCmd" engineHandleCmd :: FuncHandleCmd
foreign import ccall "&engineHandleCmd" p_engineHandleCmd :: FunPtr FuncHandleCmd
engineHandleCmd :: FuncHandleCmd
engineHandleCmd = handleCmdHs androidActs

-- Dummy main
main :: IO ()
main = return ()
-- True main
foreign export ccall "androidMain" androidMain :: Ptr AndroidApp -> IO ()
androidMain :: Ptr AndroidApp -> IO ()
androidMain = androidMainHs androidActs p_engineHandleInput p_engineHandleCmd

-- Process the next input event.
eHandleInput :: AndroidEngine -> AInputEventType -> AMotionEventAction -> (Float, Float) -> IO (Maybe AndroidEngine)
eHandleInput eng = go
  where go AInputEventTypeMotion _ (x, y) = do
          let stat = engState eng
          return (Just $ eng { engAnimating = 1
                             , engState = stat { sStateX = truncate x,  sStateY = truncate y } })
        go _ _ _ = return Nothing

-- Process the next main command.
eHandleCmd :: (AndroidApp, AndroidEngine) -> AAppCmd -> IO (Maybe AndroidApp, Maybe AndroidEngine)
eHandleCmd (app, eng) = go
  where go AAppCmdSaveState = do
          sstat <- malloc
          poke sstat $ engState eng
          return (Just $ app { appSavedState = sstat
                             , appSavedStateSize = toEnum $ sizeOf $ engState eng }, Nothing)
        go AAppCmdInitWindow | appWindow app /= nullPtr = do
          (Just eng') <- initDisplayHs androidActs eng
          engineDrawFrame eng'
          return (Nothing, Just eng')
        go AAppCmdTermWindow = do
          eng' <- engineTermDisplay eng
          return (Nothing, Just eng')
        go AAppCmdGainedFocus | engAccelerometerSensor eng /= nullPtr = do
          c_ASensorEventQueue_enableSensor (engSensorEventQueue eng) (engAccelerometerSensor eng)
          c_ASensorEventQueue_setEventRate (engSensorEventQueue eng) (engAccelerometerSensor eng) ((1000 `div` 60) * 1000)
          return (Nothing, Nothing)
        go AAppCmdLostFocus = do
          when (engAccelerometerSensor eng /= nullPtr) $ void $
            c_ASensorEventQueue_disableSensor (engSensorEventQueue eng) (engAccelerometerSensor eng)
          let eng' = eng { engAnimating = 0 }
          engineDrawFrame eng'
          return (Nothing, Just eng')
        go _ = return (Nothing, Nothing)


-- Just the current frame in the display.
engineDrawFrame :: AndroidEngine -> IO ()
engineDrawFrame enghs = do
  let disp  = engEglDisplay enghs
      surf  = engEglSurface enghs
      w     = fromIntegral $ engWidth enghs
      h     = fromIntegral $ engHeight enghs
      s     = engState enghs
      x     = fromIntegral $ sStateX s
      y     = fromIntegral $ sStateY s
      angle = sStateAngle s
  when (disp /= c_EGL_NO_DISPLAY) $ do
    c_glClearColor (x/w) angle (y/h) 1.0
    c_glClear c_GL_COLOR_BUFFER_BIT
    void $ c_eglSwapBuffers disp surf


-- Initialize an EGL context for the current display.
engineInitDisplay :: (GLint, GLint) -> IO ()
engineInitDisplay (w, h) = do
  c_glHint       c_GL_PERSPECTIVE_CORRECTION_HINT c_GL_FASTEST
  c_glEnable     c_GL_CULL_FACE
  c_glShadeModel c_GL_SMOOTH
  c_glDisable    c_GL_DEPTH_TEST
