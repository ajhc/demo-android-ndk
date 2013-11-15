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
                             , fpHandleInput = p_engineHandleInput
                             , fpHandleCmd = p_engineHandleCmd
                             , handleInput = eHandleInput
                             , handleCmd = eHandleCmd }

foreign export ccall "engineHandleInput" engineHandleInput :: Ptr AndroidApp -> Ptr AInputEvent -> IO Int
foreign import ccall "&engineHandleInput" p_engineHandleInput :: FunPtr (Ptr AndroidApp -> Ptr AInputEvent -> IO Int)
engineHandleInput :: Ptr AndroidApp -> Ptr AInputEvent -> IO Int
engineHandleInput = handleInputHs androidActs

foreign export ccall "engineHandleCmd" engineHandleCmd :: Ptr AndroidApp -> Int -> IO ()
foreign import ccall "&engineHandleCmd" p_engineHandleCmd :: FunPtr (Ptr AndroidApp -> Int -> IO ())
engineHandleCmd :: Ptr AndroidApp -> Int -> IO ()
engineHandleCmd = handleCmdHs androidActs

-- Dummy main
main :: IO ()
main = return ()
-- True main
foreign export ccall "androidMain" androidMain :: Ptr AndroidApp -> IO ()
androidMain :: Ptr AndroidApp -> IO ()
androidMain = androidMainHs androidActs

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
          (Just eng') <- engineInitDisplay eng
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
engineInitDisplay :: AndroidEngine -> IO (Maybe AndroidEngine)
engineInitDisplay enghs = do
  disp <- c_eglGetDisplay c_EGL_DEFAULT_DISPLAY
  c_eglInitialize disp nullPtr nullPtr
  let attribsHs = [ c_EGL_SURFACE_TYPE, c_EGL_WINDOW_BIT,
                    c_EGL_BLUE_SIZE,  8,
                    c_EGL_GREEN_SIZE, 8,
                    c_EGL_RED_SIZE,   8,
                    c_EGL_NONE ]
  alloca $ \config_p -> alloca $ \numConfigs_p -> alloca $ \format_p -> withArray attribsHs $ \attribs -> do
    c_eglChooseConfig disp attribs config_p 1 numConfigs_p
    config <- peek config_p
    c_eglGetConfigAttrib disp config c_EGL_NATIVE_VISUAL_ID format_p
    format <- peek format_p
    apphs <- peek $ engApp enghs
    let win = appWindow apphs
    c_ANativeWindow_setBuffersGeometry win 0 0 format
    surf <- c_eglCreateWindowSurface disp config (castPtr win) nullPtr
    cont <- c_eglCreateContext disp config nullPtr nullPtr
    b <- c_eglMakeCurrent disp surf surf cont
    if b == c_EGL_FALSE then return Nothing
      else alloca $ \w_p -> alloca $ \h_p -> do
        c_eglQuerySurface disp surf c_EGL_WIDTH w_p
        c_eglQuerySurface disp surf c_EGL_HEIGHT h_p
        w <- peek w_p
        h <- peek h_p
        c_glHint       c_GL_PERSPECTIVE_CORRECTION_HINT c_GL_FASTEST
        c_glEnable     c_GL_CULL_FACE
        c_glShadeModel c_GL_SMOOTH
        c_glDisable    c_GL_DEPTH_TEST
        let stat = engState enghs
        return . Just $ enghs { engEglDisplay = disp
                              , engEglContext = cont
                              , engEglSurface = surf
                              , engWidth      = w
                              , engHeight     = h
                              , engState      = stat { sStateAngle = 0 } }
