{-# LANGUAGE ForeignFunctionInterface #-}
import Control.Monad
import Data.Bits
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array

import AndroidNdk
import CubeVertices

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
  where go AInputEventTypeMotion AMotionEventActionUp _ = return Nothing
        go AInputEventTypeMotion act (x,y) = do
          let stat = engState eng
              ox = if act == AMotionEventActionDown then x else fromIntegral $ sStateX stat
              oy = if act == AMotionEventActionDown then y else fromIntegral $ sStateY stat
          return (Just $ eng { engAnimating = 1
                             , engState = stat { sStateX  = truncate x
                                               , sStateY  = truncate y
                                               , sStateDx = truncate $ x - ox
                                               , sStateDy = truncate $ y - oy } })
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
      dx    = fromIntegral $ sStateDx s
      dy    = fromIntegral $ sStateDy s
      angle = sStateAngle s
  when (disp /= c_EGL_NO_DISPLAY) $ do
    c_glClear $ c_GL_COLOR_BUFFER_BIT .|. c_GL_DEPTH_BUFFER_BIT
    withArray vertices $ \vp -> withArray colors $ \cp -> do -- xxx heavy
      c_glEnableClientState c_GL_VERTEX_ARRAY
      c_glEnableClientState c_GL_COLOR_ARRAY
      c_glVertexPointer 3 c_GL_FLOAT 0 vp
      c_glColorPointer 4 c_GL_FLOAT 0 cp
      c_glRotatef ((sqrt (dx ** 2 + dy ** 2)) / 10.0) dy dx 0.0
      c_glDrawArrays c_GL_TRIANGLES 0 36
      c_glDisableClientState c_GL_VERTEX_ARRAY
      c_glDisableClientState c_GL_COLOR_ARRAY
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
              c_glClearColor 0.0 0.0 0.0 1.0
              c_glViewport 0 0 w h
              c_glMatrixMode c_GL_PROJECTION
              c_glLoadIdentity
              let nearClip = (- 2.0)
                  farClip  = 2.0
                  yFOV  = 75.0
                  yMax = nearClip * tan (yFOV * pi / 360.0)
                  aspect = fromIntegral w / fromIntegral h
                  xMin = (-yMax) * aspect
                  xMax = yMax * aspect
              c_glFrustumf xMin xMax (- yMax) yMax nearClip farClip
              if w > h then c_glScalef (fromIntegral h / fromIntegral w) 1.0 1.0
                else c_glScalef 1.0 (fromIntegral w / fromIntegral h) 1.0
              c_glMatrixMode c_GL_MODELVIEW
              c_glLoadIdentity

              let stat = engState enghs
              return . Just $ enghs { engEglDisplay = disp
                                    , engEglContext = cont
                                    , engEglSurface = surf
                                    , engWidth      = w
                                    , engHeight     = h
                                    , engState      = stat { sStateAngle = 0 } }
