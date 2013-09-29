{-# LANGUAGE ForeignFunctionInterface #-}
module AndroidNdk (module AndroidNdk, module AndroidNdk.Storable) where
import Control.Monad
import Data.Maybe
import Foreign.Storable
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array

import EGL
import OpenGLES
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


-- Process the next input event.
foreign export ccall "engineHandleInput" engineHandleInput :: Ptr AndroidApp -> Ptr AInputEvent -> IO Int
foreign import ccall "&engineHandleInput" p_engineHandleInput :: FunPtr (Ptr AndroidApp -> Ptr AInputEvent -> IO Int)

engineHandleInput :: Ptr AndroidApp -> Ptr AInputEvent -> IO Int
engineHandleInput app event = do
  apphs <- peek app
  let eng = appUserData apphs
  t <- c_AInputEvent_getType event
  if t /= c_AINPUT_EVENT_TYPE_MOTION then return 0
    else do enghs <- peek eng
            let stat = engState enghs
            x <- c_AMotionEvent_getX event 0
            y <- c_AMotionEvent_getY event 0
            let enghs' = enghs { engAnimating = 1
                               , engState = stat { sStateX = truncate x,  sStateY = truncate y } }
            poke eng enghs'
            return 1

-- Process the next main command.
foreign export ccall "engineHandleCmd" engineHandleCmd :: Ptr AndroidApp -> Int -> IO ()
foreign import ccall "&engineHandleCmd" p_engineHandleCmd :: FunPtr (Ptr AndroidApp -> Int -> IO ())

engineHandleCmd :: Ptr AndroidApp -> Int -> IO ()
engineHandleCmd app cmd = do
  apphs <- peek app
  let eng = appUserData apphs
  engineHandleCmd' eng cmd

engineHandleCmd' :: Ptr AndroidEngine -> Int -> IO ()
engineHandleCmd' eng cmd
  | cmd == c_APP_CMD_SAVE_STATE = do enghs <- peek eng
                                     let app = engApp enghs
                                     apphs <- peek app
                                     sstat <- malloc
                                     poke sstat $ engState enghs
                                     let apphs' = apphs { appSavedState = sstat
                                                        , appSavedStateSize = toEnum $ sizeOf $ engState enghs }
                                     poke app apphs'
  | cmd == c_APP_CMD_INIT_WINDOW = do enghs <- peek eng
                                      let app = engApp enghs
                                      apphs <- peek app
                                      when (appWindow apphs /= nullPtr) $ do
                                        engineInitDisplay eng
                                        engineDrawFrame eng
  | cmd == c_APP_CMD_TERM_WINDOW = engineTermDisplay eng
  | cmd == c_APP_CMD_GAINED_FOCUS = do enghs <- peek eng
                                       when (engAccelerometerSensor enghs /= nullPtr) $ do
                                         c_ASensorEventQueue_enableSensor (engSensorEventQueue enghs) (engAccelerometerSensor enghs)
                                         c_ASensorEventQueue_setEventRate (engSensorEventQueue enghs) (engAccelerometerSensor enghs) ((1000 `div` 60) * 1000)
                                         return ()
  | cmd == c_APP_CMD_LOST_FOCUS = do enghs <- peek eng
                                     when (engAccelerometerSensor enghs /= nullPtr) $ do
                                       c_ASensorEventQueue_disableSensor (engSensorEventQueue enghs) (engAccelerometerSensor enghs)
                                       return ()
                                     let enghs' = enghs { engAnimating = 0 }
                                     poke eng enghs'
                                     engineDrawFrame eng
engineHandleCmd' _ _ = return ()


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


-- Just the current frame in the display.
engineDrawFrame :: Ptr AndroidEngine -> IO ()
engineDrawFrame eng = peek eng >>= go
  where go :: AndroidEngine -> IO ()
        go enghs = do
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
engineInitDisplay :: Ptr AndroidEngine -> IO Int
engineInitDisplay eng = peek eng >>= go >>= maybe (return (-1)) (\r -> poke eng r >> return 0)
  where go :: AndroidEngine -> IO (Maybe AndroidEngine)
        go enghs = do
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
