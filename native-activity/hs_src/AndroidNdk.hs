{-# LANGUAGE ForeignFunctionInterface #-}
module AndroidNdk where
import Control.Monad
import Data.Word
import Foreign.Storable
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc

-- struct saved_state
foreign import primitive "const.sizeof(struct saved_state)" sizeOf_SavedState :: Int
foreign import primitive "const.offsetof(struct saved_state, angle)" offsetOf_SavedState_angle :: Int
foreign import primitive "const.offsetof(struct saved_state, x)" offsetOf_SavedState_x :: Int
foreign import primitive "const.offsetof(struct saved_state, y)" offsetOf_SavedState_y :: Int

data SavedState = SavedState { sStateAngle :: Float
                             , sStateX     :: Int
                             , sStateY     :: Int }
instance Storable SavedState where
  sizeOf    = const sizeOf_SavedState
  alignment = sizeOf
  poke p sstat = do
    pokeByteOff p offsetOf_SavedState_angle $ sStateAngle sstat
    pokeByteOff p offsetOf_SavedState_x     $ sStateX sstat
    pokeByteOff p offsetOf_SavedState_y     $ sStateY sstat
  peek p = do
    angle <- peekByteOff p offsetOf_SavedState_angle
    x     <- peekByteOff p offsetOf_SavedState_x
    y     <- peekByteOff p offsetOf_SavedState_y
    return $ SavedState { sStateAngle = angle, sStateX = x, sStateY = y }

-- struct engine
foreign import primitive "const.sizeof(struct engine)" sizeOf_Engine :: Int
foreign import primitive "const.offsetof(struct engine, app)" offsetOf_Engine_app :: Int
foreign import primitive "const.offsetof(struct engine, sensorManager)" offsetOf_Engine_sensorManager :: Int
foreign import primitive "const.offsetof(struct engine, accelerometerSensor)" offsetOf_Engine_accelerometerSensor :: Int
foreign import primitive "const.offsetof(struct engine, sensorEventQueue)" offsetOf_Engine_sensorEventQueue :: Int
foreign import primitive "const.offsetof(struct engine, animating)" offsetOf_Engine_animating :: Int
foreign import primitive "const.offsetof(struct engine, display)" offsetOf_Engine_display :: Int
foreign import primitive "const.offsetof(struct engine, surface)" offsetOf_Engine_surface :: Int
foreign import primitive "const.offsetof(struct engine, context)" offsetOf_Engine_context :: Int
foreign import primitive "const.offsetof(struct engine, width)" offsetOf_Engine_width :: Int
foreign import primitive "const.offsetof(struct engine, height)" offsetOf_Engine_height :: Int
foreign import primitive "const.offsetof(struct engine, state)" offsetOf_Engine_state :: Int
foreign import primitive "const.sizeof(EGLDisplay)" sizeOf_EGLDisplay :: Int
foreign import primitive "const.sizeof(EGLSurface)" sizeOf_EGLSurface :: Int
foreign import primitive "const.sizeof(EGLContext)" sizeOf_EGLContext :: Int

newtype {-# CTYPE "ASensorManager" #-}    ASensorManager    = ASensorManager ()
newtype {-# CTYPE "ASensor" #-}           ASensor           = ASensor ()
newtype {-# CTYPE "ASensorEventQueue" #-} ASensorEventQueue = ASensorEventQueue ()
type EGLDisplay = Ptr () -- xxx unsafe
type EGLSurface = Ptr ()
type EGLContext = Ptr ()
c_EGL_NO_DISPLAY = nullPtr
c_EGL_NO_SURFACE = nullPtr
c_EGL_NO_CONTEXT = nullPtr

data AndroidEngine = AndroidEngine { engApp                 :: Ptr AndroidApp
                                   , engSensorManager       :: Ptr ASensorManager
                                   , engAccelerometerSensor :: Ptr ASensor
                                   , engSensorEventQueue    :: Ptr ASensorEventQueue
                                   , engAnimating           :: Int
                                   , engWidth               :: Int
                                   , engHeight              :: Int
                                   , engState               :: SavedState
                                   , engEglDisplay          :: EGLDisplay
                                   , engEglSurface          :: EGLSurface
                                   , engEglContext          :: EGLContext }
instance Storable AndroidEngine where
  sizeOf    = const sizeOf_Engine
  alignment = sizeOf
  poke p eng = do
    pokeByteOff p offsetOf_Engine_app                 $ engApp eng
    pokeByteOff p offsetOf_Engine_sensorManager       $ engSensorManager eng
    pokeByteOff p offsetOf_Engine_accelerometerSensor $ engAccelerometerSensor eng
    pokeByteOff p offsetOf_Engine_sensorEventQueue    $ engSensorEventQueue eng
    pokeByteOff p offsetOf_Engine_animating           $ engAnimating eng
    pokeByteOff p offsetOf_Engine_width               $ engWidth eng
    pokeByteOff p offsetOf_Engine_height              $ engHeight eng
    pokeByteOff p offsetOf_Engine_state               $ engState eng
    pokeByteOff p offsetOf_Engine_display             $ engEglDisplay eng
    pokeByteOff p offsetOf_Engine_surface             $ engEglSurface eng
    pokeByteOff p offsetOf_Engine_context             $ engEglContext eng
  peek p = do
    app                 <- peekByteOff p offsetOf_Engine_app
    sensorManager       <- peekByteOff p offsetOf_Engine_sensorManager
    accelerometerSensor <- peekByteOff p offsetOf_Engine_accelerometerSensor
    sensorEventQueue    <- peekByteOff p offsetOf_Engine_sensorEventQueue
    animating           <- peekByteOff p offsetOf_Engine_animating
    width               <- peekByteOff p offsetOf_Engine_width
    height              <- peekByteOff p offsetOf_Engine_height
    state               <- peekByteOff p offsetOf_Engine_state
    eglDisp             <- peekByteOff p offsetOf_Engine_display
    eglSurf             <- peekByteOff p offsetOf_Engine_surface
    eglCont             <- peekByteOff p offsetOf_Engine_context
    return $ AndroidEngine { engApp                 = app
                           , engSensorManager       = sensorManager
                           , engAccelerometerSensor = accelerometerSensor
                           , engSensorEventQueue    = sensorEventQueue
                           , engAnimating           = animating
                           , engWidth               = width
                           , engHeight              = height
                           , engState               = state
                           , engEglDisplay          = eglDisp
                           , engEglSurface          = eglSurf
                           , engEglContext          = eglCont }

-- struct android_app
foreign import primitive "const.sizeof(struct android_app)" sizeOf_AndroidApp :: Int
foreign import primitive "const.offsetof(struct android_app, userData)" offsetOf_AndroidApp_appUserData :: Int
foreign import primitive "const.offsetof(struct android_app, savedState)" offsetOf_AndroidApp_appSavedState :: Int
foreign import primitive "const.offsetof(struct android_app, savedStateSize)" offsetOf_AndroidApp_appSavedStateSize :: Int
foreign import primitive "const.offsetof(struct android_app, window)" offsetOf_AndroidApp_appWindow :: Int

newtype {-# CTYPE "ANativeWindow" #-} ANativeWindow = ANativeWindow ()

data AndroidApp = AndroidApp { appUserData       :: Ptr AndroidEngine
                             , appSavedState     :: Ptr SavedState
                             , appSavedStateSize :: CSize
                             , appWindow         :: Ptr ANativeWindow }
instance Storable AndroidApp where
  sizeOf    = const sizeOf_AndroidApp
  alignment = sizeOf
  poke p app = do
    pokeByteOff p offsetOf_AndroidApp_appUserData       $ appUserData app
    pokeByteOff p offsetOf_AndroidApp_appSavedState     $ appSavedState app
    pokeByteOff p offsetOf_AndroidApp_appSavedStateSize $ appSavedStateSize app
    pokeByteOff p offsetOf_AndroidApp_appWindow         $ appWindow app
  peek p = do
    userData       <- peekByteOff p offsetOf_AndroidApp_appUserData
    savedState     <- peekByteOff p offsetOf_AndroidApp_appSavedState
    savedStateSize <- peekByteOff p offsetOf_AndroidApp_appSavedStateSize
    window         <- peekByteOff p offsetOf_AndroidApp_appWindow
    return $ AndroidApp { appUserData       = userData
                        , appSavedState     = savedState
                        , appSavedStateSize = savedStateSize
                        , appWindow         = window }


newtype {-# CTYPE "AInputEvent" #-} AInputEvent = AInputEvent ()
foreign import primitive "const.AINPUT_EVENT_TYPE_MOTION" c_AINPUT_EVENT_TYPE_MOTION :: Int
foreign import ccall "c_extern.h AInputEvent_getType" c_AInputEvent_getType :: Ptr AInputEvent -> IO Int
foreign import ccall "c_extern.h AMotionEvent_getX" c_AMotionEvent_getX :: Ptr AInputEvent -> CSize -> IO Float
foreign import ccall "c_extern.h AMotionEvent_getY" c_AMotionEvent_getY :: Ptr AInputEvent -> CSize -> IO Float

engineHandleInput :: Ptr AndroidEngine -> Ptr AInputEvent -> IO Int
engineHandleInput eng event = do
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

foreign export ccall "engineHandleInput" engineHandleInput :: Ptr AndroidEngine -> Ptr AInputEvent -> IO Int


foreign import primitive "const.APP_CMD_SAVE_STATE" c_APP_CMD_SAVE_STATE :: Int
foreign import primitive "const.APP_CMD_INIT_WINDOW" c_APP_CMD_INIT_WINDOW :: Int
foreign import primitive "const.APP_CMD_TERM_WINDOW" c_APP_CMD_TERM_WINDOW :: Int
foreign import primitive "const.APP_CMD_GAINED_FOCUS" c_APP_CMD_GAINED_FOCUS :: Int
foreign import primitive "const.APP_CMD_LOST_FOCUS" c_APP_CMD_LOST_FOCUS :: Int

foreign import ccall "c_extern.h engine_init_display" c_engine_init_display :: Ptr AndroidEngine -> IO Int
foreign import ccall "c_extern.h engine_draw_frame" c_engine_draw_frame :: Ptr AndroidEngine -> IO ()
foreign import ccall "c_extern.h engine_term_display" c_engine_term_display :: Ptr AndroidEngine -> IO ()
foreign import ccall "c_extern.h ASensorEventQueue_enableSensor" c_ASensorEventQueue_enableSensor :: Ptr ASensorEventQueue -> Ptr ASensor -> IO Int
foreign import ccall "c_extern.h ASensorEventQueue_setEventRate" c_ASensorEventQueue_setEventRate :: Ptr ASensorEventQueue -> Ptr ASensor -> Int -> IO Int


foreign import ccall "c_extern.h ASensorEventQueue_disableSensor" c_ASensorEventQueue_disableSensor :: Ptr ASensorEventQueue -> Ptr ASensor -> IO Int

engineHandleCmd :: Ptr AndroidEngine -> Int -> IO ()
engineHandleCmd eng cmd
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
                                        c_engine_init_display eng
                                        c_engine_draw_frame eng
  | cmd == c_APP_CMD_TERM_WINDOW = c_engine_term_display eng
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
                                     c_engine_draw_frame eng
engineHandleCmd _ _ = return ()

foreign export ccall "engineHandleCmd" engineHandleCmd :: Ptr AndroidEngine -> Int -> IO ()


-- EGL

foreign import ccall "c_extern.h eglMakeCurrent" c_eglMakeCurrent :: EGLDisplay -> EGLSurface -> EGLSurface -> EGLContext -> IO Word32
foreign import ccall "c_extern.h eglDestroyContext" c_eglDestroyContext :: EGLDisplay -> EGLContext -> IO Word32
foreign import ccall "c_extern.h eglDestroySurface" c_eglDestroySurface :: EGLDisplay -> EGLSurface -> IO Word32
foreign import ccall "c_extern.h eglTerminate" c_eglTerminate :: EGLDisplay -> IO Word32

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

foreign export ccall "engineTermDisplay" engineTermDisplay :: Ptr AndroidEngine -> IO ()
