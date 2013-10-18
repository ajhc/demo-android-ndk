{-# LANGUAGE ForeignFunctionInterface #-}
module AndroidNdk.Storable where
import Foreign.Storable
import Foreign.Ptr
import Foreign.C.Types

import AndroidNdk.EGL

-- struct saved_state
foreign import primitive "const.sizeof(struct saved_state)" sizeOf_SavedState :: Int
foreign import primitive "const.offsetof(struct saved_state, angle)" offsetOf_SavedState_angle :: Int
foreign import primitive "const.offsetof(struct saved_state, x)" offsetOf_SavedState_x :: Int
foreign import primitive "const.offsetof(struct saved_state, y)" offsetOf_SavedState_y :: Int
foreign import primitive "const.offsetof(struct saved_state, dx)" offsetOf_SavedState_dx :: Int
foreign import primitive "const.offsetof(struct saved_state, dy)" offsetOf_SavedState_dy :: Int

data SavedState = SavedState { sStateAngle :: Float
                             , sStateX     :: Int
                             , sStateY     :: Int
                             , sStateDx    :: Int
                             , sStateDy    :: Int }
instance Storable SavedState where
  sizeOf    = const sizeOf_SavedState
  alignment = sizeOf
  poke p sstat = do
    pokeByteOff p offsetOf_SavedState_angle $ sStateAngle sstat
    pokeByteOff p offsetOf_SavedState_x     $ sStateX sstat
    pokeByteOff p offsetOf_SavedState_y     $ sStateY sstat
    pokeByteOff p offsetOf_SavedState_dx    $ sStateDx sstat
    pokeByteOff p offsetOf_SavedState_dy    $ sStateDy sstat
  peek p = do
    angle <- peekByteOff p offsetOf_SavedState_angle
    x     <- peekByteOff p offsetOf_SavedState_x
    y     <- peekByteOff p offsetOf_SavedState_y
    dx    <- peekByteOff p offsetOf_SavedState_dx
    dy    <- peekByteOff p offsetOf_SavedState_dy
    return $ SavedState { sStateAngle = angle, sStateX = x, sStateY = y, sStateDx = dx, sStateDy = dy }

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
newtype {-# CTYPE "ASensorEvent" #-}      ASensorEvent      = ASensorEvent ()
newtype {-# CTYPE "ASensorEventQueue" #-} ASensorEventQueue = ASensorEventQueue ()
newtype {-# CTYPE "ALooper" #-}           ALooper           = ALooper ()
type ALooper_callbackFunc = Ptr () -- xxx
c_EGL_NO_DISPLAY = nullPtr
c_EGL_NO_SURFACE = nullPtr
c_EGL_NO_CONTEXT = nullPtr
foreign import primitive "const.sizeof(ASensorEvent)" sizeOf_ASensorEvent :: Int
foreign import primitive "const.ASENSOR_TYPE_ACCELEROMETER" c_ASENSOR_TYPE_ACCELEROMETER :: Int
foreign import primitive "const.LOOPER_ID_USER" c_LOOPER_ID_USER :: Int

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
defaultAndroidEngine :: AndroidEngine
defaultAndroidEngine = AndroidEngine { engApp                 = nullPtr
                                     , engSensorManager       = nullPtr
                                     , engAccelerometerSensor = nullPtr
                                     , engSensorEventQueue    = nullPtr
                                     , engAnimating           = 0
                                     , engWidth               = 0
                                     , engHeight              = 0
                                     , engState               = SavedState { sStateAngle = 0
                                                                           , sStateX     = 0
                                                                           , sStateY     = 0
                                                                           , sStateDx    = 0
                                                                           , sStateDy    = 0 }
                                     , engEglDisplay          = nullPtr
                                     , engEglSurface          = nullPtr
                                     , engEglContext          = nullPtr }

-- struct android_app
foreign import primitive "const.sizeof(struct android_app)" sizeOf_AndroidApp :: Int
foreign import primitive "const.offsetof(struct android_app, userData)" offsetOf_AndroidApp_appUserData :: Int
foreign import primitive "const.offsetof(struct android_app, savedState)" offsetOf_AndroidApp_appSavedState :: Int
foreign import primitive "const.offsetof(struct android_app, savedStateSize)" offsetOf_AndroidApp_appSavedStateSize :: Int
foreign import primitive "const.offsetof(struct android_app, window)" offsetOf_AndroidApp_appWindow :: Int
foreign import primitive "const.offsetof(struct android_app, looper)" offsetOf_AndroidApp_appLooper :: Int
foreign import primitive "const.offsetof(struct android_app, destroyRequested)" offsetOf_AndroidApp_appDestroyRequested :: Int
foreign import primitive "const.offsetof(struct android_app, onAppCmd)" offsetOf_AndroidApp_appOnAppCmd :: Int
foreign import primitive "const.offsetof(struct android_app, onInputEvent)" offsetOf_AndroidApp_appOnInputEvent :: Int

newtype {-# CTYPE "ANativeWindow" #-} ANativeWindow = ANativeWindow ()

data AndroidApp = AndroidApp { appUserData       :: Ptr AndroidEngine
                             , appSavedState     :: Ptr SavedState
                             , appSavedStateSize :: CSize
                             , appWindow         :: Ptr ANativeWindow
                             , appLooper         :: Ptr ALooper
                             , appDestroyRequested :: Int
                             , appOnAppCmd       :: FunPtr (Ptr AndroidApp -> Int -> IO ())
                             , appOnInputEvent   :: FunPtr (Ptr AndroidApp -> Ptr AInputEvent -> IO Int) }
instance Storable AndroidApp where
  sizeOf    = const sizeOf_AndroidApp
  alignment = sizeOf
  poke p app = do
    pokeByteOff p offsetOf_AndroidApp_appUserData       $ appUserData app
    pokeByteOff p offsetOf_AndroidApp_appSavedState     $ appSavedState app
    pokeByteOff p offsetOf_AndroidApp_appSavedStateSize $ appSavedStateSize app
    pokeByteOff p offsetOf_AndroidApp_appWindow         $ appWindow app
    pokeByteOff p offsetOf_AndroidApp_appLooper         $ appLooper app
    pokeByteOff p offsetOf_AndroidApp_appDestroyRequested $ appDestroyRequested app
    pokeByteOff p offsetOf_AndroidApp_appOnAppCmd       $ appOnAppCmd app
    pokeByteOff p offsetOf_AndroidApp_appOnInputEvent   $ appOnInputEvent app
  peek p = do
    userData       <- peekByteOff p offsetOf_AndroidApp_appUserData
    savedState     <- peekByteOff p offsetOf_AndroidApp_appSavedState
    savedStateSize <- peekByteOff p offsetOf_AndroidApp_appSavedStateSize
    window         <- peekByteOff p offsetOf_AndroidApp_appWindow
    looper         <- peekByteOff p offsetOf_AndroidApp_appLooper
    destroy        <- peekByteOff p offsetOf_AndroidApp_appDestroyRequested
    onApp          <- peekByteOff p offsetOf_AndroidApp_appOnAppCmd
    onInput        <- peekByteOff p offsetOf_AndroidApp_appOnInputEvent
    return $ AndroidApp { appUserData       = userData
                        , appSavedState     = savedState
                        , appSavedStateSize = savedStateSize
                        , appWindow         = window
                        , appLooper         = looper
                        , appDestroyRequested = destroy
                        , appOnAppCmd       = onApp
                        , appOnInputEvent   = onInput }

-- struct android_poll_source
foreign import primitive "const.sizeof(struct android_poll_source)" sizeOf_AndroidPollSource :: Int
foreign import primitive "const.offsetof(struct android_poll_source, process)" offsetOf_AndroidPollSource_pollProcess :: Int
type PollProcessFunction = Ptr AndroidApp -> Ptr AndroidPollSource -> IO ()
foreign import ccall "dynamic" mkFun_AndroidPollSource_pollProcess :: FunPtr PollProcessFunction -> PollProcessFunction

data AndroidPollSource = AndroidPollSource {
  pollProcess :: FunPtr PollProcessFunction
  }
instance Storable AndroidPollSource where
  sizeOf    = const sizeOf_AndroidPollSource
  alignment = sizeOf
  poke p poll = pokeByteOff p offsetOf_AndroidPollSource_pollProcess $ pollProcess poll
  peek p = do
    process <- peekByteOff p offsetOf_AndroidPollSource_pollProcess
    return $ AndroidPollSource { pollProcess = process }

newtype {-# CTYPE "AInputEvent" #-} AInputEvent = AInputEvent ()
foreign import primitive "const.AINPUT_EVENT_TYPE_MOTION" c_AINPUT_EVENT_TYPE_MOTION :: Int
foreign import primitive "const.AMOTION_EVENT_ACTION_MASK" c_AMOTION_EVENT_ACTION_MASK :: Int
foreign import primitive "const.AMOTION_EVENT_ACTION_DOWN" c_AMOTION_EVENT_ACTION_DOWN :: Int
foreign import primitive "const.AMOTION_EVENT_ACTION_UP" c_AMOTION_EVENT_ACTION_UP :: Int
foreign import ccall "c_extern.h AInputEvent_getType" c_AInputEvent_getType :: Ptr AInputEvent -> IO Int
foreign import ccall "c_extern.h AMotionEvent_getX" c_AMotionEvent_getX :: Ptr AInputEvent -> CSize -> IO Float
foreign import ccall "c_extern.h AMotionEvent_getY" c_AMotionEvent_getY :: Ptr AInputEvent -> CSize -> IO Float
foreign import ccall "c_extern.h AKeyEvent_getAction" c_AKeyEvent_getAction :: Ptr AInputEvent -> IO Int
