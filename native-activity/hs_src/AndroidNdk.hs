{-# LANGUAGE ForeignFunctionInterface #-}
module AndroidNdk where
import Foreign.Storable
import Foreign.C.Types
import Foreign.Ptr

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
newtype {-# CTYPE "EGLDisplay" #-}        EGLDisplay        = EGLDisplay ()
newtype {-# CTYPE "EGLSurface" #-}        EGLSurface        = EGLSurface ()
newtype {-# CTYPE "EGLContext" #-}        EGLContext        = EGLContext ()

instance Storable EGLDisplay where
  sizeOf    = const sizeOf_EGLDisplay
  alignment = sizeOf
instance Storable EGLSurface where
  sizeOf    = const sizeOf_EGLSurface
  alignment = sizeOf
instance Storable EGLContext where
  sizeOf    = const sizeOf_EGLContext
  alignment = sizeOf

data AndroidEngine = AndroidEngine { engApp                 :: Ptr AndroidApp
                                   , engSensorManager       :: Ptr ASensorManager
                                   , engAccelerometerSensor :: Ptr ASensor
                                   , engSensorEventQueue    :: Ptr ASensorEventQueue
                                   , engAnimating           :: Int
                                   , engDisplay             :: EGLDisplay
                                   , engSurface             :: EGLSurface
                                   , engContext             :: EGLContext
                                   , engWidth               :: Int
                                   , engHeight              :: Int
                                   , engState               :: SavedState }
instance Storable AndroidEngine where
  sizeOf    = const sizeOf_Engine
  alignment = sizeOf
  poke p eng = do
    pokeByteOff p offsetOf_Engine_app                 $ engApp eng
    pokeByteOff p offsetOf_Engine_sensorManager       $ engSensorManager eng
    pokeByteOff p offsetOf_Engine_accelerometerSensor $ engAccelerometerSensor eng
    pokeByteOff p offsetOf_Engine_sensorEventQueue    $ engSensorEventQueue eng
    pokeByteOff p offsetOf_Engine_animating           $ engAnimating eng
    pokeByteOff p offsetOf_Engine_display             $ engDisplay eng
    pokeByteOff p offsetOf_Engine_surface             $ engSurface eng
    pokeByteOff p offsetOf_Engine_context             $ engContext eng
    pokeByteOff p offsetOf_Engine_width               $ engWidth eng
    pokeByteOff p offsetOf_Engine_height              $ engHeight eng
    pokeByteOff p offsetOf_Engine_state               $ engState eng
  peek p = do
    app                 <- peekByteOff p offsetOf_Engine_app
    sensorManager       <- peekByteOff p offsetOf_Engine_sensorManager
    accelerometerSensor <- peekByteOff p offsetOf_Engine_accelerometerSensor
    sensorEventQueue    <- peekByteOff p offsetOf_Engine_sensorEventQueue
    animating           <- peekByteOff p offsetOf_Engine_animating
    display             <- peekByteOff p offsetOf_Engine_display
    surface             <- peekByteOff p offsetOf_Engine_surface
    context             <- peekByteOff p offsetOf_Engine_context
    width               <- peekByteOff p offsetOf_Engine_width
    height              <- peekByteOff p offsetOf_Engine_height
    state               <- peekByteOff p offsetOf_Engine_state
    return $ AndroidEngine { engApp                 = app
                           , engSensorManager       = sensorManager
                           , engAccelerometerSensor = accelerometerSensor
                           , engSensorEventQueue    = sensorEventQueue
                           , engAnimating           = animating
                           , engDisplay             = display
                           , engSurface             = surface
                           , engContext             = context
                           , engWidth               = width
                           , engHeight              = height
                           , engState               = state }

-- struct android_app
foreign import primitive "const.sizeof(struct android_app)" sizeOf_AndroidApp :: Int
foreign import primitive "const.offsetof(struct android_app, userData)" offsetOf_AndroidApp_appUserData :: Int

data AndroidApp = AndroidApp { appUserData       :: Ptr ()
                             , appSavedState     :: Ptr ()
                             , appSavedStateSize :: CSize
                             , appWindow         :: Ptr () }
instance Storable AndroidApp where
  sizeOf    = const sizeOf_AndroidApp
  alignment = sizeOf
  poke p app = do
    pokeByteOff p offsetOf_AndroidApp_appUserData $ appUserData app
  peek p = do
    peekByteOff p offsetOf_AndroidApp_appUserData
    -- return
