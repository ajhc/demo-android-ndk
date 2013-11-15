{-# LANGUAGE ForeignFunctionInterface #-}
module AndroidNdk (module AndroidNdk, module AndroidNdk.Storable, module AndroidNdk.EGL, module AndroidNdk.OpenGLES) where
import Control.Monad
import Data.Maybe
import Foreign.Storable
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array

import AndroidNdk.EGL
import AndroidNdk.OpenGLES
import AndroidNdk.Storable

type CSSize = Int
foreign import primitive "const.APP_CMD_INPUT_CHANGED" c_APP_CMD_INPUT_CHANGED :: Int
foreign import primitive "const.APP_CMD_INIT_WINDOW" c_APP_CMD_INIT_WINDOW :: Int
foreign import primitive "const.APP_CMD_TERM_WINDOW" c_APP_CMD_TERM_WINDOW :: Int
foreign import primitive "const.APP_CMD_WINDOW_RESIZED" c_APP_CMD_WINDOW_RESIZED :: Int
foreign import primitive "const.APP_CMD_WINDOW_REDRAW_NEEDED" c_APP_CMD_WINDOW_REDRAW_NEEDED :: Int
foreign import primitive "const.APP_CMD_CONTENT_RECT_CHANGED" c_APP_CMD_CONTENT_RECT_CHANGED :: Int
foreign import primitive "const.APP_CMD_GAINED_FOCUS" c_APP_CMD_GAINED_FOCUS :: Int
foreign import primitive "const.APP_CMD_LOST_FOCUS" c_APP_CMD_LOST_FOCUS :: Int
foreign import primitive "const.APP_CMD_CONFIG_CHANGED" c_APP_CMD_CONFIG_CHANGED :: Int
foreign import primitive "const.APP_CMD_LOW_MEMORY" c_APP_CMD_LOW_MEMORY :: Int
foreign import primitive "const.APP_CMD_START" c_APP_CMD_START :: Int
foreign import primitive "const.APP_CMD_RESUME" c_APP_CMD_RESUME :: Int
foreign import primitive "const.APP_CMD_SAVE_STATE" c_APP_CMD_SAVE_STATE :: Int
foreign import primitive "const.APP_CMD_PAUSE" c_APP_CMD_PAUSE :: Int
foreign import primitive "const.APP_CMD_STOP" c_APP_CMD_STOP :: Int
foreign import primitive "const.APP_CMD_DESTROY" c_APP_CMD_DESTROY :: Int
data AAppCmd = AAppCmdInputChanged | AAppCmdInitWindow | AAppCmdTermWindow | AAppCmdWindowResized | AAppCmdWindowRedrawNeeded | AAppCmdContentRectChanged | AAppCmdGainedFocus | AAppCmdLostFocus | AAppCmdConfigChanged | AAppCmdLowMemory | AAppCmdStart | AAppCmdResume | AAppCmdSaveState | AAppCmdPause | AAppCmdStop | AAppCmdDestory | AAppCmdFail
appCmd :: Int -> AAppCmd
appCmd c | c == c_APP_CMD_INPUT_CHANGED = AAppCmdInputChanged
         | c == c_APP_CMD_INIT_WINDOW = AAppCmdInitWindow
         | c == c_APP_CMD_TERM_WINDOW = AAppCmdTermWindow
         | c == c_APP_CMD_WINDOW_RESIZED = AAppCmdWindowResized
         | c == c_APP_CMD_WINDOW_REDRAW_NEEDED = AAppCmdWindowRedrawNeeded
         | c == c_APP_CMD_CONTENT_RECT_CHANGED = AAppCmdContentRectChanged
         | c == c_APP_CMD_GAINED_FOCUS = AAppCmdGainedFocus
         | c == c_APP_CMD_LOST_FOCUS = AAppCmdLostFocus
         | c == c_APP_CMD_CONFIG_CHANGED = AAppCmdConfigChanged
         | c == c_APP_CMD_LOW_MEMORY = AAppCmdLowMemory
         | c == c_APP_CMD_START = AAppCmdStart
         | c == c_APP_CMD_RESUME = AAppCmdResume
         | c == c_APP_CMD_SAVE_STATE = AAppCmdSaveState
         | c == c_APP_CMD_PAUSE = AAppCmdPause
         | c == c_APP_CMD_STOP = AAppCmdStop
         | c == c_APP_CMD_DESTROY = AAppCmdDestory
         | otherwise = AAppCmdFail

foreign import ccall "c_extern.h ASensorEventQueue_enableSensor" c_ASensorEventQueue_enableSensor :: Ptr ASensorEventQueue -> Ptr ASensor -> IO Int
foreign import ccall "c_extern.h ASensorEventQueue_setEventRate" c_ASensorEventQueue_setEventRate :: Ptr ASensorEventQueue -> Ptr ASensor -> Int -> IO Int
foreign import ccall "c_extern.h ANativeWindow_setBuffersGeometry" c_ANativeWindow_setBuffersGeometry :: Ptr ANativeWindow -> Int -> Int -> Int -> IO Int
foreign import ccall "c_extern.h ASensorEventQueue_disableSensor" c_ASensorEventQueue_disableSensor :: Ptr ASensorEventQueue -> Ptr ASensor -> IO Int
foreign import ccall "c_extern.h ASensorManager_getInstance" c_ASensorManager_getInstance :: IO (Ptr ASensorManager)
foreign import ccall "c_extern.h ASensorManager_getDefaultSensor" c_ASensorManager_getDefaultSensor :: Ptr ASensorManager -> Int -> IO (Ptr ASensor)
foreign import ccall "c_extern.h ASensorManager_createEventQueue" c_ASensorManager_createEventQueue :: Ptr ASensorManager -> Ptr ALooper -> Int -> ALooper_callbackFunc -> Ptr () -> IO (Ptr ASensorEventQueue)
foreign import ccall "c_extern.h ALooper_pollAll" c_ALooper_pollAll :: Int -> Ptr Int -> Ptr Int -> Ptr (Ptr ()) -> IO Int
foreign import ccall "c_extern.h ASensorEventQueue_getEvents" c_ASensorEventQueue_getEvents :: Ptr ASensorEventQueue -> Ptr ASensorEvent -> CSize -> IO CSSize

type FuncHandleInput = Ptr AndroidApp -> Ptr AInputEvent -> IO Int
type FuncHandleCmd = Ptr AndroidApp -> Int -> IO ()

data AndroidNdkActs =
  AndroidNdkActs { drawFrame :: AndroidEngine -> IO ()
                 , initDisplay :: (GLint, GLint) -> IO ()
                 , handleInput :: AndroidEngine -> AInputEventType -> AMotionEventAction -> (Float, Float) -> IO (Maybe AndroidEngine)
                 , handleCmd :: (AndroidApp, AndroidEngine) -> AAppCmd -> IO (Maybe AndroidApp, Maybe AndroidEngine) }

while :: IO Bool -> IO ()
while f = do r <- f
             when r $ while f

androidMainHs :: AndroidNdkActs -> FunPtr FuncHandleInput -> FunPtr FuncHandleCmd -> Ptr AndroidApp -> IO ()
androidMainHs acts funI funC app = do
  eng <- malloc
  poke eng defaultAndroidEngine
  apphs <- peek app
  let apphs' = apphs { appUserData = eng, appOnAppCmd = funC, appOnInputEvent = funI }
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
  -- loop waiting for stuff to do.
  -- Read all pending events.
  events <- malloc
  source <- malloc
  while $ pollEvents acts app eng events source


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
      enghs'' <- peek eng
      if appDestroyRequested apphs /= 0 then engineTermDisplay enghs'' >> return False -- exit
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
engineTermDisplay :: AndroidEngine -> IO AndroidEngine
engineTermDisplay enghs = do
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

handleInputHs :: AndroidNdkActs -> Ptr AndroidApp -> Ptr AInputEvent -> IO Int
handleInputHs acts app event = do
  apphs <- peek app
  let eng = appUserData apphs
  enghs <- peek eng
  eType <- c_AInputEvent_getType event
  eX <- c_AMotionEvent_getX event 0
  eY <- c_AMotionEvent_getY event 0
  eAct <- c_AKeyEvent_getAction event
  r <- handleInput acts enghs (inputEventType eType) (motionEventAction eAct) (eX, eY)
  let go Nothing = return 0
      go (Just enghs') = poke eng enghs' >> return 1
  go r

handleCmdHs :: AndroidNdkActs -> Ptr AndroidApp -> Int -> IO ()
handleCmdHs acts app cmd = do
  apphs <- peek app
  let eng = appUserData apphs
  enghs <- peek eng
  (ra, re) <- handleCmd acts (apphs, enghs) (appCmd cmd)
  let pokeA Nothing = return ()
      pokeA (Just apphs') = poke app apphs'
      pokeE Nothing = return ()
      pokeE (Just enghs') = poke eng enghs'
  pokeA ra
  pokeE re

initDisplayHs :: AndroidNdkActs -> AndroidEngine -> IO (Maybe AndroidEngine)
initDisplayHs acts enghs = do
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
        initDisplay acts (w, h)
        let stat = engState enghs
        return . Just $ enghs { engEglDisplay = disp
                              , engEglContext = cont
                              , engEglSurface = surf
                              , engWidth      = w
                              , engHeight     = h
                              , engState      = stat { sStateAngle = 0 } }
