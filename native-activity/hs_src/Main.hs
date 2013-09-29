{-# LANGUAGE ForeignFunctionInterface #-}
import Control.Monad
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import AndroidNdk

-- Dummy
main :: IO ()
main = return ()

-- True main
foreign export ccall "androidMain" androidMain :: Ptr AndroidApp -> IO ()
androidMain :: Ptr AndroidApp -> IO ()
androidMain app = do
  eng <- malloc
  poke eng defaultAndroidEngine
  apphs <- peek app
  let apphs' = apphs { appUserData = eng, appOnAppCmd = p_engineHandleCmd , appOnInputEvent = p_engineHandleInput }
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
  while $ pollEvents app eng events source


while :: IO Bool -> IO ()
while f = do r <- f
             when r $ while f

-- Read all pending events.
-- If not animating, we will block forever waiting for events.
-- If animating, we loop until all events are read, then continue
-- to draw the next frame of animation.
pollEvents :: Ptr AndroidApp -> Ptr AndroidEngine -> Ptr Int -> Ptr (Ptr AndroidPollSource)  -> IO Bool
pollEvents app eng events source = do
  r <- pollEventWhile app eng events source
  if r == False then return False -- exit
    else do
      enghs' <- peek eng
      when (engAnimating enghs' /= 0) $ do
        -- Done with events; draw next animation frame.
        let stat      = engState $ enghs'
            newAngle  = sStateAngle stat + 0.01
            newAngle' = if newAngle > 1 then 0 else newAngle
        poke eng $ enghs' { engState = stat { sStateAngle = newAngle' } }
        -- Drawing is throttled to the screen update rate, so there is no need to do timing here.
        engineDrawFrame eng
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
