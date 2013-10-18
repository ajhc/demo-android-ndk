{-# LANGUAGE ForeignFunctionInterface #-}
import Control.Monad
import Data.Bits
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array

import AndroidNdk

import CubeVertices

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
            enghs''   = enghs' { engState = stat { sStateAngle = newAngle' } }
        poke eng enghs''
        -- Drawing is throttled to the screen update rate, so there is no need to do timing here.
        engineDrawFrame enghs''
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
                ox = fromIntegral $ sStateX stat
                oy = fromIntegral $ sStateY stat
            x <- c_AMotionEvent_getX event 0
            y <- c_AMotionEvent_getY event 0
            act <- c_AKeyEvent_getAction event
            let act' = act .&. c_AMOTION_EVENT_ACTION_MASK
                x' = if act' == c_AMOTION_EVENT_ACTION_DOWN then x else x - ox
                y' = if act' == c_AMOTION_EVENT_ACTION_DOWN then y else y - oy
                enghs' = enghs { engAnimating = 1
                               , engState = stat { sStateX = truncate x',  sStateY = truncate y' } }
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
                                        peek eng >>= engineDrawFrame
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
                                     engineDrawFrame enghs'
                                     poke eng enghs'
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
engineDrawFrame :: AndroidEngine -> IO ()
engineDrawFrame = go
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
            c_glClear $ c_GL_COLOR_BUFFER_BIT .|. c_GL_DEPTH_BUFFER_BIT
            withArray vertices $ \vp -> withArray colors $ \cp -> do -- xxx heavy
              c_glEnableClientState c_GL_VERTEX_ARRAY
              c_glEnableClientState c_GL_COLOR_ARRAY
              c_glVertexPointer 3 c_GL_FLOAT 0 vp
              c_glColorPointer 4 c_GL_FLOAT 0 cp
              c_glRotatef ((sqrt (x ** 2 + y ** 2)) / 500.0) (- y) x 0.0
              c_glDrawArrays c_GL_TRIANGLES 0 36
              c_glDisableClientState c_GL_VERTEX_ARRAY
              c_glDisableClientState c_GL_COLOR_ARRAY
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
