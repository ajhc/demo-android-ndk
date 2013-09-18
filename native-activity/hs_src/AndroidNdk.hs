{-# LANGUAGE ForeignFunctionInterface #-}
module AndroidNdk where
import Foreign.Storable
import Foreign.C.Types
import Foreign.Ptr

-- struct saved_state
foreign import primitive "sizeOf.struct saved_state" sizeOf_SavedState :: Int
foreign import primitive "alignmentOf.struct saved_state" alignmentOf_SavedState :: Int
foreign import primitive "const.offsetof(struct saved_state, angle)" offsetOf_SavedState_sStateAngle :: Int
foreign import primitive "const.offsetof(struct saved_state, x)" offsetOf_SavedState_sStateX :: Int
foreign import primitive "const.offsetof(struct saved_state, y)" offsetOf_SavedState_sStateY :: Int

data SavedState = SavedState { sStateAngle :: Float
                             , sStateX     :: Int
                             , sStateY     :: Int }
instance Storable SavedState where
  sizeOf    = const sizeOf_SavedState
  alignment = const alignmentOf_SavedState
  poke p sstat = do
    pokeByteOff p offsetOf_SavedState_sStateAngle $ sStateAngle sstat
    pokeByteOff p offsetOf_SavedState_sStateX $ sStateX sstat
    pokeByteOff p offsetOf_SavedState_sStateY $ sStateY sstat
  peek p = do
    angle <- peekByteOff p offsetOf_SavedState_sStateAngle
    x <- peekByteOff p offsetOf_SavedState_sStateX
    y <- peekByteOff p offsetOf_SavedState_sStateY
    return $ SavedState { sStateAngle = angle, sStateX = x, sStateY = y }

-- struct engine
foreign import primitive "sizeOf.struct engine" sizeOf_Engine :: Int

-- struct android_app
foreign import primitive "sizeOf.struct android_app" sizeOf_AndroidApp :: Int
foreign import primitive "alignmentOf.struct android_app" alignmentOf_AndroidApp :: Int
foreign import primitive "const.offsetof(struct android_app, userData)" offsetOf_AndroidApp_appUserData :: Int

data AndroidApp = AndroidApp { appUserData       :: Ptr ()
                             , appSavedState     :: Ptr ()
                             , appSavedStateSize :: CSize
                             , appWindow         :: Ptr () }
instance Storable AndroidApp where
  sizeOf    = const sizeOf_AndroidApp
  alignment = const alignmentOf_AndroidApp
  poke p app = do
    pokeByteOff p offsetOf_AndroidApp_appUserData $ appUserData app
  peek p = do
    peekByteOff p offsetOf_AndroidApp_appUserData
    -- return
