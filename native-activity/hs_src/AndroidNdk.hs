{-# LANGUAGE ForeignFunctionInterface #-}
module AndroidNdk where
import Foreign.Storable
import Foreign.C.Types
import Foreign.Ptr

foreign import primitive "sizeOf.struct android_app" sizeOfAndroidApp :: Int
foreign import primitive "alignmentOf.struct android_app" alignmentOfAndroidApp :: Int
foreign import primitive "const.offsetof(struct android_app, userData)" offsetOfAndroidAppUserData :: Int

data AndroidApp = AndroidApp { appUserData       :: Ptr ()
                             , appSavedState     :: Ptr ()
                             , appSavedStateSize :: CSize
                             , appWindow         :: Ptr () }
instance Storable AndroidApp where
  sizeOf    = const sizeOfAndroidApp
  alignment = const alignmentOfAndroidApp
  poke p app = do
    pokeByteOff p offsetOfAndroidAppUserData $ appUserData app
  peek p = do
    peekByteOff p offsetOfAndroidAppUserData
