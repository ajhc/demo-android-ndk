{-# LANGUAGE ForeignFunctionInterface #-}
module EGL where
import Foreign.Ptr

type EGLint     = Int
type EGLBoolean = Word32
type EGLenum    = Word32
type EGLNativeDisplayType = Ptr () -- xxx Not same type on platforms
type EGLNativeWindowType  = Ptr ()
type EGLConfig  = Ptr () -- xxx unsafe
type EGLContext = Ptr ()
type EGLDisplay = Ptr ()
type EGLSurface = Ptr ()
type EGLClientBuffer = Ptr ()
type EGLattribs = [EGLint] -- Should use with withArray0

foreign import primitive "const.EGL_SURFACE_TYPE" c_EGL_SURFACE_TYPE :: EGLint
foreign import primitive "const.EGL_WINDOW_BIT" c_EGL_WINDOW_BIT :: EGLint
foreign import primitive "const.EGL_BLUE_SIZE" c_EGL_BLUE_SIZE :: EGLint
foreign import primitive "const.EGL_GREEN_SIZE" c_EGL_GREEN_SIZE :: EGLint
foreign import primitive "const.EGL_RED_SIZE" c_EGL_RED_SIZE :: EGLint
foreign import primitive "const.EGL_NONE" c_EGL_NONE :: EGLint
foreign import primitive "const.EGL_HEIGHT" c_EGL_HEIGHT :: EGLint
foreign import primitive "const.EGL_WIDTH" c_EGL_WIDTH :: EGLint
foreign import primitive "const.EGL_NATIVE_VISUAL_ID" c_EGL_NATIVE_VISUAL_ID :: EGLint

c_EGL_DEFAULT_DISPLAY = nullPtr
c_EGL_FALSE, c_EGL_TRUE :: EGLBoolean
c_EGL_FALSE = 0
c_EGL_TRUE = 1

foreign import ccall "c_extern.h eglGetDisplay" c_eglGetDisplay :: EGLNativeDisplayType -> IO EGLDisplay
foreign import ccall "c_extern.h eglInitialize" c_eglInitialize :: EGLDisplay -> Ptr EGLint -> Ptr EGLint -> IO EGLBoolean
foreign import ccall "c_extern.h eglChooseConfig" c_eglChooseConfig :: EGLDisplay -> Ptr EGLint -> Ptr EGLConfig -> EGLint -> Ptr EGLint -> IO EGLBoolean
foreign import ccall "c_extern.h eglGetConfigAttrib" c_eglGetConfigAttrib :: EGLDisplay -> EGLConfig -> EGLint -> Ptr EGLint -> IO EGLBoolean
foreign import ccall "c_extern.h eglCreateWindowSurface" c_eglCreateWindowSurface :: EGLDisplay -> EGLConfig -> EGLNativeWindowType -> Ptr EGLint -> IO EGLSurface
foreign import ccall "c_extern.h eglCreateContext" c_eglCreateContext :: EGLDisplay -> EGLConfig -> EGLContext -> Ptr EGLint -> IO EGLContext
foreign import ccall "c_extern.h eglQuerySurface" c_eglQuerySurface :: EGLDisplay -> EGLSurface -> EGLint -> Ptr EGLint -> IO EGLBoolean
foreign import ccall "c_extern.h eglSwapBuffers" c_eglSwapBuffers :: EGLDisplay -> EGLSurface -> IO Word32
foreign import ccall "c_extern.h eglMakeCurrent" c_eglMakeCurrent :: EGLDisplay -> EGLSurface -> EGLSurface -> EGLContext -> IO Word32
foreign import ccall "c_extern.h eglDestroyContext" c_eglDestroyContext :: EGLDisplay -> EGLContext -> IO Word32
foreign import ccall "c_extern.h eglDestroySurface" c_eglDestroySurface :: EGLDisplay -> EGLSurface -> IO Word32
foreign import ccall "c_extern.h eglTerminate" c_eglTerminate :: EGLDisplay -> IO Word32
