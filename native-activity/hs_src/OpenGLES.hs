{-# LANGUAGE ForeignFunctionInterface #-}
module OpenGLES where
import Data.Word

type GLenum = Word32
type GLbitfield = Word32
foreign import ccall "c_extern.h glHint" c_glHint :: GLenum -> GLenum -> IO ()
foreign import ccall "c_extern.h glEnable" c_glEnable:: GLenum -> IO ()
foreign import ccall "c_extern.h glShadeModel" c_glShadeModel :: GLenum -> IO ()
foreign import ccall "c_extern.h glDisable" c_glDisable :: GLenum -> IO ()
foreign import ccall "c_extern.h glClearColor" c_glClearColor :: Float -> Float -> Float -> Float -> IO ()
foreign import ccall "c_extern.h glClear" c_glClear :: GLbitfield -> IO ()
foreign import primitive "const.GL_PERSPECTIVE_CORRECTION_HINT" c_GL_PERSPECTIVE_CORRECTION_HINT :: GLenum
foreign import primitive "const.GL_FASTEST" c_GL_FASTEST :: GLenum
foreign import primitive "const.GL_CULL_FACE" c_GL_CULL_FACE :: GLenum
foreign import primitive "const.GL_SMOOTH" c_GL_SMOOTH :: GLenum
foreign import primitive "const.GL_DEPTH_TEST" c_GL_DEPTH_TEST :: GLenum
foreign import primitive "const.GL_COLOR_BUFFER_BIT" c_GL_COLOR_BUFFER_BIT :: Word32
