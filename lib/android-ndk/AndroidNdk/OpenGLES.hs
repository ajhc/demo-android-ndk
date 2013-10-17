{-# LANGUAGE ForeignFunctionInterface #-}
module AndroidNdk.OpenGLES where
import Data.Word
import Foreign.Ptr

type GLenum = Word32
type GLbitfield = Word32
type GLint = Int
type GLsizei = Int
type GLfloat = Float

foreign import primitive "const.GL_PERSPECTIVE_CORRECTION_HINT" c_GL_PERSPECTIVE_CORRECTION_HINT :: GLenum
foreign import primitive "const.GL_FASTEST" c_GL_FASTEST :: GLenum
foreign import primitive "const.GL_CULL_FACE" c_GL_CULL_FACE :: GLenum
foreign import primitive "const.GL_SMOOTH" c_GL_SMOOTH :: GLenum
foreign import primitive "const.GL_DEPTH_TEST" c_GL_DEPTH_TEST :: GLenum
foreign import primitive "const.GL_PROJECTION" c_GL_PROJECTION :: GLenum
foreign import primitive "const.GL_MODELVIEW" c_GL_MODELVIEW :: GLenum
foreign import primitive "const.GL_VERTEX_ARRAY" c_GL_VERTEX_ARRAY :: GLenum
foreign import primitive "const.GL_COLOR_ARRAY" c_GL_COLOR_ARRAY :: GLenum
foreign import primitive "const.GL_FLOAT" c_GL_FLOAT :: GLenum
foreign import primitive "const.GL_TRIANGLES" c_GL_TRIANGLES :: GLenum
foreign import primitive "const.GL_COLOR_BUFFER_BIT" c_GL_COLOR_BUFFER_BIT :: Word32
foreign import primitive "const.GL_DEPTH_BUFFER_BIT" c_GL_DEPTH_BUFFER_BIT :: Word32

foreign import ccall "GLES/gl.h glHint" c_glHint :: GLenum -> GLenum -> IO ()
foreign import ccall "GLES/gl.h glEnable" c_glEnable:: GLenum -> IO ()
foreign import ccall "GLES/gl.h glShadeModel" c_glShadeModel :: GLenum -> IO ()
foreign import ccall "GLES/gl.h glDisable" c_glDisable :: GLenum -> IO ()
foreign import ccall "GLES/gl.h glClearColor" c_glClearColor :: Float -> Float -> Float -> Float -> IO ()
foreign import ccall "GLES/gl.h glClear" c_glClear :: GLbitfield -> IO ()
foreign import ccall "GLES/gl.h glViewport" c_glViewport :: GLint -> GLint -> GLsizei -> GLsizei -> IO ()
foreign import ccall "GLES/gl.h glMatrixMode" c_glMatrixMode :: GLenum -> IO ()
foreign import ccall "GLES/gl.h glLoadIdentity" c_glLoadIdentity :: IO ()
foreign import ccall "GLES/gl.h glFrustumf" c_glFrustumf :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
foreign import ccall "GLES/gl.h glScalef" c_glScalef :: GLfloat -> GLfloat -> GLfloat -> IO ()
foreign import ccall "GLES/gl.h glEnableClientState" c_glEnableClientState :: GLenum -> IO ()
foreign import ccall "GLES/gl.h glVertexPointer" c_glVertexPointer :: GLint -> GLenum -> GLsizei -> Ptr GLfloat -> IO ()
foreign import ccall "GLES/gl.h glColorPointer" c_glColorPointer :: GLint -> GLenum -> GLsizei -> Ptr GLfloat -> IO ()
foreign import ccall "GLES/gl.h glRotatef" c_glRotatef ::GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
foreign import ccall "GLES/gl.h glDrawArrays" c_glDrawArrays :: GLenum -> GLint -> GLsizei -> IO ()
foreign import ccall "GLES/gl.h glDisableClientState" c_glDisableClientState :: GLenum -> IO ()
