module Main where
import Graphics.UI.GLUT

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Hello World"
  displayCallback  $= display
  mainLoop

vert :: GLfloat -> GLfloat -> GLfloat -> Vertex3 GLfloat
vert = Vertex3

display :: DisplayCallback
display = do
  clear [ColorBuffer]

  renderPrimitive Lines $ do
    color $ Color3 1 1 (1 :: GLfloat)
    vertex $ vert 0 0 0
    vertex $ vert 0.5 0.5 0.3
    vertex $ vert 0 0 0
    vertex $ vert (-0.5) 0.5 0.3

  flush
