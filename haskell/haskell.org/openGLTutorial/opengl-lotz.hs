module Main where
import Data.IORef
import Graphics.UI.GLUT

type Point = (GLfloat, GLfloat, GLfloat)
type Line  = [Point]

-- 初期値
start = (1.0, 1.0, 1.0)

-- ローレンツ方程式
lorenz :: Point -> Point
lorenz = \(x, y, z) ->
    let dt = 0.01
        p  = 10.0
        r  = 28.0
        b  = 8.0/3.0
    in  ( (\x' -> x + x' * dt) $ p * (y - x)
        , (\y' -> y + y' * dt) $ x * (r - z) - y
        , (\z' -> z + z' * dt) $ x * y - b * z
        )

main :: IO ()
main = do
    -- OpenGLの初期化
    (_progName, _args) <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]
    _window <- createWindow "Lorenz Attractor"
    -- 共有パラメータの設定
    rot <- newIORef 0
    sc  <- newIORef 0.02
    orbit <- newIORef [start]
    -- コールバックの設定
    keyboardMouseCallback $= Just (keyboardMouse rot sc)
    idleCallback $= Just (idle orbit)
    displayCallback $= display orbit rot sc
    -- 実行
    mainLoop

idle :: IORef Line -> IdleCallback
idle orbit = do
    ps <- get orbit
    orbit $= (lorenz (head ps) : take 10000 ps)
    postRedisplay Nothing

display :: IORef Line -> IORef GLfloat -> IORef GLfloat -> DisplayCallback
display orbit rot sc = do 
    -- バッファをクリア
    clear [ColorBuffer]
    -- 共有パラメータの取得
    r <- get rot
    s <- get sc
    ps <- get orbit
    -- 変換行列の初期化
    loadIdentity
    preservingMatrix $ do
        scale s s s
        rotate r $ Vector3 0 1 0
        -- 軌道の描画
        let mkVertex (x, y, z) = vertex $ Vertex3 x y z
        renderPrimitive LineStrip $ mapM_ mkVertex ps
    -- 表示
    swapBuffers

keyboardMouse :: IORef GLfloat -> IORef GLfloat -> KeyboardMouseCallback
keyboardMouse rot sc _key _state _ _ = do
    case _key of
        Char 'w' -> sc  $~! (*1.1)
        Char 'a' -> rot $~! (+5)
        Char 's' -> sc  $~! (*0.9)
        Char 'd' -> rot $~! (subtract 5)
        _        -> return ()
