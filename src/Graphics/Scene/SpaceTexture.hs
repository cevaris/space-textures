import Control.Monad ( when )
import Control.Applicative

import Graphics.UI.GLUT
import Graphics.GLUtil
--import Codec.Picture
--import qualified Graphics.Rendering.OpenGL as GL
--import qualified Graphics.GLUtil as GLU
--import qualified Codec.Picture as Pic

import Data.State
import Binding.Input
import Graphics.Util.GLUtils
import Graphics.Scene.Idle
import Graphics.Object.Cube
import Graphics.Object.Star
import Graphics.Object.Grid
import Graphics.Object.StarCluster
import Graphics.Object.Fighter
import Graphics.Object.Pyramid
import Graphics.Object.Station
import Graphics.Object.Sphere

----------------------------------------------------------------------------------------------------------------
-- Timer 
timerFrequencyMillis :: Timeout
timerFrequencyMillis = 20

timer :: State -> TimerCallback
timer state = do
  addTimerCallback timerFrequencyMillis (timer state)




visible :: State -> Visibility -> IO ()
visible state Visible    = idleCallback $= Just (idle state)
visible _     NotVisible = idleCallback $= Nothing
  
reshape :: State -> ReshapeCallback
reshape state s@(Size width height) = do

  if height > 0
    then asp state $~! (\x -> (fromIntegral width)/(fromIntegral height))
    else asp state $~! (\x -> 1)

  viewport   $= (Position 0 0, s)

  matrixMode $= Projection
  loadIdentity

  fov <- get (fov state)
  asp <- get (asp state)
  dim <- get (dim state)
  setPerspective fov asp (dim/16) (dim*16)

  matrixMode $= Modelview 0
  loadIdentity


----------------------------------------------------------------------------------------------------------------
-- Debug info
updateInfo :: State -> IO ()
updateInfo state = do 
  frames state $~! (+1)
  t0' <- get (t0 state)
  t <- get elapsedTime
  when (t - t0' >= 1000) $ do
    f <- get (frames state)
    ph <- get (ph' state)
    th <- get (th' state)
    gr <- get (gr' state)
    zh <- get (zh' state)
    asp <- get (asp state)
    fov <- get (fov state)
    dim <- get (dim state)
    
    spec <- get (spec' state)
    amb <- get (amb' state)
    diff <- get (diff' state)
    shine <- get (shine' state)
    emiss <- get (emiss' state)
    lightStatus <- get (light' state)
    shadStatus <- get (smooth' state)

    let seconds = fromIntegral (t - t0') / 1000 :: GLfloat
        fps = fromIntegral f / seconds
        result = ("[ph " ++ round2 ph ++ "] [th " ++ round2 th ++ "] [zh " ++ round2 zh ++ "] [zoom " ++ show dim ++ "] [lightStatus " ++ show lightStatus ++  "] [shading " ++ show shadStatus ++  "] ",
                  "[specular " ++ show spec ++  "] [ambience " ++ show amb ++  "] [diffuse " ++ show diff ++  "] [shininess " ++ show shine ++  "] [emission " ++ show emiss ++  "] ")
    info state $= result
    t0 state $= t
    frames state $= 0


draw :: State -> IO ()
draw state = do

  clear [ ColorBuffer, DepthBuffer ]

  ph <- get (ph' state)
  th <- get (th' state)
  gr <- get (gr' state)
  zh  <- get (zh' state)
  dim <- get (dim state)
  info <- get (info state)
  
  ylight <- get (ylight' state)
  rlight <- get (rlight' state)
  ambience <- get (amb' state)
  diffusion <- get (diff' state)
  specularizion <- get (spec' state)
  emission <- get (emiss' state)
  
  shineVal   <- get (shine' state)
  let shine = shineVal^2


  lightStatus <- get (light' state)
  shadeStatus <- get (smooth' state)


  loadIdentity


  let ex = (-2)*dim*sin(toDeg(th))*cos(toDeg(ph))
      ey =    2*dim               *sin(toDeg(ph))
      ez =    2*dim*cos(toDeg(th))*cos(toDeg(ph))
  setLookAt (ex,ey,ez) (0,0,0) (0,cos(toDeg(ph)),0)


  ------------------------------------
  --shadeModel $= Smooth

  let ambs     = (Point4 (0.01*ambience) (0.01*ambience) (0.01*ambience) 1.0)
      diffs    = (Point4 (0.01*diffusion) (0.01*diffusion) (0.01*diffusion) 1.0)
      specs    = (Point4 (0.01*specularizion) (0.01*specularizion) (0.01*specularizion) 1.0)
      loc3     = (rlight*glCos(zh), ylight, rlight*glSin(zh))
      loc4     = (Point4 (rlight*glCos(zh)) ylight (rlight*glSin(zh)) 1.0)
      yellow   = (Point4 1.0 1.0 0.0 1.0)
      white    = (Point4 1 1 1 1)
      black    = (Point4 0 0 0 1)
      emiss    = (Point4 0.0 0.0 (0.01*emission) 1.0)
      darkGray = (Point4 (50/255) (50/255) (50/255) 0)
      snowGray = (Point4 (138/255) (138/255) (138/255) 0)


  if lightStatus
    then do
      normalize $= Enabled
      lighting $= Enabled
      lightModelLocalViewer $= Enabled
      colorMaterial $= Just (FrontAndBack, AmbientAndDiffuse)
      light (Light 0) $= Enabled

      shadeModel $= shadeStatus

      ambient4f ambs
      specular4f specs
      diffuse4f diffs
      position4f loc4
    else do
      lighting $= Disabled


    
  drawFighter state $ ObjectAttributes {  
    scaleSize  = Just 2,
    paint      = Just darkGray,
    location   = Just (0, 1, 1),
    noseVector = Just (0, (-1), 1),
    upVector   = Just (0,1,0),
    ambience4  = Nothing,
    diffuse4   = Nothing,
    specular4  = Just white,
    emission4  = Just emiss,
    shininess  = Just shine
  }

  drawFighter state $ ObjectAttributes {  
    scaleSize  = Just 2,
    paint      = Just darkGray,
    location   = Just (0, (-1), 0),
    noseVector = Just (0, 0, 1),
    upVector   = Just (0,1,0),
    ambience4  = Nothing,
    diffuse4   = Nothing,
    specular4  = Just white,
    emission4  = Just emiss,
    shininess  = Just shine
  }

  drawSphere state $ ObjectAttributes {  
    scaleSize  = (Just 0.25),
    paint      = Just $ (Point4 255 255 0 0),
    location   = (Just loc3),
    noseVector = Nothing,
    upVector   = Nothing,
    ambience4  = Nothing,
    diffuse4   = Nothing,
    specular4  = Nothing,
    emission4  = Nothing,
    shininess  = Just shine
  }

  drawCube state $ ObjectAttributes {  
    scaleSize  = (Just 0.5),
    paint      = Just $ (Point4 1 0 0 0),
    location   = (Just ((-1.5), 0, 0)),
    noseVector = Nothing,
    upVector   = Nothing,
    ambience4  = Nothing,
    diffuse4   = Nothing,
    specular4  = Just yellow,
    emission4  = Just emiss,
    shininess  = Just shine
  }
  
  drawStation state (fToGL gr) 0.3 (1.5,0,0)

  drawStation state (fToGL (gr*1.2)) 0.3 (1,1,1)

  drawSphere state $ ObjectAttributes {  
    scaleSize  = (Just 0.5),
    paint      = Just $ (Point4 1 1 1 1),
    location   = (Just (0, 0, 0)),
    noseVector = Nothing,
    upVector   = Nothing,
    ambience4  = Nothing,
    diffuse4   = Nothing,
    specular4  = Just yellow,
    emission4  = Just emiss,
    shininess  = Just shine
  }

  

  lighting $= Disabled
  ------------------------------------
  
  drawGrid 5


  
  --drawStar 0.5 (0, 1.5, 0)

  --drawStarCluster state (5, 1, 3)
  --drawStarCluster state (5, 5, 1)
  --drawStarCluster state (1, 5, 5)

  --drawStation 0.0 0.5 (1,0,0) (0,1,0)

  
  --drawFighter 0.7 (1, 0.7, 0) (1,0,0) (0,1,0)
  --drawFighter 0.5 (0,1,1) (1,1,1) (0,1,0)

  preservingMatrix $ do
    glWindowPos 5 30
    renderString Helvetica18 $ (fst info)
    glWindowPos 5 5
    renderString Helvetica18 $ (snd info)

  swapBuffers
  updateInfo state
  reportErrors


myInit :: [String] -> State -> IO ()
myInit args state = do
  clearColor $= Color4 (100/255) (100/255) (100/255) 0
  depthFunc $= Just Less  

  texture Texture2D $= Enabled


main :: IO ()
main = do
    initialWindowSize $= Size 800 800
    (_progName, args) <- getArgsAndInitialize
    initialDisplayMode $= [ RGBMode, WithDepthBuffer, DoubleBuffered ]
    
    --initialWindowPosition $= Position 500 500
    _window <- createWindow "Space Scene Projection - Adam Cardenas"

    state <- makeState
    myInit args state

    displayCallback $= draw state
    reshapeCallback $= Just (reshape state)
    
    keyboardMouseCallback $= Just (keyboard state)
    --keyboardUpCallback $= Just (keyboard state)
    visibilityCallback $= Just (visible state)

    mainLoop
  


