module Data.State where
import Data.IORef ( IORef, newIORef )

import Graphics.UI.GLUT
import Graphics.GLUtil

import Graphics.Util.Textures

----------------------------------------------------------------------------------------------------------------
-- Global State
data ChangeDirection = Increase | Decrease deriving (Show)

data ProjectionView = PerspectiveView | OrthogonalView | FirstPersonView deriving (Show, Eq)

data Direction = UpDirection | DownDirection | LeftDirection | RightDirection deriving (Show, Eq)

data State = State {
  frames  :: IORef Int,
  t0      :: IORef Int,
  ph'     :: IORef Float,
  th'     :: IORef Float,
  gr'     :: IORef Float,
  zh'     :: IORef Float,
  asp     :: IORef Float,
  fov     :: IORef Float,
  dim     :: IORef Float,
   
  ylight' :: IORef Float,
  rlight' :: IORef Float,
  emiss'  :: IORef Float,
  diff'   :: IORef Float,
  amb'    :: IORef Float,
  spec'   :: IORef Float,
  smooth' :: IORef ShadingModel,
  light'  :: IORef Bool,
  shine'  :: IORef Int,
  move'   :: IORef Bool,

  textures :: Textures,
   
  info    :: IORef (String,String)
}

makeState :: IO State
makeState = do
  f  <- newIORef 0
  t  <- newIORef 0
  ph <- newIORef 20
  th <- newIORef (-30)
  gr <- newIORef 0
  zh <- newIORef 90
  fv <- newIORef 65
  as <- newIORef 1
  di <- newIORef 2
  
  yl <- newIORef 0
  rl <- newIORef 5
  em <- newIORef 0
  df <- newIORef 45
  am <- newIORef 45
  sp <- newIORef 55
  sm <- newIORef Smooth
  li <- newIORef True
  sh <- newIORef 6
  mv <- newIORef True
  tx <- makeTextures

  i  <- newIORef ("","")
  return $ State {  
    frames = f, t0 = t, ph' = ph, th' = th, gr' = gr, zh' = zh, asp = as, fov = fv, dim = di, 
    ylight' = yl, rlight' = rl, emiss' = em, diff' = df, amb' = am, spec' = sp, smooth' = sm, light' = li, shine' = sh,
    move' = mv,
    textures = tx,
    info = i
  }


data Textures = Textures {
  steel :: TextureObject,
  water :: TextureObject,
  borg  :: TextureObject,
  alien :: TextureObject
} deriving (Show, Eq)

makeTextures :: IO Textures
makeTextures = do
  steel' <- loadGLTextureFromFile "resources/textures/steel.jpg"
  water' <- loadGLTextureFromFile "resources/textures/water.jpg"
  borg'  <- loadGLTextureFromFile "resources/textures/borg.jpg"
  alien' <- loadGLTextureFromFile "resources/textures/terran.jpg"
  --alien' <- loadGLTextureFromFile "resources/textures/earth.bmp"

  return $ Textures {
    steel = steel',
    water = water',
    borg  = borg',
    alien = alien'
  }


type Point3 = (Float, Float, Float)
data Point4 = Point4 Float Float Float Float deriving (Show, Eq)

type Scale      = Maybe Float
type Paint      = Maybe Point4
type Location   = Maybe Point3
type NoseVector = Maybe Point3
type UpVector   = Maybe Point3
type Ambience4  = Maybe Point4
type Diffuse4   = Maybe Point4
type Specular4  = Maybe Point4
type Emission4  = Maybe Point4
type Shininess  = Maybe Int



--type ObjectAttributes = (Scale, Paint, Location, NoseVector, UpVector, Ambience4, Diffuse4, Specular4, Shininess)
data ObjectAttributes = ObjectAttributes {
  scaleSize  :: Scale,
  paint      :: Paint,
  location   :: Location,
  noseVector :: NoseVector,
  upVector   :: UpVector,
  ambience4  :: Ambience4,
  diffuse4   :: Diffuse4,
  specular4  :: Specular4,
  emission4  :: Emission4,
  shininess  :: Shininess
} deriving (Show, Eq)
