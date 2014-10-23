module Graphics.Object.Station (drawStation) where 
 
import Graphics.UI.GLUT

import Graphics.Util.GLUtils

import Graphics.Object.MetalCube
import Graphics.Object.MetalPyramid
import Data.State
  
drawStation :: State ->
               GLfloat ->
               Float->
               (Float, Float, Float) -> IO ()
drawStation state a s (x, y, z) = do

    ambience <- get (amb' state)
    diffusion <- get (diff' state)
    specularizion <- get (spec' state)
    emission <- get (emiss' state)
    
    shineVal   <- get (shine' state)
    let shine = shineVal^2

    let ws = s*0.6
        wd = s*2
        cs = s*1.5
        ambs     = (Point4 (0.01*ambience) (0.01*ambience) (0.01*ambience) 1.0)
        diffs    = (Point4 (0.01*diffusion) (0.01*diffusion) (0.01*diffusion) 1.0)
        specs    = (Point4 (0.01*specularizion) (0.01*specularizion) (0.01*specularizion) 1.0)
        emiss    = (Point4 0.0 0.0 (0.01*emission) 1.0)
        yellow   = (Point4 1.0 1.0 0.0 1.0)
        white    = (Point4 1 1 1 1)
        black    = (Point4 0 0 0 1)
        gray     = (Point4 (112/255) (128/255) (144/255) 0)
        lightGray = (Point4 (220/255) (220/255) (220/255) 0)
    
    preservingMatrix $ do
      preservingAttrib [AllServerAttributes] $ do

        translate $ vector3f x y z
        scale3f s s s
        rotate a (Vector3 0 1 0)

        drawMetalCube state $ ObjectAttributes {
          rotation   = Nothing,  
          scaleSize  = Just cs,
          paint      = Just white,
          location   = Just $ (0, 0, 0),
          noseVector = Nothing,
          upVector   = Nothing,
          ambience4  = Just white,
          diffuse4   = Just yellow,
          specular4  = Just yellow,
          emission4  = Just emiss,
          shininess  = Just shine
        }
        -- Bottom
        drawMetalPyramid state $ ObjectAttributes {
          rotation   = Nothing,
          scaleSize  = Just $ s,
          paint      = Just white,
          location   = Just $ (0,(-0.75),0),
          noseVector = Just $ (1,0,0),
          upVector   = Just $ (0,1,0),
          ambience4  = Just white,
          diffuse4   = Just yellow,
          specular4  = Just yellow,
          emission4  = Just emiss,
          shininess  = Just shine
        }
        -- Top
        drawMetalPyramid state $ ObjectAttributes {
          rotation   = Nothing,
          scaleSize  = Just $ s,
          paint      = Just white,
          location   = Just $ (0,0.75,0),
          noseVector = Just $ (1,0,0),
          upVector   = Just $ (0,(-1),0),
          ambience4  = Just white,
          diffuse4   = Just yellow,
          specular4  = Just yellow,
          emission4  = Just emiss,
          shininess  = Just shine
        }
        
            
      