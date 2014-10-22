module Graphics.Object.Sphere (drawSphere) where 
 
import Graphics.UI.GLUT
import Data.Fixed

import Graphics.Util.GLUtils
import Data.State

spherePh:: Float -> [Float]
spherePh d = [ ph | ph <- [(-90.0)..90.0], ((mod' ph d) == 0 && ph < 90)]

sphereTh:: Float -> [Float]
sphereTh d = [th | th <- [0.0..360.0], (mod' th d) == 0]


drawLatBand :: Float -> (Float,Float) -> IO ()
drawLatBand d (ph, th) =  do

  
  --double x = -Sin(th)*Cos(ph);
  --double y =  Cos(th)*Cos(ph);
  --double z =          Sin(ph);

  drawNormal3f ((sin th)*(cos ph)) (sin ph) ((cos th)*(cos ph))
  --drawTexCoord2f (th/360) (ph/180+0.5)
  drawVertex3f ((sin th)*(cos ph)) (sin ph) ((cos th)*(cos ph))

  drawNormal3f ((sin th)*(cos (ph+d))) (sin (ph+d)) ((cos th)*(cos (ph+d)))
  --drawTexCoord2f (th/360) ((ph+d)/180+0.5)
  drawVertex3f ((sin th)*(cos (ph+d))) (sin (ph+d)) ((cos th)*(cos (ph+d)))

  

--Draw solid sphere
drawSphere :: State -> ObjectAttributes -> IO ()
drawSphere state object@(ObjectAttributes scaleSize paint location noseVector upVector ambience4 diffuse4 specular4 emission4 shininess) = do

  preservingMatrix $ do
    preservingAttrib [AllServerAttributes] $ do
      let q = 5
          tex = textures state
          alien' = alien tex


      case (paint, location, scaleSize) of
        ((Just (Point4 px py pz pa)), (Just (lx, ly, lz)), (Just s))-> do 
          color3f px py pz
          translate $ vector3f lx ly lz
          scale3f s s s

          drawLightingEffects object

          texture Texture2D $= Enabled
          textureBinding Texture2D $= Just alien'
          --textureWrapMode Texture2D S $= (Repeated, Clamp)
          --textureWrapMode Texture2D T $= (Repeated, Clamp)
          textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
          --textureFunction $= Modulate

          mapM_ (\ph -> do
              renderPrimitive QuadStrip $ mapM_ (\th -> drawLatBand q (ph, th)) (sphereTh q)
            ) (spherePh q)