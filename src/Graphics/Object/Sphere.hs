module Graphics.Object.Sphere (drawSphere) where 
 
import Graphics.UI.GLUT

import Graphics.Util.GLUtils
import Data.State

  

--Draw solid sphere
drawSphere :: State -> ObjectAttributes -> IO ()
drawSphere state object@(ObjectAttributes scaleSize paint location noseVector upVector ambience4 diffuse4 specular4 emission4 shininess) = do

  preservingMatrix $ do
    preservingAttrib [AllServerAttributes] $ do
      let q = 5
      
      case (paint, location, scaleSize) of
        ((Just (Point4 px py pz pa)), (Just (lx, ly, lz)), (Just s))-> do 
          color3f px py pz
          translate $ vector3f lx ly lz
          scale3f s s s

          drawLightingEffects object

          mapM_ (\ph -> do
              renderPrimitive QuadStrip $ mapM_ (\th -> do
                drawLatBand q (ph, th)
                drawLatBand q ((ph+5), th)) (sphereTh q)
            ) (spherePh q)