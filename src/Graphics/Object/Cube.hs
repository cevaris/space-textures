module Graphics.Object.Cube (drawCube) where 
 
import Graphics.UI.GLUT

import Graphics.Util.GLUtils
import Data.State

--drawCube :: Float -> (Float, Float, Float) -> IO ()
--drawCube s (x, y, z) = do
drawCube :: State -> ObjectAttributes -> IO ()
drawCube state object@(ObjectAttributes scaleSize paint location noseVector upVector _ _ _ _ _) = do

  let w = 1.0

  let tex = textures state
      borg' = borg tex

  preservingMatrix $ do
    preservingAttrib [AllServerAttributes] $ do

      drawLightingEffects object

        
      case (paint, location, scaleSize) of
        ((Just (Point4 px py pz pa)), (Just (lx, ly, lz)), (Just s)) -> do 
          color3f px py pz
          translate $ vector3f lx ly lz
          scale3f s s s
          
          texture Texture2D $= Enabled
          textureBinding Texture2D $= Just borg'
          --textureWrapMode Texture2D S $= (Repeated, Clamp)
          --textureWrapMode Texture2D T $= (Repeated, Clamp)
          textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
          textureFunction $= Modulate

          renderPrimitive Quads $ do
            cube w


cube :: Float -> IO ()
cube w = do

  -- Front
  drawNormal3f 0 0 w
  drawTexCoord2f 0 0
  drawVertex3f (-w) (-w)  w
  drawTexCoord2f 1 0
  drawVertex3f w (-w)  w
  drawTexCoord2f 1 1
  drawVertex3f w w  w
  drawTexCoord2f 0 1
  drawVertex3f (-w) w  w
  -- Back
  drawNormal3f 0  0 (-w)
  drawTexCoord2f 0 0
  drawVertex3f w (-w) (-w)
  drawTexCoord2f 1 0
  drawVertex3f (-w) (-w) (-w)
  drawTexCoord2f 1 1
  drawVertex3f (-w) w (-w)
  drawTexCoord2f 0 1
  drawVertex3f w w (-w)
  -- Right
  drawNormal3f w  0  0
  drawTexCoord2f 0 0
  drawVertex3f w (-w) w
  drawTexCoord2f 1 0
  drawVertex3f w (-w) (-w)
  drawTexCoord2f 1 1
  drawVertex3f w w (-w)
  drawTexCoord2f 0 1
  drawVertex3f w w w
  -- Left
  drawNormal3f (-w)  0  0
  drawTexCoord2f 0 0
  drawVertex3f (-w) (-w) (-w)
  drawTexCoord2f 1 0
  drawVertex3f (-w) (-w) w
  drawTexCoord2f 1 1
  drawVertex3f (-w) w w
  drawTexCoord2f 0 1
  drawVertex3f (-w) w (-w)
  -- Top
  drawNormal3f 0 w  0
  drawTexCoord2f 0 0
  drawVertex3f (-w) w w
  drawTexCoord2f 1 0
  drawVertex3f w w w
  drawTexCoord2f 1 1
  drawVertex3f w w (-w)
  drawTexCoord2f 0 1
  drawVertex3f (-w) w (-w)
  -- Bottom
  drawNormal3f 0 (-1) 0
  drawTexCoord2f 0 0
  drawVertex3f (-w) (-w) (-w)
  drawTexCoord2f 1 0
  drawVertex3f w (-w) (-w)
  drawTexCoord2f 1 1
  drawVertex3f w (-w) w
  drawTexCoord2f 0 1
  drawVertex3f (-w) (-w) w
