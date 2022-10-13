{-# LANGUAGE Rank2Types #-}
module Main(main) where

import Reanimate
import Reanimate.Scene
import Reanimate.Builtin.Documentation
import Control.Lens hiding (transform)
import Control.Lens.At (Ixed)
import Control.Monad.State.Class (gets)
import Graphics.SvgTree.Types (Tree, defaultSvg
                              , rectangleTree
                              , clipPathTree
                              , clipPathContent
                              , rectWidth
                              , rectHeight
                              , Number(Num, Percent)
                              , attrId
                              , clipPathRef
                              , ElementRef(Ref)
                              , Transformation(Translate)
                              , TreeBranch(ClipPathNode)
                              , treeBranch
                              , transform

                              , circleTree
                              , circleRadius
                              )
import qualified Graphics.SvgTree.Types as SvgTreeTy
import Control.Monad (void, forM)


objRectWidth :: Lens' Rectangle Double
objRectWidth = Reanimate.Scene.rectWidth

objRectHeight :: Lens' Rectangle Double
objRectHeight = Reanimate.Scene.rectHeight

main :: IO ()
main = reanimate $ slidingTransition "white" 2 0.5 ["skyblue", "dodgerblue", "royalblue"]

-- | Creates transition animation 1
--
-- Based on the AE transition tutorial: https://www.youtube.com/watch?v=pxFLnPPB-R4
slidingTransition :: String -> Duration -> Double -> [String] -> Animation
slidingTransition bgColor duration steepness colors = scene $ do
  background <- oNew (withFillColor bgColor $ mkRect screenWidth screenHeight)
  oShow background
  cPath <- flip simpleVar screenLeft $ \d -> clipPathTree $ defaultSvg
                                        -- &clipPathContent.~[circleTree $ defaultSvg&svgCircleRadius.~(Percent width)
                                        --                   ]
                                        &clipPathContent.~[rectangleTree $ defaultSvg
                                                           &SvgTreeTy.transform.~(Just [SvgTreeTy.Translate d (-screenHeight)])
                                                           &SvgTreeTy.rectWidth.~(Just $ Percent 100)
                                                           &SvgTreeTy.rectHeight.~(Just $ Percent 100)]
                                        &attrId.~(Just "slidingTransition.Mask")

  slides <-  forM (zip colors [0,steepness..]) $ \(color, offset) -> do
    obj <- fork $ slidingPlane duration offset color
    oModifyS obj (oContext.=(clipPathRef.~Just (Ref "slidingTransition.Mask")))

  oModifyS background (oContext.=(clipPathRef.~Just (Ref "slidingTransition.Mask"))) -- TODO: remove this; visualizing clippath
  wait 2.1
  tweenVar cPath 2 $ \_ -> \t ->  (-screenWidth*2) + (screenWidth*2)*(curveS 3 t)
  -- tweenVar cPath 2 $ \_ -> fromToS screenLeft screenRight
  -- tweenVar cPath 2 $ \_ -> fromToS (-screenWidth*2) 0
  return ()
  

slidingPlane :: Duration -> Duration -> String -> Scene s (Object s Tree)
slidingPlane duration offset color = do
  box <- oNew (withFillColor color $ mkRect screenWidth screenHeight)
  wait offset
  oShow box
  oModifyS box (oEasing .= curveS 3)
  oTweenS box duration (\t -> oCenterX .= fromToS (-screenWidth*2) 0 t)
  return box
