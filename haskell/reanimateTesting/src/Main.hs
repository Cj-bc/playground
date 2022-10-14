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
  cPath <- flip simpleVar screenLeft $ \d -> clipPathTree $ defaultSvg
                                        &clipPathContent.~[rectangleTree $ defaultSvg
                                                           &SvgTreeTy.transform.~(Just [SvgTreeTy.Translate d (-screenHeight)])
                                                           &SvgTreeTy.rectWidth.~(Just $ Percent 100)
                                                           &SvgTreeTy.rectHeight.~(Just $ Percent 100)]
                                        &attrId.~(Just "slidingTransition.Mask")

  slides <-  forM (zip colors [0,steepness..]) $ \(color, offset) -> do
    obj <- fork $ slidingPlane duration offset color
    oModifyS obj (oContext.=(clipPathRef.~Just (Ref "slidingTransition.Mask")))

  wait 2.1
  tweenVar cPath 2 $ \_ -> \t ->  (-screenWidth*2) + (screenWidth*2)*(curveS 3 t)
  return ()
  

slidingPlane :: Duration -> Duration -> String -> Scene s (Object s Tree)
slidingPlane duration offset color = do
  box <- oNew (withFillColor color $ mkRect screenWidth screenHeight)
  wait offset
  oShow box
  oModifyS box (oEasing .= curveS 3)
  oTweenS box duration (\t -> oCenterX .= fromToS (-screenWidth*2) 0 t)
  return box
