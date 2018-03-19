module Example.Rectangle where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Int (fromNumber, toNumber, fromString, floor)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (CANVAS, rect, fillPath, setFillStyle, getContext2D,
                          getCanvasElementById)
import Partial.Unsafe (unsafePartial)

import Control.Monad.Eff
import Data.Traversable
import Data.List
import Math
import Graphics.Canvas hiding (translate)

type Point = { x :: Number, y :: Number }

renderPath :: forall eff. Context2D -> List Point -> Eff (canvas :: CANVAS | eff) Context2D
renderPath ctx Nil = closePath ctx
renderPath ctx (firstPoint : points) = do
  _ <- setStrokeStyle "#000000" ctx
  strokePath ctx $ do
    _ <- moveTo ctx firstPoint.x firstPoint.y
    _ <- traverse (\point -> lineTo ctx point.x point.y) points
    closePath ctx



-- points =
--   let firstPoint = head
--   let Just tailPoints = tail points
--   case uncons points of
--          Just {head : firstPoint, tail: tailPoints} -> do
--            _ <- setStrokeStyle "#000000" ctx
--            strokePath ctx $ do
--              moveTo ctx firstPoint.x firstPoint.y
--              traverse (\point -> lineTo ctx point.x point.y) tailPoints
--              closePath ctx
--          Nothing ->

f :: Number -> Point
f x = { x: x * 4.0, y: 250.0 + (sin x * 100.0) }
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"


  ctx <- getContext2D canvas
  _ <-     setFillStyle "#0000FF" ctx

  _ <-     fillPath ctx $ do
         _ <- rect ctx  { x: 250.0
                        , y: 250.0
                        , w: 100.0
                        , h: 1000.0
                        }
         rect ctx  { x: 100.0
                   , y: 250.0
                   , w: 100.0
                   , h: 100.0
                   }

  _ <- renderPath ctx ({ x: 10.0, y: 20.0 } : { x: 30.0, y: 49.0 } : { x: 10.0, y: 60.0 } : Nil )

 renderPath ctx $ f <$> (map fromInt (1 .. 500))
