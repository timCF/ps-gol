module Main where

import Prelude
import Data.Int
import Data.Maybe
import Data.List
import Control.Monad.Eff
import Graphics.Drawing
import Control.Monad.Eff.Random
import Control.Monad.Eff.Console
import Control.Bind (ifM)
import DOM.HTML (window)
import DOM.HTML.HTMLCanvasElement (height)
import DOM.HTML.Window (innerWidth, innerHeight)
import DOM.RequestAnimationFrame (requestAnimationFrame)
import Data.Array ((..))
import Data.Foldable (fold)
import Graphics.Canvas (getCanvasElementById, getContext2D, setCanvasWidth, setCanvasHeight, getCanvasDimensions)

infixl 0 |>
(|>) a b = b a

newobj w h = circle w h 10.0 |> filled (fillColor $ fromInt 0xff0000)
alpha w h = rectangle 0.0 0.0 w h |> filled (fillColor $ rgba 0 0 0 0.1)

loop = requestAnimationFrame do
	Just canvas <- getCanvasElementById "canvas"
	ctx <- getContext2D canvas
	{width: this_width, height: this_heigth} <- getCanvasDimensions(canvas)
	window >>= innerWidth >>= \n -> return (toNumber n) >>= \n -> if (n /= this_width) then setCanvasWidth n canvas else return canvas
	window >>= innerHeight >>= \n -> return (toNumber n) >>= \n -> if (n /= this_heigth) then setCanvasHeight n canvas else return canvas
	-- render background and meltdown effect
	render ctx $ alpha this_width this_heigth
	w <- random >>= \n -> return $ n * this_width
	h <- random >>= \n -> return $ n * this_heigth
	-- render new object
	render ctx $ newobj w h
	loop

main = do loop
