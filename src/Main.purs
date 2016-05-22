module Main where

import Prelude
import Data.Int
import Data.Maybe
import Control.Monad.Eff
import Graphics.Drawing
import Control.Monad.Eff.Random
import Control.Monad.Eff.Console
import DOM
import Color
import DOM.HTML (window)
import DOM.HTML.HTMLCanvasElement (height)
import DOM.HTML.Window (innerWidth, innerHeight)
import DOM.RequestAnimationFrame (requestAnimationFrame)
import Data.Array (zip, (..))
import Data.Foldable (fold)
import Data.Tuple (Tuple(..))
import Graphics.Canvas (getCanvasElementById, getContext2D, setCanvasWidth, setCanvasHeight, getCanvasDimensions)

infixl 0 |>
(|>) a b = b a

setalpha color = let s = color |> toRGBA in rgba s.r s.g s.b 0.1
cellsize = 50.0
mincolor = 0x000000
maxcolor = 0xffffff
data Cell = Cell {state :: Int , x :: Number , y :: Number}
newcell x y = randomInt mincolor maxcolor >>= \state -> return $ Cell {state: state, x: (toNumber x), y: (toNumber y)}

claimcells prevcells xn yn =
	let prevyn = Data.Array.length prevcells
	in
		(if (prevyn >= yn) then Data.Array.take yn prevcells else Data.Array.concat [prevcells, ((prevyn..yn) |> map \_ -> [])])
		|> Data.Array.zip (0..yn)
		|> map \(Tuple yindex row) -> claimrow row xn yindex
claimrow prevrow xn yindex =
	let prevxn = Data.Array.length prevrow
	in
		if (prevxn >= xn) then Data.Array.take xn prevrow else Data.Array.concat [prevrow, (prevxn..xn |> map \x -> newcell x yindex)]
evalcells prevcells = prevcells

drawcells newcells ctx = do
	foreachE newcells \row -> foreachE row \cell -> do
		Cell {state: state, x: x, y: y} <- cell
		rectangle (x * cellsize) (y * cellsize) cellsize cellsize
			|> filled (state |> fromInt |> setalpha |> fillColor)
			|> render ctx

loop prevcells =
	requestAnimationFrame do
		Just canvas <- getCanvasElementById "canvas"
		ctx <- getContext2D canvas
		{width: this_width, height: this_heigth} <- getCanvasDimensions(canvas)
		window >>= innerWidth >>= \n -> return (toNumber n) >>= \n -> if (n /= this_width) then setCanvasWidth n canvas else return canvas
		window >>= innerHeight >>= \n -> return (toNumber n) >>= \n -> if (n /= this_heigth) then setCanvasHeight n canvas else return canvas
		-- render background and meltdown effect
		-- rectangle 0.0 0.0 this_width this_heigth |> filled (fillColor $ rgba 0 0 0 0.1) |> render ctx
		newcells <- claimcells prevcells (round $ this_width / cellsize) ( round $ this_heigth / cellsize) |> evalcells |> return
		-- render new objects
		drawcells newcells ctx
		loop newcells

main = do
	loop [[(newcell 0 0)]]
