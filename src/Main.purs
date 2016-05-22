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
import Color.Scale.Perceptual (inferno)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import DOM.File.FileList (length)
import DOM.HTML (window)
import DOM.HTML.HTMLCanvasElement (height)
import DOM.HTML.Window (innerWidth, innerHeight)
import DOM.RequestAnimationFrame (requestAnimationFrame)
import Data.Array (length, filter, index, zip, (..), mapMaybe)
import Data.Foldable (fold)
import Data.Tuple (Tuple(..))
import Graphics.Canvas (getCanvasElementById, getContext2D, setCanvasWidth, setCanvasHeight, getCanvasDimensions)

infixl 0 |>
(|>) a b = b a

cellsize = 25.0
colors = 8

data Cell = Cell {state :: Int , x :: Number , y :: Number}

makecolor state =
	let hue = (toNumber state) * (360.0 / (toNumber colors))
	in hsla hue 1.0 0.5 0.5

-- need f**king unsafePerformEff here
newcell x y = Cell {state: (randomInt 0 colors |> unsafePerformEff), x: (toNumber x), y: (toNumber y)}

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
evaluate prevcells = prevcells |> map (\row -> row |> map (\el -> evalcell el prevcells))
evalcell (Cell {state: state, x: xr, y: yr}) prevcells =
	let
		x = round xr
		y = round yr
		nextstate = (state + 1) `mod` colors
		around = [[x-1,y],[x+1,y],[x,y-1],[x,y+1]] |> mapMaybe (\[x,y] -> index prevcells y >>= \row -> index row x )
		innext = around |> filter \(Cell {state: st}) -> st == nextstate
	in
		if (Data.Array.length(innext) == 0) then (Cell {state: state, x: xr, y: yr}) else (Cell {state: nextstate, x: xr, y: yr})

drawcells newcells ctx = do
	foreachE newcells \row -> foreachE row \(Cell {state: state, x: x, y: y}) -> do
		rectangle (x * cellsize) (y * cellsize) cellsize cellsize
			|> filled (state |> makecolor |> fillColor)
			|> render ctx

loop prevcells =
	requestAnimationFrame do
		Just canvas <- getCanvasElementById "canvas"
		ctx <- getContext2D canvas
		{width: this_width, height: this_heigth} <- getCanvasDimensions(canvas)
		window >>= innerWidth >>= \n -> return (toNumber n) >>= \n -> if (n /= this_width) then setCanvasWidth n canvas else return canvas
		window >>= innerHeight >>= \n -> return (toNumber n) >>= \n -> if (n /= this_heigth) then setCanvasHeight n canvas else return canvas
		newcells <- claimcells prevcells ((+) 1 $ round $ this_width / cellsize) ((+) 1 $ round $ this_heigth / cellsize) |> evaluate |> return
		-- render new objects
		drawcells newcells ctx
		loop newcells

main = do
	loop []
