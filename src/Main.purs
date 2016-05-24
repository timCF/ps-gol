module Main where

import Prelude
import Control.Monad.Eff (foreachE)
import Data.Array ((..))
import Data.Int (toNumber, round)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

infixl 0 |>
(|>) a b = b $ a

cellsize = 25.0
colors = 8

data Cell = Cell {state :: Int , x :: Number , y :: Number}


makecolor state =
	let hue = (toNumber state) * (360.0 / (toNumber colors))
	in Color.hsla hue 1.0 0.5 0.5 |> Color.cssStringRGBA

-- need f**king unsafePerformEff here
newcell x y = Cell {state: (Control.Monad.Eff.Random.randomInt 0 colors |> Control.Monad.Eff.Unsafe.unsafePerformEff), x: (toNumber x), y: (toNumber y)}


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
		around = [[x-1,y],[x+1,y],[x,y-1],[x,y+1]] |> Data.Array.mapMaybe (\[x,y] -> Data.Array.index prevcells y >>= \row -> Data.Array.index row x )
		innext = around |> Data.Array.filter \(Cell {state: st}) -> st == nextstate
	in
		if (Data.Array.length(innext) == 0) then (Cell {state: state, x: xr, y: yr}) else (Cell {state: nextstate, x: xr, y: yr})


diffcells prevcells newcells =
	let filterpred xn yn sn = do
			case Data.Array.index prevcells (round yn) >>= \row -> Data.Array.index row (round xn) of
				Nothing -> true
				Just (Cell {x: xp, y: yp, state: sp}) -> not((xp == xn) && (yp == yn) && (sp == sn))
	in
		newcells |> Data.Array.concat |> Data.Array.filter \(Cell {x: xn, y: yn, state: sn}) -> filterpred xn yn sn


drawcells ctx newcells =
	let groupedcells = newcells |> Data.Array.groupBy (\(Cell {state: s1}) (Cell {state: s2}) -> s1 == s2)
	in
		foreachE groupedcells \thisgroup -> do
			case (Data.Array.head thisgroup >>= \(Cell {state: state}) -> makecolor state |> return) of
				Nothing -> return unit
				Just color -> do
					coloredctx <- Graphics.Canvas.setFillStyle color ctx
					foreachE thisgroup \(Cell {x: x, y: y}) -> do
						Graphics.Canvas.fillRect coloredctx { x:  (x * cellsize), y: (y * cellsize), w: cellsize, h: cellsize }
						return unit


loop prevcells canvas ctx =
	DOM.RequestAnimationFrame.requestAnimationFrame do
		{width: this_width, height: this_heigth} <- Graphics.Canvas.getCanvasDimensions(canvas)
		DOM.HTML.window >>= DOM.HTML.Window.innerWidth >>= \n -> return (toNumber n) >>= \n -> if (n /= this_width) then Graphics.Canvas.setCanvasWidth n canvas else return canvas
		DOM.HTML.window >>= DOM.HTML.Window.innerHeight >>= \n -> return (toNumber n) >>= \n -> if (n /= this_heigth) then Graphics.Canvas.setCanvasHeight n canvas else return canvas
		newcells <- claimcells prevcells ((+) 1 $ round $ this_width / cellsize) ((+) 1 $ round $ this_heigth / cellsize) |> evaluate |> return
		-- render new objects
		newcells |> diffcells prevcells |> drawcells ctx
		loop newcells canvas ctx


main = do
	Just canvas <- Graphics.Canvas.getCanvasElementById "canvas"
	ctx <- Graphics.Canvas.getContext2D canvas
	loop [] canvas ctx
