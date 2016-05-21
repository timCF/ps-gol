module Main where

import Prelude

import Data.Int
import Data.Maybe
import Data.Array ((..))
import Data.List
import Data.Monoid (mempty)
import Data.Foldable (fold)

import Control.Monad.Eff

import Graphics.Drawing
import Graphics.Canvas (getCanvasElementById, getContext2D)

import Control.Monad.Eff.Random
import Control.Monad.Eff.Console

import DOM.HTML(window)
import DOM.HTML.Window(innerWidth, innerHeight)
import DOM.RequestAnimationFrame (requestAnimationFrame)

infixl 0 |>
(|>) a b = b a

newobj w h = circle (toNumber w) (toNumber h) 10.0 |> filled (fillColor $ fromInt 0xff0000)
alpha w h = rectangle 0.0 0.0 w h |> filled (fillColor $ rgba 0 0 0 0.1)

loop = requestAnimationFrame do
	Just canvas <- getCanvasElementById "canvas"
	ctx <- getContext2D canvas
	this_width <- window >>= \w -> innerWidth(w)
	this_heigth <- window >>= \w -> innerHeight(w)
	w <- randomInt 0 this_width
	h <- randomInt 0 this_heigth
	render ctx $ alpha (toNumber this_width) (toNumber this_heigth)
	render ctx $ newobj w h
	print [this_width, this_heigth]
	loop

main = do loop
