module Ising where

import Window
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import Text exposing (..)
import Signal
import Time exposing (Time)
import Random
import Array exposing (Array)

import Array2d exposing (..)


-- FFI

-- port inSeed : Int

-- Data Model

-- Spins are modeled as floats, +1 or -1 for (+) or (-)

type alias Spin = Float

{-- We represent a spin lattice as a 2d array of spins on (X, Y)
      Y -->
      ----------------------
    X (-) | (-) | (-) | (-)
    | ----------------------
    | (+) | (+) | (+) | (+)
    v  ----------------------
      (+) | (+) | (+) | (+)
      ----------------------
      (-) | (-) | (-) | (-)
--}

type alias Lattice = Array2d Spin

{--
  Initialize a spin lattice of equilateral size nSize All spins are initially (-)
--}

lattice : Int -> Lattice
lattice nSize = Array2d.repeat nSize nSize -1

type alias Observable = Array Float
type alias Observables = {
  magnetization : Observable,
  energy : Observable,
  square_energy : Observable
}

type alias State = {
  lattice : Lattice,
  rowSeed : Random.Seed,
  colSeed : Random.Seed,
  acceptSeed : Random.Seed,
  temperature : Float,
  nStep : Int,
  observables : Observables
}

rowGen : Lattice -> Random.Generator Int
rowGen lattice = Random.int 0 (Array2d.nRows lattice)

colGen : Lattice -> Random.Generator Int
colGen lattice = Random.int 0 (Array2d.nCols lattice)

{--
  Retrieves a spin value from the lattice.
  Enforces periodic boundary conditions.
--}
getSpin : Int -> Int -> Lattice -> Spin
getSpin rowIdx colIdx lattice =
  let
    nRows = Array2d.nRows lattice
    nCols = Array2d.nCols lattice
    rowIdx' = if 0 <= rowIdx && rowIdx < nRows then rowIdx else rowIdx % nRows
    colIdx' = if 0 <= colIdx && colIdx < nCols then colIdx else colIdx % nCols
  in
    case (Array2d.get rowIdx' colIdx' lattice) of
      Just spin -> spin
      Nothing -> 0

{--
  Return a list of a site's neighboring site spin values.
--}
neighborSpins : Int -> Int -> Lattice -> List Spin
neighborSpins rowIdx colIdx lattice =
  [ --getSpin (rowIdx + 1) (colIdx + 1) lattice,
    getSpin rowIdx       (colIdx + 1) lattice,
    --getSpin (rowIdx - 1) (colIdx + 1) lattice,
    getSpin (rowIdx - 1) colIdx       lattice,
    --getSpin (rowIdx - 1) (colIdx - 1) lattice,
    getSpin rowIdx       (colIdx - 1) lattice,
    --getSpin (rowIdx + 1) (colIdx - 1) lattice,
    getSpin (rowIdx + 1) colIdx       lattice ]

{--
  Calculate a site's interaction factor with its neighbor sites
  given a spin value.
--}

spinInteraction : Int -> Int -> Spin -> Lattice -> Float
spinInteraction row col spin lattice =
  negate (List.sum (List.map (\spin' -> spin * spin') (neighborSpins row col lattice)))

{--
  Compute observables from lattice.
--}
computeObservables : Lattice -> Observables -> Observables
computeObservables lattice observables =
  let
    m = Array2d.foldl (+) 0.0 lattice
    e = Array2d.foldl (+) 0.0 (Array2d.indexedMap (\ i j v -> spinInteraction i j (getSpin i j lattice) lattice) lattice)
    square_e = e ^ 2.0
  in
    { observables | magnetization <- Array.push m observables.magnetization,
                    energy <- Array.push e observables.energy,
                    square_energy <- Array.push square_e observables.square_energy }

{--
  Evolve the state of a lattice according to an MCMC algorithm.

  1. Select a random site from the lattice according to a uniform distribution
     across the sites.
  2. Calculate current energy contribution of site spin.
  3. Negate spin value and calculate proposed spin contribution.
  4. If the dE < 0 -> keep proposal.
  5. Otherwise, keep proposal with probability exp(-beta * dE)

--}

nextState : State -> State
nextState prevState =
  let
    lattice = prevState.lattice
    (randRow, rowSeed') = Random.generate (rowGen lattice) prevState.rowSeed
    (randCol, colSeed') = Random.generate (colGen lattice) prevState.colSeed
    currentSpin = getSpin randRow randCol lattice
    flippedSpin = negate currentSpin
    currentE = spinInteraction randRow randCol currentSpin lattice
    proposedE = spinInteraction randRow randCol flippedSpin lattice
    dE = proposedE - currentE
    (flipProbability, acceptSeed') = Random.generate (Random.float 0 1) prevState.acceptSeed
    logFlipProbability = logBase e flipProbability
    keepFlip = dE < 0 || logFlipProbability < (-1.0 / prevState.temperature) * dE
    observables' = computeObservables lattice' prevState.observables
    lattice' = if keepFlip then Array2d.set randRow randCol flippedSpin lattice else lattice
  in
    { prevState | lattice <- lattice',
                  rowSeed <- rowSeed',
                  colSeed <- colSeed',
                  acceptSeed <- acceptSeed',
                  nStep <- prevState.nStep + 1,
                  observables <- observables'
                  }

-- Display

{--
  Render current lattice state to screen
--}

renderSpin : Spin -> Element
renderSpin spin =
  let symbol = if | spin <  0.0 -> (Text.color red (fromString "(-)"))
                  | spin >= 0.0 -> (Text.color blue (fromString "(+)"))
                  | otherwise   -> (fromString "(?)")
  in
    (leftAligned (monospace symbol))

renderSpin2 : (Float, Float) -> Spin -> Form
renderSpin2 pos spin =
  let color = if spin < 0.0 then orange else blue
  in
    move pos (filled color (circle 6.0))

renderGrid : Int -> Int -> Lattice -> Form
renderGrid width height lattice =
  let nRows = toFloat <| Array2d.nRows lattice
      nCols = toFloat <| Array2d.nCols lattice
      dY = (toFloat height) / nRows
      dX = (toFloat width) / nCols
      rows = List.map (\n -> traced defaultLine (segment (0.0, n * dY) (toFloat width, n * dY))) [0 .. nRows]
      cols = List.map (\n -> traced defaultLine (segment (n * dX, 0.0) (n * dX, toFloat height))) [0 .. nCols]
      spins = List.concat (List.indexedMap (\i row -> (List.indexedMap (\j spin -> (renderSpin2 ((toFloat i) * dY, (toFloat j) * dX) spin)) row))
                            (Array2d.asRows lattice))
  in
    group (List.concat [rows, cols, spins])


render : (Int, Int) -> State -> Element
render (width, height) state =
  container width height topLeft <|
    collage width height
      [ renderGrid 400 400 state.lattice,
        move (0, -40.0) <| toForm <| show state.nStep ]
{--
  flow down
  [ flow down
      (List.map (\row -> (flow right (List.map renderSpin row)))
                (Array2d.asRows state.lattice))
  , show state.nStep
  , show (getLast state.observables.energy) ]
--}


initialState = {
  lattice = lattice 20,
  rowSeed = Random.initialSeed 15485867,
  colSeed = Random.initialSeed 32452843,
  acceptSeed = Random.initialSeed 5,
  temperature = 1E6,
  nStep = 0,
  observables = {
    magnetization = Array.empty,
    energy = Array.empty,
    square_energy = Array.empty } }


latticeStates = Signal.foldp (\ _ state -> (nextState state)) initialState (Time.fps 1000)

main : Signal Element
main = Signal.map2 render Window.dimensions (Signal.sampleOn (Time.fps 1.0) latticeStates)
