module Main where

import IO.IO exposing (..)
import IO.Runner exposing (Request, Response)
import IO.Runner as Run

import Test
import ElmTest.Runner.Console as Console



import ElmTest.Test exposing (Test, test, suite)
import ElmTest.Assertion exposing (..)
import Time exposing (every, second)

tick = every (0.5 * second)
tick' = every (1.5 * second)
alternate s = Signal.foldp (\_ b -> not b) False s
invert s = Signal.map not s
flip s =
  Signal.foldp (\b r -> if b
                        then (Just <| snd r, not <| snd r)
                        else (Nothing, snd r)) (Nothing, True) s
  |> Signal.map fst

alternatingSignal = tick
  |> alternate
  |> flip

alternatingSignal2 = tick'
  |> alternate
  |> invert
  |> flip

alternatingSignal3 = tick'
  |> alternate
  |> flip

alternatingSignal4 = tick
  |> alternate
  |> invert
  |> flip

alternatingSignalTest : Test
alternatingSignalTest = test "alternating signal test" <| assertSignal alternatingSignal




sigs : Signal (IO ())
sigs = Console.mapDisplay alternatingSignalTest

-- | Can't use a type alias in ports, yet :/
port requests : Signal Request
port requests = Run.map responses sigs

port responses : Signal Response
