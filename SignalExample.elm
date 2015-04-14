module Main where

import Graphics.Element exposing (..)
import ElmTest.Run as Run
import ElmTest.Runner.Element as ElementRunner
import ElmTest.Runner.String  as StringRunner
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

alternatingSignalTest2 : Test
alternatingSignalTest2 = test "ALTERNATING SIGNAL TEST" <| assertMap identity alternatingSignal2

alternatingSignalTest3 : Test
alternatingSignalTest3 = test "ALTERNATING signal TEST" <| assertSignal alternatingSignal3

alternatingSignalTest4 : Test
alternatingSignalTest4 = test "alternating SIGNAL test" <| assertMap identity alternatingSignal4


alternatingSignalSuite : Test
alternatingSignalSuite = suite "alternating signal suite" [ alternatingSignalTest
                                                          , alternatingSignalTest2
                                                          ]

alternatingSignalSuite2 : Test
alternatingSignalSuite2 = suite "alternating signal suite" [ alternatingSignalTest3
                                                          , alternatingSignalTest4
                                                          ]

allTests : Test
allTests = suite "All tests" [ alternatingSignalTest
                             , alternatingSignalTest3
                             , alternatingSignalSuite
                             , alternatingSignalSuite2
                             ]

otherTests : Test
otherTests = suite "Other tests" [ alternatingSignalTest2
                                 , alternatingSignalSuite2
                                 ]

view : String -> Element -> Element -> Element -> Element
view sr er er2 er3 =
  flow down [ show "The string runner:"
            , show sr
            , show "The Element runner:"
            , er
            , er2
            , er3
            ]

main : Signal Element
main =
  Signal.map4 view (StringRunner.mapDisplay alternatingSignalSuite) (ElementRunner.mapDisplay alternatingSignalTest2) (ElementRunner.mapDisplay allTests) (ElementRunner.mapDisplay otherTests)
