module ElmTest.Run where

{-| Basic utilities for running tests and customizing the output. If you don't care about customizing
the output, instead look at the ```runDisplay``` series in ElmTest.Runner

# Run
@docs run, pass, fail

-}

import ElmTest.Assertion exposing (..)
import ElmTest.Test exposing (..)
import List

type alias PendingPayload =
  { result : Result
  , signal : Signal Result
  }

type alias ReportPayload =
  { results  : List Result
  , passes   : List Result
  , failures : List Result
  , pending  : List Result
  }

type Result = Pass String
            | Fail String String
            | Report String ReportPayload
            | Pending String (Maybe PendingPayload)

type SuiteUpdate = NoOp
                 | Update Int Result

{-| Run a test and get a Result -}
run : Test -> Result
run test =
    case test of
        TestCase name assertion -> let runAssertion t m = if t ()
                                                          then Pass name
                                                          else Fail name m
                                in case assertion of
                                     AssertEqual t a b    -> runAssertion t <| "Expected: " ++ a ++ "; got: " ++ b
                                     AssertNotEqual t a b -> runAssertion t <| a ++ " equals " ++ b
                                     AssertTrue  t        -> runAssertion t <| "not True"
                                     AssertFalse t        -> runAssertion t <| "not False"
                                     AssertSignal s       -> defer name (Pending name Nothing) <| testSignal name s
        Suite name tests -> let results = List.map run tests
                                report = suiteReport name <| List.map derefPending results
                            in if List.any pending results
                               then defer name report <| suiteSignal name results
                               else report

defer : String -> Result -> Signal Result -> Result
defer n r s =
  Pending n <| Just { result = r
                    , signal = s
                    }

derefPending : Result -> Result
derefPending r =
  case r of
    Pending _ mp -> case mp of
                      Just p -> p.result
                      Nothing -> r
    _ -> r

suiteReport : String -> List Result -> Result
suiteReport n rs =
  Report n (suiteReportPayload rs)

suiteReportPayload : List Result -> ReportPayload
suiteReportPayload results =
  let (pending', completed) = List.partition pending results
      (passes, fails) = List.partition pass completed
  in  { results  = results
      , passes   = passes
      , failures = fails
      , pending  = pending'
      }

{-| Creates a signal that maps a conditional bool signal to a pass/fail/pending result -}
testSignal : String -> Signal (Maybe Bool) -> Signal Result
testSignal n s =
  let c = \m -> case m of
              Just b -> if b
                        then Pass n
                        else Fail n "Signal failed"
              Nothing -> Pending n Nothing
  in  Signal.map c s

suiteSignal : String -> List Result -> Signal Result
suiteSignal n rs =
  let
      -- Maps test update signal to indexed suite update signal
      isus = \i s -> Signal.map (\r -> Update i r) s

      -- Maybe map a result to indexed suite update signal
      misus = \i r -> case r of
                        Pending _ mp -> case mp of
                                          Just p -> Just (isus i p.signal)
                                          Nothing -> Nothing
                        _ -> Nothing

      -- Filter results into suite update signals
      ss = List.filterMap identity <| List.indexedMap misus rs

      -- Merge signals into giant suite update list (so they don't get eaten by regular merge)
      fl = \s s' -> Signal.map2 (::) s s'
      ss' = List.foldl fl (Signal.constant [NoOp]) ss

      -- Turn results into list of (index, dereffed result) tuples as state
      im = List.indexedMap (,) <| List.map derefPending rs

      -- Creates a map fn that will swap out the result tuple at the given index
      up = \i r -> \t -> if fst t == i then (i, r) else t

      -- Applies a suite update to the results
      fup = \su rs -> case su of
                        Update i r -> List.map (up i r) rs
                        NoOp -> rs -- TODO: Report rterr when this case is missing

      -- Updates state
      fp = \sus rs -> List.foldl fup rs sus

      -- Track state over suite updates
      s = Signal.foldp fp im ss'

      -- Extract raw results from tuple index map
      s' = Signal.map (\im -> List.map snd im) s

  -- Map suite reports
  in Signal.map (\rs -> suiteReport n rs) s'

{-| Transform a Result into a Bool. True if the result represents a pending test, otherwise False -}
pending : Result -> Bool
pending m = case m of
              Pending _ _ -> True
              Report _ r  -> if (List.length (.pending r) > 0) then True else False
              _ -> False

{-| Transform a Result into a Bool. True if the result represents a pass, otherwise False -}
pass : Result -> Bool
pass m = case m of
           Pass _      -> True
           Fail _ _    -> False
           Report _ r  -> if pending m
                          then False
                          else if (List.length (.failures r) > 0) then False else True
           Pending _ _ -> False

{-| Transform a Result into a Bool. True if the result represents a fail, otherwise False -}
fail : Result -> Bool
fail m = case m of
           Pending _ _ -> False
           Report _ r  -> if pending m
                          then False
                          else not <| pass m
           _ -> not <| pass m

passedTests : Result -> Int
passedTests result = case result of
                        Pass _       -> 1
                        Fail _ _     -> 0
                        Report n r   -> List.sum << List.map passedTests <| r.results
                        Pending _ mp -> case mp of
                                            Just p  -> passedTests p.result
                                            Nothing -> 0

failedTests : Result -> Int
failedTests result = case result of
                        Pass _       -> 0
                        Fail _ _     -> 1
                        Report n r   -> List.sum << List.map failedTests <| r.results
                        Pending _ mp -> case mp of
                                          Just p  -> failedTests p.result
                                          Nothing -> 0

pendingTests : Result -> Int
pendingTests result = case result of
                        Report n r   -> List.sum << List.map pendingTests <| r.results
                        Pending _ mp -> case mp of
                                          Just p  -> pendingTests p.result
                                          Nothing -> 1
                        _ -> 0

passedSuites : Result -> Int
passedSuites result = case result of
                        Report n r   -> let passed = if pass result
                                                     then 1
                                                     else 0
                                        in  passed + (List.sum << List.map passedSuites <| r.results)
                        Pending _ mp -> case mp of
                                          Just p  -> passedSuites p.result
                                          Nothing -> 0
                        _ -> 0

failedSuites : Result -> Int
failedSuites result = case result of
                        Report n r   -> let failed = if fail result
                                                     then 1
                                                     else 0
                                        in  failed + (List.sum << List.map failedSuites <| r.results)
                        Pending _ mp -> case mp of
                                          Just p  -> failedSuites p.result
                                          Nothing -> 0
                        _ -> 0

pendingSuites : Result -> Int
pendingSuites result = case result of
                        Report n r   -> let pending' = if pending result
                                                       then 1
                                                       else 0
                                        in  pending' + (List.sum << List.map pendingSuites <| r.results)
                        Pending _ mp -> case mp of
                                          Just p  -> pendingSuites p.result
                                          Nothing -> 0
                        _ -> 0
