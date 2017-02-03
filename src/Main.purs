module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Model (ExerciseResults, Workout)

data Query a = AddWorkout a

type State = { workouts :: Array Workout }

defaultWorkout :: Workout
defaultWorkout = { results: [
    {
        exercise: { name: "SQ", weight: 22.5 },
        results: [5, 5, 5, 5, 5]
    }
  ], currentWeight: 54.4 }

initialState :: State
initialState = { workouts: [ defaultWorkout, defaultWorkout, defaultWorkout ] }

addDefaultWorkout :: State -> State
addDefaultWorkout state = state { workouts = state.workouts <> [defaultWorkout] }

ui :: forall m. H.Component HH.HTML Query Unit Void m
ui =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  renderResult :: ExerciseResults -> H.ComponentHTML Query
  renderResult result = HH.p_ [
    HH.text $ result.exercise.name <> " 5x5 " <> show result.exercise.weight
  ]
  renderWorkout :: Workout -> H.ComponentHTML Query
  renderWorkout workout =
    HH.div_ [
        HH.h1_ [ HH.text $ "Today " <> show workout.currentWeight ],
        HH.div_ $ map renderResult workout.results
    ]
  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
      [
        HH.h1_
          [ HH.text "Workout log" ]
      , HH.div_
          (map renderWorkout state.workouts)
      , HH.button
      [ HE.onClick (HE.input_ AddWorkout)]
          [ HH.text "+" ]
      ]
  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    AddWorkout next -> do
      H.modify addDefaultWorkout
      pure next

launchHalogen :: Eff (HA.HalogenEffects ()) Unit
launchHalogen = HA.runHalogenAff $ do
    body <- HA.awaitBody
    runUI ui unit body

main :: Unit
main = unsafePerformEff $ do
    launchHalogen
