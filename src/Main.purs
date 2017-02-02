module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Unsafe (unsafePerformEff)

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.Util (awaitBody, runHalogenAff)

type Exercise = {
    name :: String,
    weight :: Number
}

type ExerciseResults = {
    exercise :: Exercise,
    results :: Array Int
}

type Workout = {
    results :: Array ExerciseResults,
    currentWeight :: Number
}



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

ui :: forall g. H.Component State Query g
ui = H.component { render, eval }
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

  eval :: Query ~> H.ComponentDSL State Query g
  eval (AddWorkout next) = do
    H.modify addDefaultWorkout
    pure next

launchHalogen :: Eff (H.HalogenEffects ()) Unit
launchHalogen = runHalogenAff $ do
    body <- awaitBody
    H.runUI ui initialState body

main :: Unit
main = unsafePerformEff $ do
    launchHalogen
