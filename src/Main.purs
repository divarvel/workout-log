module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.Array (find)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Model (ExerciseResults, Workout, WorkoutId)

data Query a =
    AddWorkout a
  | DisplayWorkout WorkoutId a
data CurrentScreen =
    WorkoutList
  | EditWorkout Workout
type State =
  { currentScreen :: CurrentScreen
  , workouts :: Array Workout }

defaultWorkout :: WorkoutId -> Workout
defaultWorkout =
  { id: _
  , results:
    [
      { exercise: { name: "SQ", weight: 22.5 }
      , results: [5, 5, 5, 5, 5]
      }
    ]
  , currentWeight: 54.4
  }

initialState :: State
initialState =
  { currentScreen: WorkoutList
  , workouts: map defaultWorkout ["1", "2", "3"] }

addDefaultWorkout :: State -> State
addDefaultWorkout state = state { workouts = state.workouts <> [defaultWorkout "4"] }

selectWorkout :: WorkoutId -> State -> State
selectWorkout id state =
  case find (\s -> s.id == id) state.workouts of
    Just w -> state { currentScreen = EditWorkout w}
    _ -> state

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
    HH.div
    [ HE.onClick (HE.input_ $ DisplayWorkout workout.id)]
    [
        HH.h1_ [ HH.text $ "Today, workout " <> workout.id <> " " <> show workout.currentWeight ],
        HH.div_ $ map renderResult workout.results
    ]
  renderWorkoutList :: Array Workout -> H.ComponentHTML Query
  renderWorkoutList workouts =
    HH.div_
      [
        HH.h1_
          [ HH.text "Workout log" ]
      , HH.div_
          (map renderWorkout workouts)
      , HH.button
      [ HE.onClick (HE.input_ AddWorkout)]
          [ HH.text "+" ]
      ]

  render :: State -> H.ComponentHTML Query
  render state =
    case state.currentScreen of
      WorkoutList   -> renderWorkoutList state.workouts
      EditWorkout w -> renderWorkout w
  eval :: Query ~> H.ComponentDSL State Query Void m
  eval (AddWorkout next) = do
      H.modify addDefaultWorkout
      pure next
  eval (DisplayWorkout idx next) = do
      H.modify $ selectWorkout idx
      pure next

launchHalogen :: Eff (HA.HalogenEffects ()) Unit
launchHalogen = HA.runHalogenAff $ do
    body <- HA.awaitBody
    runUI ui unit body

main :: Unit
main = unsafePerformEff $ do
    launchHalogen
