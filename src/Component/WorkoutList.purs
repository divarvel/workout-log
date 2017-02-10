module Component.WorkoutList where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Data.Array (find)
import Data.Maybe (Maybe(..))


import Model (ExerciseResults, Workout, WorkoutId)
import Component.WorkoutEditor

data Query a =
    AddWorkout a
  | DisplayWorkout WorkoutId a
  | HandleEditorMessage WorkoutId EditorMessage a
data CurrentScreen =
    WorkoutList
  | EditWorkout Workout
type State =
  { currentScreen :: CurrentScreen
  , workouts :: Array Workout }

newtype EditorSlot = EditorSlot WorkoutId
derive instance eqEditorSlot :: Eq EditorSlot
derive instance ordEditorSlot :: Ord EditorSlot

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



type ListComponentHTML m = H.ParentHTML Query EditorQuery EditorSlot m
type ListComponentDSL m = H.ParentDSL State Query EditorQuery EditorSlot Void m
ui :: forall m. Applicative m => H.Component HH.HTML Query Unit Void m
ui =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  renderResult :: ExerciseResults -> ListComponentHTML m
  renderResult result = HH.p_ [
    HH.text $ result.exercise.name <> " 5x5 " <> show result.exercise.weight
  ]
  renderWorkout :: Workout -> ListComponentHTML m
  renderWorkout workout =
    HH.div
    [ HE.onClick (HE.input_ $ DisplayWorkout workout.id)]
    [
        HH.h1_ [ HH.text $ "Today, workout " <> workout.id <> " " <> show workout.currentWeight ],
        HH.div_ $ map renderResult workout.results
    ]

  renderWorkoutEditor :: Workout -> ListComponentHTML m
  renderWorkoutEditor workout =
    HH.slot
      (EditorSlot workout.id)
      (editor workout)
      unit
      (const Nothing)

  renderWorkoutList :: Array Workout -> ListComponentHTML m
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

  render :: State -> ListComponentHTML m
  render state =
    case state.currentScreen of
      WorkoutList   -> renderWorkoutList state.workouts
      EditWorkout w -> renderWorkout w
  eval :: Query ~> ListComponentDSL m
  eval (AddWorkout next) = do
      H.modify addDefaultWorkout
      pure next
  eval (DisplayWorkout idx next) = do
      H.modify $ selectWorkout idx
      pure next
  eval (HandleEditorMessage slot msg next) = do
    pure next
