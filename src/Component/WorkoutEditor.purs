module Component.WorkoutEditor where

import Prelude

import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

import Model (ExerciseResults, Workout, WorkoutId)

data EditorQuery a =
  Save a

data EditorMessage =
  SaveWorkout Workout

type EditorState =
  { workout :: Workout
  }

initialState :: Workout -> EditorState
initialState workout = { workout }

editor :: forall m. Applicative m => Workout -> H.Component HH.HTML EditorQuery Unit EditorMessage m
editor workout =
  H.component
    { initialState: const $ initialState workout
    , render
    , eval
    , receiver: const Nothing
    }
  where

  render :: EditorState -> H.ComponentHTML EditorQuery
  render state = HH.div_ []

  eval :: EditorQuery ~> H.ComponentDSL EditorState EditorQuery EditorMessage m
  eval (Save next) = pure next
