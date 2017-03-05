module Component.WorkoutEditor where

import Prelude

import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import DOM.HTML.Indexed.InputType (InputType(..))

import Model (ExerciseResults, Workout, WorkoutId)

data EditorQuery a
  = UpdateBodyWeight String a
  | UpdateResultCounter Int Int a
  | Save a

data EditorMessage =
  SaveWorkout Workout

type EditorState =
  { workout :: Workout
  }

initialState :: Workout -> EditorState
initialState workout = { workout }

renderExerciseCounter :: Int -> Int -> Int -> H.ComponentHTML EditorQuery
renderExerciseCounter exerciseIndex counterIndex value =
  HH.button
    [ HP.title "Count reps"
    , HE.onClick $ HE.input_ (UpdateResultCounter exerciseIndex counterIndex)
    ]
    [ HH.text $ if value > 0 then show value else ""]

renderExerciseBlock :: ExerciseResults -> H.ComponentHTML EditorQuery
renderExerciseBlock results =
  HH.div_
    [ HH.p_ [ HH.text results.exercise.name ]
    , HH.p_ [ HH.text $ "5x5" <> (show results.exercise.weight) <> " kg" ]
    , HH.div_ $ map (renderExerciseCounter 0 0) results.results
    ]

renderBodyWeightBlock :: Number -> H.ComponentHTML EditorQuery
renderBodyWeightBlock weight =
  HH.div_
    [ HH.p_
        [ HH.text "Body weight"
        ]
    , HH.input
        [ HP.type_ HP.InputNumber
        , HP.value $ show weight
        , HE.onValueChange (HE.input UpdateBodyWeight)
      ]
    ]

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
  render state = HH.div_ $
    map renderExerciseBlock state.workout.results


  eval :: EditorQuery ~> H.ComponentDSL EditorState EditorQuery EditorMessage m
  eval (Save next) = pure next
  eval (UpdateBodyWeight value next) = pure next -- ToDo
  eval (UpdateResultCounter exerciseIndex counterIndex next) = pure next -- ToDo
