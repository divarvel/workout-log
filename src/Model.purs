module Model where

type Exercise = {
    name :: String,
    weight :: Number
}

type ExerciseResults = {
    exercise :: Exercise,
    results :: Array Int
}

type WorkoutId = String
type Workout = {
    id :: WorkoutId,
    results :: Array ExerciseResults,
    currentWeight :: Number
}
